package akka.cluster.ddata

import java.util.UUID

import scala.concurrent.duration._
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import akka.cluster.Cluster

import scala.collection.immutable.{ HashSet, TreeSet }

object ShoppingORCart {
  import akka.cluster.ddata.Replicator._

  def props(userId: String): Props = Props(new ShoppingORCart(userId))

  case object GetCart
  final case class ChangeItemQuantity(item: LineItem)
  final case class RemoveItem(productId: String)

  final case class Cart(items: Set[LineItem])
  final case class LineItem(productId: String, title: String, quantity: Int)
  final case class SingleLineItem(productId: String, title: String, uid: String)

  //#read-write-majority
  private val timeout = 3.seconds
  private val readMajority = ReadMajority(timeout)
  private val writeMajority = WriteMajority(timeout)
  //#read-write-majority

  def getUniqueId(implicit cluster: Cluster): String = {
    cluster.selfUniqueAddress.toString + UUID.randomUUID().toString
  }

}

class ShoppingORCart(userId: String) extends Actor {
  import ShoppingORCart._
  import akka.cluster.ddata.Replicator._

  val replicator = DistributedData(context.system).replicator
  implicit val cluster = Cluster(context.system)

  val DataKey = ORMultiMapKey[String, SingleLineItem]("cart-" + userId)

  def receive = receiveGetCart
    .orElse[Any, Unit](receiveAddItem)
    .orElse[Any, Unit](receiveRemoveItem)
    .orElse[Any, Unit](receiveOther)

  //#get-cart
  def receiveGetCart: Receive = {
    case GetCart ⇒
      replicator ! Get(DataKey, readMajority, Some(sender()))

    case g @ GetSuccess(DataKey, Some(replyTo: ActorRef)) ⇒
      val data = g.get(DataKey)
      val entrySet = data.entries.groupBy(_._1).map {
        entry ⇒ LineItem(entry._2.values.head.head.productId, entry._2.values.head.head.title, entry._2.values.head.size)
      }.toSet
      val cart = Cart(entrySet)
      replyTo ! cart

    case NotFound(DataKey, Some(replyTo: ActorRef)) ⇒
      replyTo ! Cart(Set.empty)

    case GetFailure(DataKey, Some(replyTo: ActorRef)) ⇒
      // ReadMajority failure, try again with local read
      replicator ! Get(DataKey, ReadLocal, Some(replyTo))
  }
  //#get-cart

  //#add-item
  def receiveAddItem: Receive = {
    case cmd @ ChangeItemQuantity(item) ⇒
      val update = Update(DataKey, ORMultiMap.empty[String, SingleLineItem], writeMajority, Some(cmd)) {
        cart ⇒ updateCart(cart, item)
      }
      replicator ! update
  }
  //#add-item

  def updateCart(data: ORMultiMap[String, SingleLineItem], item: LineItem): ORMultiMap[String, SingleLineItem] =
    data.get(item.productId) match {
      case Some(entries) ⇒
        val existingQuantity = entries.size
        if (existingQuantity == item.quantity) {
          data
        } else if (existingQuantity < item.quantity) {
          println("Existing: " + existingQuantity + " target " + item.quantity)
          var newEntries = HashSet[SingleLineItem]()
          (1 to (item.quantity - existingQuantity)).foreach { _ ⇒
            newEntries = newEntries + SingleLineItem(item.productId, item.title, getUniqueId)
          }
          val ops = newEntries.foldLeft(data) { case (d, item) ⇒ d.addBinding(item.productId, item) }
          ops
        } else {
          val existingItems = entries.toVector
          val ops = (1 to (existingQuantity - item.quantity)).foldLeft(data) {
            case (d, index) ⇒
              d.removeBinding(item.productId, existingItems(index - 1))
          }
          ops
        }
      case None ⇒
        var items: Set[SingleLineItem] = new HashSet[SingleLineItem]()
        (1 to item.quantity).foreach { _ ⇒
          items = items + SingleLineItem(item.productId, item.title, getUniqueId)
        }
        data + (item.productId → items)
    }

  //#remove-item
  def receiveRemoveItem: Receive = {
    case cmd @ RemoveItem(productId) ⇒
      // Try to fetch latest from a majority of nodes first, since ORMap
      // remove must have seen the item to be able to remove it.
      // Actually this is just working around possible anomalies :)
      replicator ! Get(DataKey, readMajority, Some(cmd))

    case GetSuccess(DataKey, Some(RemoveItem(productId))) ⇒
      replicator ! Update(DataKey, ORMultiMap(), writeMajority, None) {
        _ - productId // be careful, possible anomaly similar to the one with LWWMap, use ORMultiMap.emptyWithValueDeltas for safety
      }

    case GetFailure(DataKey, Some(RemoveItem(productId))) ⇒
      // ReadMajority failed, fall back to best effort local value
      replicator ! Update(DataKey, ORMultiMap(), writeMajority, None) {
        _ - productId
      }

    case NotFound(DataKey, Some(RemoveItem(productId))) ⇒
    // nothing to remove
  }
  //#remove-item

  def receiveOther: Receive = {
    case _: UpdateSuccess[_] | _: UpdateTimeout[_] ⇒
    // UpdateTimeout, will eventually be replicated
    case e: UpdateFailure[_]                       ⇒ throw new IllegalStateException("Unexpected failure: " + e)
  }

}
