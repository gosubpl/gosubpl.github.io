package pl.p.lodz.bootstrap

abstract class BaseClass {
  var x: Int = 7
  def getX: Int
  def setX(xv: Int) = { x = xv }
}

class DerivClass extends BaseClass {
  def getX = x
  override def setX(xv: Int) = { x = xv }
}

abstract class BaseClassTwo(xv: Int) {
  var x: Int = xv
}

class DerivClassTwo(xv: Int) extends BaseClassTwo(xv) {
}

trait BaseTrait {
  var x: Int
  def getX: Int
  def setX(xv: Int) = {x = xv}
}

trait OtherTrait extends BaseTrait {
  def getY = x
  def setY(yv: Int)
}

class DerivClassTrait(xv: Int) extends OtherTrait {
  var x: Int = xv
  def getX = x
  def setY(yv: Int) = {x = yv}
}

class Zajecia09 {
}

object Zajecia09 extends App {
  override def main(args: Array[String]): Unit = {
    println("hello traits!")
    val z09 = new Zajecia09()
    val dct = new DerivClassTrait(7)
    println(dct.getY)
  }
}

