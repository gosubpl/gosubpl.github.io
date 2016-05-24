package pl.p.lodz.bootstrap

class Zajecia08 {
  def traverseList[T](x: List[T]): Unit = {
    x match {
      case Nil =>
      case a :: atail => { println(a); traverseList(atail) }
    }
  }

  // the ::: operator
  // val xys = xs ::: ys
  def append[T](xs: List[T], ys: List[T]): List[T] =
    xs match {
      case List() => ys
      case x :: xs1 => xs // fix this
    }

  // write length, reverse

  // "extreme" pattern matching over lists
  def msort[T](less: (T, T) => Boolean)(xs: List[T]): List[T] = {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (less(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (ys, zs) = xs splitAt n
      merge(msort(less)(ys), msort(less)(zs))
    }
  }

  // using a fold
  def sum(xs: List[Int]): Int = (0 /: xs) (_ + _)
}

object Zajecia08 extends App {
  override def main(args: Array[String]): Unit = {
    println("hello lists!")
    val ls: List[String] = List("apples", "oranges", "tomatoes")
    val li: List[Int] = List(1, 2, 3)
    val la: List[Any] = List(1, "apples")
    // lists are covariant
    ls foreach println
    val ls2 = "apples" :: "oranges" :: Nil
    ls2 map println
    println(ls.head)
    println(li.headOption)
    println(ls.tail)
    val z08 = new Zajecia08()
    z08.traverseList(ls2)
    val ls12 = z08.append(ls, ls2)
    z08.traverseList(ls12)
    // try drop take splitAt
    // try zip unzip
    println(ls zip li)
    println(List((1, "a"), (2, "b")).unzip)
    // try toString mkString toArray
    println(ls.mkString("|"))
    // try foldLeft foldRight
    println(z08.sum(li))
    // write reverse using one of the folds
    // try map flatMap foreach
    // try filter partition find takeWhile dropWhile
    println(li filter (_ < 2))
  }
}

