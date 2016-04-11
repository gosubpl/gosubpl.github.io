package pl.p.lodz.bootstrap

class Zajecia04 {

  def show_args(args: Array[String]): Unit = {
    var i = 0
    while (i < args.length) {
      if (i != 0)
        print(" ")
      print(args(i))
      i += 1
    }
    println()
  }

  def print_arg(arg: String): Unit = {
    println(arg)
  }

  def print_args(args: Array[String]): Unit = {
    args.foreach(print_arg)
  }

}

object Zajecia04 extends App {
  override def main(args: Array[String]): Unit = {
    val z04 = new Zajecia04()
    z04.show_args(args)

    z04.show_args(Array("a", "b", "c"))

    z04.print_args(Array("a", "b"))

    args.foreach(arg => println(arg))

    args.foreach(println(_))

    args.foreach(println _)

    args.foreach(println)

    args foreach println

    for (arg <- args)
      println(arg)

    for (i <- 1 to 3)
      println(i)

    for (i <- 1.to(3))
      println(i)
  }
}

