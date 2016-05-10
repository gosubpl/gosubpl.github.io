package pl.p.lodz.bootstrap

case class TwoInts(a: Int, b: Int) {}
case class TwoStrings(a: String, b: String) {}

class Zajecia07 {
  def matchAnyValue(x: Any): String = {
    x match {
      case 5 => "5"
      case true => "truth"
      case "hello" => "hello"
      case _ => "something else"
    }
  }

  def matchAnyType(x: Any): String = {
    x match {
      case n: Number => "a number" // but Number(_) won't work!
      case s: String => "string" // but String(_) won't work!
      case somethingElse => "something else"
    }
  }

  def matchConstructors(x: Any): String = {
    x match { // first match wins - order matters
      case TwoInts(5, 3) => "two ints - 5, 3"
      case TwoInts(5, _) => "two ints - 5 and something"
      case TwoInts(_, 3) => "two ints - something and three"
      case TwoStrings("5", _) => "two strings - 5 and something"
      case TwoStrings("5", "3") => "two strings - 5, 3"
      case _ => "something else"
    }
  }

  def extractSecondString(x: TwoStrings): String = {
    x match {
      case TwoStrings(_, x @ "3") => x
      case TwoStrings(_, x @ _) => "x other than 3: " + x
      case _ => "something else"
    }
  }

  def twoIntsEqual(x: TwoInts): String = {
    x match {
      case TwoInts(x, y) if x == y => "equal"
      case _ => "not equal"
    }
  }
}

object Zajecia07 extends App {
  override def main(args: Array[String]): Unit = {
    val z07 = new Zajecia07()

    val mav5 = z07.matchAnyValue(5)
    println("match: " + mav5)

    val mav6 = z07.matchAnyValue(6)
    println("match: " + mav6)

    val mat5 = z07.matchAnyType(5)
    println("type: " + mat5)

    val matAAA = z07.matchAnyType("AAA")
    println("type: " + matAAA)

    val mat53 = z07.matchConstructors(TwoInts(5, 3))
    println("two: " + mat53)

    val mat54 = z07.matchConstructors(TwoInts(5, 4))
    println("two: " + mat54)

    val mats53 = z07.matchConstructors(TwoStrings("5", "3"))
    println("two: " + mats53)

    val maes53 = z07.extractSecondString(TwoStrings("5", "3"))
    println("extract: " + maes53)

    val maes54 = z07.extractSecondString(TwoStrings("5", "4"))
    println("extract: " + maes54)

    val mae77 = z07.twoIntsEqual(TwoInts(7, 7))
    println("equal: " + mae77)

    val mae75 = z07.twoIntsEqual(TwoInts(7, 5))
    println("equal: " + mae75)
  }
}

