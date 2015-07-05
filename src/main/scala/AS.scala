package main.scala
import algstructs._

object AS {
  def main(args: Array[String]) = {
    case class IntAdd extends AbelianGroup[Int] {
      val e = 0
      def inv(a: Int): Int = 0 - a

      def op(a: Int, b: Int): Int = a + b
    }

    case class StringConcat extends Monoid[String] {
      val e = ""
      def op(a: String, b: String): String = a + b
    }

    val i = new IntAdd
    println(i.op(1, 2))
  }

}
