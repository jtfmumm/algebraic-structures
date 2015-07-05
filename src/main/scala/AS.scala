package main.scala
import algstructs._
import algprops._
import org.scalacheck._
import org.scalacheck.Arbitrary._

object AS {
  def main(args: Array[String]) = {
    case class IntAdd extends AbelianGroup[Int] {
      val e = 0
      def inv(a: Int): Int = 0 - a

      def op(a: Int, b: Int): Int = a + b
    }

    case class IntDivide extends AbelianGroup[Int] {
      val e = 1
      def inv(a: Int): Int = a

      def op(a: Int, b: Int): Int = a / b
    }

    case class StringConcat extends Monoid[String] {
      val e = ""
      def op(a: String, b: String): String = a + b
    }

    val i = new IntAdd
    println(i.op(1, 2))
    val int2List = Gen.listOfN(2, Gen.choose(0, 100)).suchThat(_.size == 2)
    val int3List = Gen.listOfN(3, Gen.choose(0, 100)).suchThat(_.size == 3)
    val string2List = Gen.listOfN(3, arbitrary[String]).suchThat(_.size == 2)
    val string3List = Gen.listOfN(3, arbitrary[String]).suchThat(_.size == 3)
    println("IntAdd: defined")
    println(AlgProperties.definedForAllElementsOn[Int](IntAdd(), int2List).check)
    println("IntDivide: defined")
    println(AlgProperties.definedForAllElementsOn[Int](IntDivide(), int2List).check)
    println("StringConcat: defined")
    println(AlgProperties.definedForAllElementsOn[String](StringConcat(), string2List).check)
    println("StringConcat: assoc")
    println(AlgProperties.associativityOn[String](StringConcat(), string3List).check)
    println("IntAdd: assoc")
    println(AlgProperties.associativityOn[Int](IntAdd(), int3List).check)
    println("StringConcat: comm")
    println(AlgProperties.commutativityOn[String](StringConcat(), string2List).check)
    println("IntAdd: comm")
    println(AlgProperties.commutativityOn[Int](IntAdd(), int3List).check)
  }

}
