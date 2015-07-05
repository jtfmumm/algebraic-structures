package main.scala
import algstructs._
import algprops._
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Test._

object AS {
  def main(args: Array[String]) = {
//    val testParams = new Parameters.Default {
//      override val minSuccessfulTests = 600
//    }

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

    val int1 = Gen.choose(0, 100)
    val int2List = Gen.listOfN(2, Gen.choose(0, 100)).suchThat(_.size == 2)
    val int3List = Gen.listOfN(3, Gen.choose(0, 100)).suchThat(_.size == 3)
    val string1 = arbitrary[String]
    val string2List = Gen.listOfN(2, arbitrary[String]).suchThat(_.size == 2)
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
    println("IntAdd: id")
    println(AlgProperties.identityOn[Int](IntAdd(), int1).check)
    println("IntAdd: inv")
    println(AlgProperties.inverseOn[Int](IntAdd(), int1).check)
    println("StringConcat: id")
    println(AlgProperties.identityOn[String](StringConcat(), string1).check)
  }
}
