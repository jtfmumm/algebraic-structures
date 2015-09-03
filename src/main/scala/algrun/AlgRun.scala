package com.jtfmumm.algrun

import com.jtfmumm.sets._
import com.jtfmumm.sets.ui._
import com.jtfmumm.algprops._
import com.jtfmumm.algstructs._
import org.scalacheck.Prop._
import org.scalacheck._
import org.scalacheck.Arbitrary._

object AlgRun {
  def main(args: Array[String]) = {
    case class IntAdd extends AbelianGroup[Int] {
      val e = 0
      def inv(a: Int): Int = 0 - a

      def op(a: Int, b: Int): Int = a + b
    }

    case class IntDivide extends AbelianGroup[Int] {
      val e = 1
      def inv(a: Int): Int = 1 / a

      def op(a: Int, b: Int): Int = a / b
    }

    case class StringConcat extends Monoid[String] {
      val e = ""
      def op(a: String, b: String): String = a + b
    }

    case class SetUnion extends Monoid[PureSet] {
      val e = PureSet()
      def op(a: PureSet, b: PureSet): PureSet = a.unionWith(b)
      override def is(a: PureSet, b: PureSet): Boolean = a.is(b)
    }

    case class SetIntersection extends SemiGroup[PureSet] {
      //val e = [the universe] <-- but this isn't allowed
      def op(a: PureSet, b: PureSet): PureSet = a.intersectionWith(b)
      override def is(a: PureSet, b: PureSet): Boolean = a.is(b)
    }

    case class SetDifference extends Group[PureSet] {
      //This is not really an identity element, since it only
      //holds for a / e = a, not e / a = a.
      val e = PureSet()
      //If e were really the identity element, this would be
      //the inverse function.
      def inv(a: PureSet): PureSet = a

      def op(a: PureSet, b: PureSet): PureSet = a.relativeComplementIn(b)
      override def is(a: PureSet, b: PureSet): Boolean = a.is(b)
    }

    val intGen = Gen.choose(0, 100)
    val int2List = Gen.listOfN(2, Gen.choose(0, 100)).suchThat(_.size == 2)
    val int3List = Gen.listOfN(3, Gen.choose(0, 100)).suchThat(_.size == 3)
    val stringGen = arbitrary[String]
    val string2List = Gen.listOfN(2, arbitrary[String]).suchThat(_.size == 2)
    val string3List = Gen.listOfN(3, arbitrary[String]).suchThat(_.size == 3)

    val setGen: Gen[PureSet] = for {
      name <- Gen.choose(0, 10)
    } yield SetUI.parseInt(name.toString)

    def setEquality(a: PureSet, b: PureSet): Boolean = a is b


    println("IntAdd: AbelianGroup?") // true
    println(PropertyChecker.isAbelianGroup[Int](IntAdd(), intGen))
    println("IntDivide: AbelianGroup?") // false
    println(PropertyChecker.isAbelianGroup[Int](IntDivide(), intGen))
    println("IntDivide: Algebraic Structure?") // false
    println(PropertyChecker.isAlgStruct[Int](IntDivide(), intGen))
    println("StringConcat: Monoid?") // true
    println(PropertyChecker.isMonoid[String](StringConcat(), stringGen))
    println("---STRUCTURES---")
    println("--IntAdd is") // Abelian Group
    println(PropertyChecker.findStructure[Int](IntAdd(), intGen))
    println("--IntDivide is") // No Algebraic Structure
    println(PropertyChecker.findStructure[Int](IntDivide(), intGen))
    println("--StringConcat is") // Monoid
    println(PropertyChecker.findStructure[String](StringConcat(), stringGen))
    println("---SETS---")
    println("SetUnion: Monoid?") // true
    println(PropertyChecker.isMonoid[PureSet](SetUnion(), setGen))
    println("SetIntersection: SemiGroup?") // true
    println(PropertyChecker.isSemiGroup[PureSet](SetIntersection(), setGen))
    println("SetDifference: Algebraic Structure?") // true
    println(PropertyChecker.isAlgStruct[PureSet](SetDifference(), setGen))
    println("SetDifference: Group?") // false
    println(PropertyChecker.isGroup[PureSet](SetDifference(), setGen))


    //FINITE STRUCTURES
    println(MultMod5Without0()) // this succeeds
//    println(MultMod4Without0()) // this fails!
  }
}
