package com.jtfmumm.algprops

import org.scalacheck._
import org.scalacheck.Prop
import org.scalacheck.Arbitrary._
import org.scalacheck.Test._
import com.jtfmumm.algstructs._

object PropertyChecker {
  def standardEquality[A](a: A, b: A) = a == b

  val tests600 = new Parameters.Default {
    override val minSuccessfulTests = 600
  }

  def listOfN[A](n: Int, gen: Gen[A]) = Gen.listOfN(n, gen).suchThat(_.size == n)

  def isMagma[A](m: Magma[A], gen: Gen[A]): Boolean = {
    val res = Test.check(tests600, AlgProperties.definedForAllElementsOn[A](m, listOfN[A](2, gen)))
    res.passed
  }

  def isSemiGroup[A](s: SemiGroup[A], gen: Gen[A]): Boolean = {
    val res = Test.check(tests600, AlgProperties.associativityOn[A](s, listOfN[A](3, gen)))
    isMagma(s, gen) && res.passed
  }

  def isMonoid[A](m: Monoid[A], gen: Gen[A]): Boolean = {
    val res = Test.check(tests600, AlgProperties.identityOn[A](m, gen))
    isSemiGroup(m, gen) && res.passed
  }

  def isGroup[A](g: Group[A], gen: Gen[A]): Boolean = {
    val res = Test.check(tests600, AlgProperties.inverseOn[A](g, gen))
    isMonoid(g, gen) && res.passed
  }

  def isAbelianGroup[A](a: AbelianGroup[A], gen: Gen[A]): Boolean = {
    val res = Test.check(tests600, AlgProperties.commutativityOn[A](a, listOfN[A](2, gen)))
    isGroup(a, gen) && res.passed
  }

  def isFiniteGroup[A](fg: FiniteGroup[A]): Boolean = {
    def gen = for {
      index <- Gen.choose(0, fg.set.size - 1)
    } yield fg.set.toList(index)
    isGroup(fg, gen)
  }


  def findStructure[A](as: AbelianGroup[A], gen: Gen[A]): String = {
    val props = checkAsAbelianGroup[A](as, gen)
    StructureByProperties.lookup(props) + "\n" + props
  }

  def findStructure[A](as: Monoid[A], gen: Gen[A]): String = {
    val props = checkAsMonoid[A](as, gen)
    StructureByProperties.lookup(props) + "\n" + props
  }

  def findStructure[A](as: SemiGroup[A], gen: Gen[A]): String = {
    val props = checkAsSemiGroup[A](as, gen)
    StructureByProperties.lookup(props) + "\n" + props
  }

//  def findStructure[A](as: Ring[A], gen: Gen[A]): String = {
//    val props = checkAsRing[A](as, gen)
//    StructureByProperties.lookup(props) + "\n" + props
//  }
//
//  def findStructure[A](as: IntegralDomain[A], gen: Gen[A]): String = {
//    val props = checkAsIntegralDomain[A](as, gen)
//    StructureByProperties.lookup(props) + "\n" + props
//  }
//
//  def findStructure[A](as: Field[A], gen: Gen[A]): String = {
//    val props = checkAsField[A](as, gen)
//    StructureByProperties.lookup(props) + "\n" + props
//  }

  def checkAsAbelianGroup[A](as: AbelianGroup[A], gen: Gen[A]): Map[String, Boolean] = {
    Map("defined" -> Test.check(tests600, AlgProperties.definedForAllElementsOn[A](as, listOfN[A](2, gen))).passed,
        "associative" -> Test.check(tests600, AlgProperties.associativityOn[A](as, listOfN[A](3, gen))).passed,
        "commutative" -> Test.check(tests600, AlgProperties.commutativityOn[A](as, listOfN[A](2, gen))).passed,
        "identity element" -> Test.check(tests600, AlgProperties.identityOn[A](as, gen)).passed,
        "inverse element" -> Test.check(tests600, AlgProperties.inverseOn[A](as, gen)).passed
    )
  }

  def checkAsMonoid[A](as: Monoid[A], gen: Gen[A]): Map[String, Boolean] = {
    Map("defined" -> Test.check(tests600, AlgProperties.definedForAllElementsOn[A](as, listOfN[A](2, gen))).passed,
      "associative" -> Test.check(tests600, AlgProperties.associativityOn[A](as, listOfN[A](3, gen))).passed,
      "commutative" -> false,
      "identity element" -> Test.check(tests600, AlgProperties.identityOn[A](as, gen)).passed,
      "inverse element" -> false
    )
  }

  def checkAsSemiGroup[A](as: SemiGroup[A], gen: Gen[A]): Map[String, Boolean] = {
    Map("defined" -> Test.check(tests600, AlgProperties.definedForAllElementsOn[A](as, listOfN[A](2, gen))).passed,
      "associative" -> Test.check(tests600, AlgProperties.associativityOn[A](as, listOfN[A](3, gen))).passed,
      "commutative" -> false,
      "identity element" -> false,
      "inverse element" -> false
    )
  }
}
