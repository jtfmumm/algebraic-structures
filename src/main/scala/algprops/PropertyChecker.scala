package main.scala.algprops

import org.scalacheck._
import org.scalacheck.Prop
import org.scalacheck.Arbitrary._
import org.scalacheck.Test._
import main.scala.algstructs._

object PropertyChecker {
  def standardEquality[A](a: A, b: A) = a == b

  val tests600 = new Parameters.Default {
    override val minSuccessfulTests = 600
  }

  def listOfN[A](n: Int, gen: Gen[A]) = Gen.listOfN(n, gen).suchThat(_.size == n)

  def isMagma[A](m: Magma[A], gen: Gen[A], is: (A, A) => Boolean = standardEquality[A] _): Boolean = {
    val res = Test.check(tests600, AlgProperties.definedForAllElementsOn[A](m, listOfN[A](2, gen), is))
    res.passed
  }

  def isSemiGroup[A](s: SemiGroup[A], gen: Gen[A], is: (A, A) => Boolean = standardEquality[A] _): Boolean = {
    val res = Test.check(tests600, AlgProperties.associativityOn[A](s, listOfN[A](3, gen), is))
    isMagma(s, gen, is) && res.passed
  }

  def isMonoid[A](m: Monoid[A], gen: Gen[A], is: (A, A) => Boolean = standardEquality[A] _): Boolean = {
    val res = Test.check(tests600, AlgProperties.identityOn[A](m, gen, is))
    isSemiGroup(m, gen, is) && res.passed
  }

  def isGroup[A](g: Group[A], gen: Gen[A], is: (A, A) => Boolean = standardEquality[A] _): Boolean = {
    val res = Test.check(tests600, AlgProperties.inverseOn[A](g, gen, is))
    isMonoid(g, gen, is) && res.passed
  }

  def isAbelianGroup[A](a: AbelianGroup[A], gen: Gen[A], is: (A, A) => Boolean = standardEquality[A] _): Boolean = {
    val res = Test.check(tests600, AlgProperties.commutativityOn[A](a, listOfN[A](2, gen), is))
    isGroup(a, gen, is) && res.passed
  }
}
