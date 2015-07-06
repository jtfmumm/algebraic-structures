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
}
