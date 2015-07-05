package main.scala.algprops
import org.scalacheck.Prop._
import org.scalacheck._
import main.scala.algstructs._

class AlgProperties {
  // closure and injection are assured by the fact that in this implementation op is always a function of the form (A, A) => A
  def definedForAllElementsOn[A](as: Magma[A], genFor: Gen[(A, A)]) = forAll(genFor)(t => {
    val x = t._1
    val y = t._2
    try {
      as.op(x, y)
      true
    }
    catch {
      case _: Throwable => false
    }
  })
  def associativityOn[A](as: Magma[A], genFor: Gen[(A, A, A)]) = forAll(genFor)(t => {
    val x = t._1
    val y = t._2
    val z = t._3
    as.op(as.op(x, y), z) == as.op(x, as.op(y, z))
  })
  def commutativityOn[A](as: Magma[A], genFor: Gen[(A, A)]) = forAll(genFor)(t => {
    val x = t._1
    val y = t._2
    as.op(x, y) == as.op(y, x)
  })
  def identityOn[A](as: Monoid[A], genFor: Gen[A]) = forAll(genFor)(x => {
    as.op(x, as.e) == x
  })
  def inverseOn[A](as: Group[A], genFor: Gen[A]) = forAll(genFor)(x => {
    as.op(x, as.inv(x)) == as.e
  })
}

