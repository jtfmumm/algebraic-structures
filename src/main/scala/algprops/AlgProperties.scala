package main.scala.algprops
import org.scalacheck.Prop._
import org.scalacheck._
import main.scala.algstructs._



object AlgProperties {
  def standardEquality[A](a: A, b: A) = a == b

  // closure and injection are assured by the fact that in this implementation op is always a function of the form (A, A) => A
  def definedForAllElementsOn[A](as: Magma[A], genFor: Gen[List[A]], is: (A, A) => Boolean = standardEquality[A] _) = forAll(genFor)(l => {
    val x = l(0)
    val y = l(1)
    try {
      as.op(x, y)
      true
    }
    catch {
      case _: Throwable => false
    }
  })
  def associativityOn[A](as: Magma[A], genFor: Gen[List[A]], is: (A, A) => Boolean = standardEquality[A] _) = forAll(genFor)(l => {
    val x = l(0)
    val y = l(1)
    val z = l(2)
    is(as.op(as.op(x, y), z), as.op(x, as.op(y, z)))
  })
  def commutativityOn[A](as: Magma[A], genFor: Gen[List[A]], is: (A, A) => Boolean = standardEquality[A] _) = forAll(genFor)(l => {
    val x = l(0)
    val y = l(1)
    is(as.op(x, y), as.op(y, x))
  })
  def identityOn[A](as: Monoid[A], genFor: Gen[A], is: (A, A) => Boolean = standardEquality[A] _) = forAll(genFor)(x => {
    is(as.op(x, as.e), x)
  })
  def inverseOn[A](as: Group[A], genFor: Gen[A], is: (A, A) => Boolean = standardEquality[A] _) = forAll(genFor)(x => {
    is(as.op(x, as.inv(x)), as.e)
  })
}

