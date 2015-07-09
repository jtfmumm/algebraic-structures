package com.jtfmumm.algprops

import org.scalacheck.Prop._
import org.scalacheck._
import com.jtfmumm.algstructs._



object AlgProperties {
  // closure and injection are assured by the fact that in this implementation op is always a function of the form (A, A) => A
  def definedForAllElementsOn[A](as: Magma[A], genFor: Gen[List[A]]) = forAll(genFor)(l => {
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
  def associativityOn[A](as: Magma[A], genFor: Gen[List[A]]) = forAll(genFor)(l => {
    val x = l(0)
    val y = l(1)
    val z = l(2)
    as.is(as.op(as.op(x, y), z), as.op(x, as.op(y, z)))
  })
  def commutativityOn[A](as: Magma[A], genFor: Gen[List[A]]) = forAll(genFor)(l => {
    val x = l(0)
    val y = l(1)
    as.is(as.op(x, y), as.op(y, x))
  })
  def identityOn[A](as: Monoid[A], genFor: Gen[A]) = forAll(genFor)(x => {
    as.is(as.op(x, as.e), x)
  })
  def inverseOn[A](as: Group[A], genFor: Gen[A]) = forAll(genFor)(x => {
    as.is(as.op(x, as.inv(x)), as.e)
  })
  def multiplicativeAssociativityOn[A](as: Ring[A], genFor: Gen[List[A]]) = forAll(genFor)(l => {
    val x = l(0)
    val y = l(1)
    val z = l(2)
    as.is(as.mult(as.mult(x, y), z), as.mult(x, as.mult(y, z)))
  })
  def multiplicativeCommutativityOn[A](as: Ring[A], genFor: Gen[List[A]]) = forAll(genFor)(l => {
    val x = l(0)
    val y = l(1)
    as.is(as.mult(x, y), as.mult(y, x))
  })
  def unityOn[A](as: IntegralDomain[A], genFor: Gen[A]) = forAll(genFor)(x => {
    as.is(as.mult(x, as.one), x)
  })
  def multiplicativeInverseOn[A](as: Field[A], genFor: Gen[A]) = forAll(genFor)(x => {
    as.is(as.mult(x, as.multInv(x)), as.one)
  })


  //FINITE STRUCTURES
  def definedForAllElementsOn[A](as: FiniteMagma[A]): Boolean = {
    (for (
      x <- as.set;
      y <- as.set
    ) yield {
      try {
        as.op(x, y)
        true
      }
      catch {
        case _: Throwable => false
      }
    }).forall(result => result)
  }
  def associativityOn[A](as: FiniteMagma[A]): Boolean = {
    (for (
      x <- as.set;
      y <- as.set;
      z <- as.set
    ) yield {
      as.is(as.op(as.op(x, y), z), as.op(x, as.op(y, z)))
    }).forall(result => result)
  }
  def commutativityOn[A](as: FiniteMagma[A]): Boolean = {
    (for (
      x <- as.set;
      y <- as.set
    ) yield {
      as.is(as.op(x, y), as.op(y, x))
    }).forall(result => result)
  }
  def identityOn[A](as: FiniteMonoid[A]): Boolean = {
    (for (
      x <- as.set;
      y <- as.set
    ) yield {
      as.is(as.op(x, as.e), x)
    }).forall(result => result)
  }
  def inverseOn[A](as: FiniteGroup[A]): Boolean = {
    (for (
      x <- as.set;
      y <- as.set
    ) yield {
      as.is(as.op(x, as.inv(x)), as.e)
    }).forall(result => result)
  }
}

