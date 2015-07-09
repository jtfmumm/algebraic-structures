package com.jtfmumm.algstructs

import com.jtfmumm.algprops.FProps


trait AlgStruct[A] {
  def is(a: A, b: A): Boolean = a == b
}

trait Magma[A] extends AlgStruct[A] {
  //Defined for any (a, b) in A.
  def op(a: A, b: A): A
}

trait SemiGroup[A] extends Magma[A] {
    //Op is associative
}

trait Monoid[A] extends SemiGroup[A] {
  //Contains an identity element
  val e: A
}

trait Group[A] extends Monoid[A] {
  //Contains an inverse element
  def inv(a: A): A
}

trait AbelianGroup[A] extends Group[A] {
    //Op is commutative
}

trait Ring[A] extends AbelianGroup[A] {
  //Mult is associative and distributive over op
  def mult(a: A, b: A): A
}

trait IntegralDomain[A] extends Ring[A] {
  //Contains identity element for mult (unity)
  def one: A
  //Mult is commutative
  //Cancellation property holds
}

trait Field[A] extends IntegralDomain[A] {
  //Contains inverse element for mult (unity)
  def multInv(a: A): A
}

// FINITE


trait FiniteStructure[A] extends AlgStruct[A] {
  //Contains a finite set
  val set: Set[A]

  def order = set.size

  def contains(x: A) = set.contains(x)
}

trait FiniteMagma[A] extends Magma[A] with FiniteStructure[A]

trait FiniteSemiGroup[A] extends SemiGroup[A] with FiniteMagma[A]

trait FiniteMonoid[A] extends Monoid[A] with FiniteSemiGroup[A]

trait FiniteGroup[A] extends Group[A] with FiniteMonoid[A] {
  def isSubgroupOf(g: FiniteGroup[A]) = set.subsetOf(g.set)

  def isHomomorphismTo[B](f: A => B, h: FiniteGroup[B]) = {
    // f(xy) = f(x)*f(y) where * is in group h
    set.forall((x: A) =>
      set.forall((y: A) => f(op(x, y)) == h.op(f(x), f(y)))
    )
  }

  def isHomomorphismOnto[B](f: A => B, h: FiniteGroup[B]) = {
    isHomomorphismTo[B](f, h) && FProps.isSurjection[A, B](f, set, h.set)
  }


  def elOrder(x: A): Int = {
    if (!set.contains(x)) throw new Exception("x must be an element of the group!")
    if (x == e) return 1
    def loop(cur: A, count: Int): Int = {
      val newCur = op(x, cur)
      if (newCur == e) count else loop(newCur, count + 1)
    }
    loop(x, 2)
  }
}