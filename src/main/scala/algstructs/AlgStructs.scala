package com.jtfmumm.algstructs


trait AlgStruct[A] {
  def is(a: A, b: A): Boolean = a == b
}

trait Magma[A] extends AlgStruct[A] {
  def op(a: A, b: A): A
  //Defined for any (a, b) in A.
}

trait SemiGroup[A] extends Magma[A] {
    //Op is associative
}

trait Monoid[A] extends SemiGroup[A] {
  val e: A
    //Contains an identity element
}

trait Group[A] extends Monoid[A] {
  def inv(a: A): A
    //Contains an inverse element
}

trait AbelianGroup[A] extends Group[A] {
    //Op is commutative
}

trait Ring[A] extends AbelianGroup[A] {
  def mult(a: A, b: A): A
  //Mult is associative and distributive over op
}

trait IntegralDomain[A] extends Ring[A] {
  //Mult is commutative
  def one: A
  //Contains identity element for mult (unity)
  //Cancellation property holds
}

trait Field[A] extends IntegralDomain[A] {
  //Contains inverse element for mult (unity)
  def multInv(a: A): A
}
