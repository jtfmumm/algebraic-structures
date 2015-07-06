package main.scala.algstructs


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
