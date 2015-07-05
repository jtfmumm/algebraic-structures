
trait AlgStruct[A] {
  val set: A

  def op: (A, A) => A
    //Defined for any (a, b) in A.
}

trait SemiGroup[A] extends AlgStruct {
    //Op is associative
}

trait Monoid[A] extends SemiGroup {
    //Contains an identity element
}

trait Group[A] extends Monoid {
    //Contains an inverse element
}

trait AbelianGroup[A] extends Group {
    //Op is commutative
}