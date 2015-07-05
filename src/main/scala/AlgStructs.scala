
trait AlgStruct[A] {
  def op(a: A, b: A): A
    //Defined for any (a, b) in A.
}

trait SemiGroup[A] extends AlgStruct[A] {
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


case class IntAdd extends AbelianGroup[Int] {
  val e = 0
  def inv(a: Int): Int = 0 - a

  def op(a: Int, b: Int): Int = a + b
}

case class StringConcat extends Monoid[String] {
  val e = ""
  def op(a: String, b: String): String = a + b
}