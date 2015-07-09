package com.jtfmumm.algstructs

import com.jtfmumm.algprops._


// We can use assertions to verify that our structure really is what we're claiming in the code.

case class MultMod5Without0 extends FiniteGroup[Int] {
  val set = Set(1, 2, 3, 4)
  def op(a: Int, b: Int): Int = (a * b) % 5
  val e = 1
  def inv(a: Int) = a match {
    case 1 => 1
    case 2 => 3
    case 3 => 2
    case 4 => 4
  }
  assert(PropertyChecker.verifyStructure(this))
}

//And now for something that isn't really a group
case class MultMod4Without0 extends FiniteGroup[Int] {
  val set = Set(1, 2, 3)
  def op(a: Int, b: Int): Int = (a * b) % 4
  val e = 1
  def inv(a: Int) = a match {
    case 1 => 1
    case 2 => 3
    case 3 => 2
  }
  assert(PropertyChecker.verifyStructure(this))
}