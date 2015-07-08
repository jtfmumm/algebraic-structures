package com.jtfmumm.algprops

object StructureByProperties {
  def lookup(m: Map[String, Boolean]) = {
    m match {
      case _ if isAbelianGroup(m) => "Abelian Group"
      case _ if isGroup(m) => "Group"
      case _ if isCommutativeMonoid(m) => "Commutative Monoid"
      case _ if isMonoid(m) => "Monoid"
      case _ if isSemiGroup(m) => "SemiGroup"
      case _ if isMagma(m) => "Magma"
      case _ => "No Algebraic Structure"
    }
  }

  def isDefined(m: Map[String, Boolean]) = m("defined")
  def isAssociative(m: Map[String, Boolean]) = m("associative")
  def isCommutative(m: Map[String, Boolean]) = m("commutative")
  def hasIdentity(m: Map[String, Boolean]) = m("identity element")
  def hasInverse(m: Map[String, Boolean]) = m("inverse element")

  def isMagma(m: Map[String, Boolean]) = isDefined(m)
  def isSemiGroup(m: Map[String, Boolean]) = isMagma(m) && isAssociative(m)
  def isMonoid(m: Map[String, Boolean]) = isSemiGroup(m) && hasIdentity(m)
  def isCommutativeMonoid(m: Map[String, Boolean]) = isSemiGroup(m) && isMonoid(m) && isCommutative(m)
  def isGroup(m: Map[String, Boolean]) = isMonoid(m) && hasInverse(m)
  def isAbelianGroup(m: Map[String, Boolean]) = isGroup(m) && isCommutative(m)
}