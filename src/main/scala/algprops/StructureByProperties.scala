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

  private def isDefined(m: Map[String, Boolean]) = m("defined")
  private def isAssociative(m: Map[String, Boolean]) = m("associative")
  private def isCommutative(m: Map[String, Boolean]) = m("commutative")
  private def hasIdentity(m: Map[String, Boolean]) = m("identity element")
  private def hasInverse(m: Map[String, Boolean]) = m("inverse element")

  private def isMagma(m: Map[String, Boolean]) = isDefined(m)
  private def isSemiGroup(m: Map[String, Boolean]) = isMagma(m) && isAssociative(m)
  private def isMonoid(m: Map[String, Boolean]) = isSemiGroup(m) && hasIdentity(m)
  private def isCommutativeMonoid(m: Map[String, Boolean]) = isSemiGroup(m) && isMonoid(m) && isCommutative(m)
  private def isGroup(m: Map[String, Boolean]) = isMonoid(m) && hasInverse(m)
  private def isAbelianGroup(m: Map[String, Boolean]) = isGroup(m) && isCommutative(m)
}