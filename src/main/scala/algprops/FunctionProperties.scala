package com.jtfmumm.algprops

object FProps {
  def isInjection[A, B](f: A => B, xs: Set[A], ys: Set[B]): Boolean = {
    val range = xs.map(f)
    range.subsetOf(ys) && range.size == xs.size  //ensure there is one unique y for every x
  }
  def isSurjection[A, B](f: A => B, xs: Set[A], ys: Set[B]): Boolean = {
    val range = xs.map(f)
    range.subsetOf(ys) && range.size == ys.size  //ensure there is a mapping to every y
  }
  def isBijection[A, B](f: A => B, xs: Set[A], ys: Set[B]): Boolean = {
    isInjection(f, xs, ys) && isSurjection(f, xs, ys)
  }
}