package com.topaz.utils

case class LazyIndexedSeq[A](length: Int, lookup: Int => A) extends IndexedSeq[A] {
  def apply(i: Int) = lookup(i)
}
