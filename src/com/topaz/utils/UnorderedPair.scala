package com.topaz.utils

class UnorderedPair[A] private(val x: A, val y: A) {
  override def hashCode(): Int = x.hashCode() * y.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case other: UnorderedPair[_] => (x == other.x && y == other.y) || (x == other.y && y == other.x)
    case _ => false
  }

  override def toString: String = s"UnorderedPair($x, $y)"
  
  def isDegenerate: Boolean = x == y
}

object UnorderedPair {
  def apply[A](x: A, y: A): UnorderedPair[A] = {
    new UnorderedPair(x, y)
  }
}
