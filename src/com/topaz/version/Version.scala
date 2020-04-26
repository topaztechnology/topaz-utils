package com.topaz.version

case class Version(value: Long) extends AnyVal with Ordered[Version] {

  def compare(rhs: Version): Int = value.compare(rhs.value)

  def increment: Version = copy(value + 1)
  
  def decrement: Version = copy(value - 1)
}
