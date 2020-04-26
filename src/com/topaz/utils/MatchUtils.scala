package com.topaz.utils

import scala.math.BigDecimal

object DoubleUtils {

  implicit class RicherDouble(d: Double) {
    def pow(pow: Double): Double = math.pow(d, pow)

    def sqrt: Double = math.sqrt(d)
    
    def isAlmostZero: Boolean = almostEquals(0.0)
    
    def almostEquals(other: Double, tolerance: Double = 1e-9): Boolean = DoubleUtils.almostEquals(d, other, tolerance)
  }

  def almostEquals(a: Double, b: Double, tolerance: Double): Boolean = {
    math.abs(a - b) <= tolerance.abs ||
      a.isInfinite && b.isInfinite ||
      a.isNaN && b.isNaN
  }
}

object ParseDouble {
  def unapply(a: Any): Option[Double] = try {
    a match {
      case s: String => Some(s.toDouble)
      case d: java.lang.Double => Some(d)
      case i: java.lang.Integer => Some(i.toDouble)
      case _ => None
    }
  } catch {
    case _: NumberFormatException => None
  }
}

object ParseInt {
  def unapply(a: Any): Option[Int] = try {
    a match {
      case s: String => Some(s.trim.toInt)
      case i: java.lang.Integer => Some(i)
      case _ => None
    }
  } catch {
    case _: NumberFormatException => 
      // Excel sends a "2" in a cell as a string "2.0"
      ParseDouble.unapply(a).collect {
        case d if d.toInt == d => d.toInt
      }
  }
}

object ParseBigDecimal {
  def unapply(s: String): Option[BigDecimal] = try {
    Some(BigDecimal(s))
  } catch {
    case _: NumberFormatException => None
  }
}

object LowerCaseMatchString {
  def unapply(s: String): Option[String] = {
    Some(s.toLowerCase)
  }
}
