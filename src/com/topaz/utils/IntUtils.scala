package com.topaz.utils

object IntUtils {
  def toOrdinal(i: Int) = {
    require(i <= 110, s"toOrdinal: $i too large")
    i % 10 match {
      case 1 if i != 11 => i + "st"
      case 2 if i != 12 => i + "nd"
      case 3 if i != 13 => i + "rd"
      case _ => i + "th"
    }
  }

  implicit class PowerInt(i: Int) {
    def **(b: Int): Int = scala.math.pow(i, b).intValue
  }
}

object LongUtils {
  implicit class PowerLong(l: Long) {
    def **(b: Int): Long = scala.math.pow(l, b).longValue()
  }
}
