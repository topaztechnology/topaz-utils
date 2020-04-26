package com.topaz.utils

import org.scalatest.Suite
import org.scalatest.matchers.{BeMatcher, MatchResult}

trait ScalaTestUtils {
  self : Suite  =>

  private def asString(c: Traversable[_]): String = {
    if (c.isEmpty)
      "[Empty]"
    else if (c.size > 5)
      c.take(5).mkString("[", ", ", ", ...]")
    else
      c.mkString("[", ", ", "]")
  }
  def checkContentMatches(lhs: Traversable[_], rhs: Traversable[_]) {
    val lSet = lhs.toSet
    val rSet = rhs.toSet
    if (lSet != rSet) {
      val lDiff = lSet.diff(rSet)
      val rDiff = rSet.diff(lSet)

      val msg =
        s"""|Content mismatch:
            |   Left Diff  ${asString(lDiff)}
            |   Right Diff ${asString(rDiff)}
            |""".stripMargin

      fail(msg)
    }
  }

  import org.scalactic._
  val whiteSpaceNormalised: Uniformity[String] =
    new AbstractStringUniformity {
      def normalized(s: String): String = s.trim().stripLineEnd
      override def toString: String = "whiteSpaceNormalised"
    }

  implicit class LevenshteinMatcher(rhs: String) {
    import scala.math._
    // https://gist.github.com/gustavonalle/9e9430507a40fd3f410b
    private def levenshtein(s1: String, s2: String) = {
      def minimum(i1: Int, i2: Int, i3: Int) = min(min(i1, i2), i3)
      val dist = Array.tabulate(s2.length + 1, s1.length + 1) { (j, i) => if (j == 0) i else if (i == 0) j else 0 }
      for (j <- 1 to s2.length; i <- 1 to s1.length)
        dist(j)(i) = if (s2(j - 1) == s1(i - 1)) dist(j - 1)(i - 1)
        else minimum(dist(j - 1)(i) + 1, dist(j)(i - 1) + 1, dist(j - 1)(i - 1) + 1)
      dist(s2.length)(s1.length)
    }

    private def levenshteinPercent(s1: String, s2: String) = {
      val maxLen = max(s1.length, s2.length)
      val dist = levenshtein(s1, s2)
      (maxLen - dist) / maxLen.toDouble
    }

    def withinPercentOfExpected(pc: Double): BeMatcher[String] = new BeMatcher[String] {
      def apply(lhs: String): MatchResult = {
        val matchPercent = levenshteinPercent(lhs, rhs) * 100

        if (matchPercent >= pc) {
          MatchResult(matches = true, "", "")
        } else {
          MatchResult(matches = false, s"'$lhs' is not within $pc of '$rhs'. Distance: $matchPercent.", "Match")
        }
      }
    }
  }

}

