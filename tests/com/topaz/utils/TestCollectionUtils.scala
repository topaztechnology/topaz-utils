package com.topaz.utils

trait TestCollectionUtils {
  implicit class TestRichSeq[A](seq: Seq[A]) {
    def singleValueOrThrow: A = seq match {
      case Seq(a) => a
      case other => throw new Exception(s"Expected single element, got $other")
    }
  }
}
