package com.topaz.utils

import org.scalatest.Assertions

trait EitherTestPimps extends EitherPimpsMixin with Assertions {
  implicit class RichEither[A, B](e: Either[A, B]) {
    def R: B = {
      e match {
        case Right(b) => b
        case Left(e: ExceptionTopazFail) =>
          fail(s"ExceptionTopazFail thrown $e - (${e.e})")
        case Left(e) =>
          fail(s"Expected Right but got $e - ${e.getClass}")
      }
    }
    def L: A = {
      e match {
        case Left(a) => a
        case Right(b) =>
          fail(s"Expected Left but got $b - ${b.getClass}")
      }
    }
  }
}
