package com.topaz.utils

import scala.concurrent.Future
import scala.util.control.NonFatal

trait TopazFail {
  def s: String
  def asThrowable: Throwable = new Exception(toString)
  def asFailedFuture: Future[Nothing] = Future.failed(new Exception(s))
  @transient val stackTrace: Option[Exception] = if(TopazFail.recordStackTraces)
    Some(new Exception(s))
  else
    None

  override def toString: String = {
    stackTrace.foreach(_.printStackTrace())
    s
  }
}

object TopazFail {
  // If you run tests with -DrecordStackTraces=1 then TopazFails will have their stack traces
  // attached and the stack traces will be printed on the toString of GeneralTopazFails
  val recordStackTraces = sys.props.get("recordStackTraces").contains("1")

  /*
   * Return type has potential for confusion here, as TopazFail("blah") looks like a constructor.
   * I wouldn't make a habit of this, however since the vast majority of uses of TopazFail
   * are in the form `Left(GeneralTopazFail(...))` it seems reasonable to make an exception.
   */
  def apply[A](s: String): Either[GeneralTopazFail, A] = Left(GeneralTopazFail(s))

  def fromException[R](f: => R): Either[TopazFail, R] = {
    try {
      Right(f)
    } catch {
      case NonFatal(e) =>
        Left(ExceptionTopazFail(e))
    }
  }
  def flatMapFromException[R](f: => Either[TopazFail, R]): Either[TopazFail, R] = {
    try {
      f
    } catch {
      case NonFatal(e) =>
        Left(ExceptionTopazFail(e))
    }
  }
}

case class GeneralTopazFail(s: String) extends TopazFail

case class ExceptionTopazFail(e: Throwable) extends TopazFail {
  @transient override val stackTrace: Option[Exception] = if(TopazFail.recordStackTraces)
    Some(new Exception(s, e))
  else
    None

  def s: String = e.toString

  override def asThrowable: Throwable = e

  override def asFailedFuture: Future[Nothing] = Future.failed(e)
}
