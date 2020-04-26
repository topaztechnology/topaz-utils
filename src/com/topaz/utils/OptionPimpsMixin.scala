package com.topaz.utils

import java.util.Optional

trait OptionPimpsMixin {
  implicit class RichOption[T](opt: Option[T]) {
    def asJava: Optional[T] = Optional.ofNullable(opt.getOrElse(null).asInstanceOf[T])
    def getOrThrowWithMessage(m: => String): T = if (opt.isDefined) opt.get else sys.error(m)
    def toFail(m: => String): Either[TopazFail, T] = opt.toRight(GeneralTopazFail(m))
  }

  implicit class RichOptionTuple[A, B](opt: Option[(A, B)]) {
    def unzip: (Option[A], Option[B]) = opt.map {
      case (a, b) => Some(a) -> Some(b)
    }.getOrElse(None -> None)
  }

  implicit class RichOptional[T](opt: Optional[T]) {
    def asScala: Option[T] = if (opt.isPresent) Some(opt.get()) else None
  }

  def reduceLeftIfAllDefined[A, B >: A](seq: Seq[Option[A]])(op: (B, A) => B): Option[B] = {
    val as = seq.flatten
    if (as.size == seq.size)
      Some(as.reduceLeft(op))
    else
      None
  }
}

object OptionPimpsMixin extends OptionPimpsMixin

object TOption {
  def when[A](cond: Boolean)(a: => A): Option[A] = { // available on Option in 2.13
    if (cond) Some(a) else None
  }
}
