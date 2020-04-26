package com.topaz.utils

import shapeless.Typeable

import scala.language.implicitConversions
import scala.reflect.ClassTag

trait RichAnys {
  implicit class RichAny[T](protected val value: T) extends EitherPimpsMixin {

    /**
     * type safe cast provided by shapeless. returns None if the cast fails
     *
     * https://github.com/milessabin/shapeless/wiki/Feature-overview:-shapeless-2.0.0#type-safe-cast
     */
    def cast[V](implicit t: Typeable[V]): Option[V] = t.cast(value).filter(_ != null)
    
    def to[V](implicit t: Typeable[V], ct: ClassTag[V]): Either[TopazFail, V] = cast(t).toRight(
      GeneralTopazFail(s"'$value', of type ${value.getClass.getName} is not a ${ct.runtimeClass}")
    )
    
    def toF[V](implicit t: Typeable[V], ct: ClassTag[V]): Either[TopazFail, V] = to[V]

    /**
     * tries to cast and provides a better exception than asInstanceOf if it fails.
     */
    def as[V](implicit t: Typeable[V], ct: ClassTag[V]): V = to.getOrErrorLeft(_.s)

    def eitherFlatMatch[R](f: PartialFunction[T, Either[TopazFail, R]]): Either[TopazFail, R] = {
      if (f.isDefinedAt(value)) {
        f(value)
      } else {
        Left(ExceptionTopazFail(new MatchError(value)))
      }
    }

    def eitherMatch[R](f: PartialFunction[T, R]): Either[TopazFail, R] = {
      if (f.isDefinedAt(value)) {
        Right(f(value))
      } else {
        Left(ExceptionTopazFail(new MatchError(value)))
      }
    }
  }

}

object RichAnys extends RichAnys

trait RichBooleans {
  implicit class RichBoolean(val b: Boolean) {
    final def option[A](a: => A): Option[A] = if (b) Some(a) else None
  }
}
