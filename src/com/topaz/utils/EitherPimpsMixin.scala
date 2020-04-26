package com.topaz.utils

import scala.language.implicitConversions
import scala.util.Either.RightProjection
import com.topaz.TopazCodingError

import scala.concurrent.Future
import scala.util.control.{ControlThrowable, NonFatal}
import scala.collection.breakOut

trait EitherPimpsMixin {
  def ensure(f: => Boolean, msg: => String): Either[TopazFail, Unit] = {
    if(!f) {
      TopazFail(msg)
    } else {
      Right(Unit)
    }
  }

  def ensureF(f: => Either[TopazFail, Boolean], msg: => String): Either[TopazFail, Unit] = {
    f match {
      case Left(value) => Left(value)
      case Right(false) => TopazFail(msg)
      case Right(true) => Right(Unit)
    }
  }

  def ensureAll[T](s: Seq[T])(check: T => Boolean, message: T => String): Either[TopazFail, Unit] = {
    firstToFail(s)(t => if (!check(t)) TopazFail(message(t)) else Right(Unit))
  }

  implicit def rightBias[L, R](either: Either[L, R]): RightProjection[L, R] = either.right

  implicit class EitherTopazFailPimp[R](e: Either[TopazFail, R]) {
    def getOrThrow(): R = if (e.isRight) e.right.get else throw e.left.get.asThrowable
    def toFuture: Future[R] = {
      e match {
        case Left(fail) => Future.failed(fail.asThrowable)
        case Right(value) => Future.successful(value)
      }
    }
    def flattenWithFuture[F](f: R => Future[Either[TopazFail, F]]): Future[Either[TopazFail, F]] = {
      e match {
        case Left(fail) => Future.successful(Left(fail))
        case Right(value) => f(value)
      }
    }
  }

  implicit class RightBiasedEitherPimp[L, R](e: Either[L, R]) {
    def getOrThrowWithMessage(m: String): R = if (e.isRight) e.right.get else sys.error(m)
    def getOrErrorLeft(f: L => String): R = if (e.isRight) e.right.get else sys.error(f(e.left.get))
    def getOrThrowLeft(f: L => Nothing): R = if (e.isRight) e.right.get else f(e.left.get)
    def getOrRecoverLeft(f: L => R): R = if (e.isRight) e.right.get else f(e.left.get)
    def recover(f: PartialFunction[L, R]): Either[L, R] = if (e.isLeft) Right(f(e.left.get)) else e
    def orElse(x: => Either[L, R]): Either[L, R] = if (e.isLeft) x else e
    def orElseWithOriginalLeft(x: => Either[L, R]): Either[L, R] = {
      if (e.isLeft)
        x match {
          case r@Right(_) => r
          case _ => e
        }
      else
        e
    }
  }

  def firstToSucceed[R](failMsg: String, seq: Seq[() => Either[TopazFail, R]]): Either[TopazFail, R] ={
    var errors = Seq[TopazFail]()
    val it = seq.iterator
    while (it.hasNext) {
      val a = it.next()
      val res = a()
      if (res.isRight)
        return res
      else
        errors :+= res.left.get
    }
    val fullMsg = s"$failMsg (Errors: ${errors.map(_.s).mkString(", ")})"
    TopazFail(fullMsg)
  }

  def firstToFail[A, R](seq: Seq[A])(fn: A => Either[TopazFail, Unit]): Either[TopazFail, Unit] = {
    val it = seq.iterator
    while (it.hasNext) {
      val a = it.next()
      val res = fn(a)
      if (res.isLeft)
        return res
    }
    Right(Unit)
  }

  def flatten[L, R](seq: Traversable[Either[L, R]]): Either[L, IndexedSeq[R]] = {
    val acc = new scala.collection.immutable.VectorBuilder[R]()
    var maybeFailure: Option[L] = None
    seq.foreach {
      a =>
        if (maybeFailure.isEmpty) {
          a match {
            case Left(l) =>
              maybeFailure = Some(l)
            case Right(r) =>
              acc += r
          }
        }
    }
    maybeFailure.toLeft(acc.result)
  }

  def buildMapOrLeft[A, L, K, V](as: Traversable[A])(fn: A => Either[L, (K, V)]): Either[L, Map[K, V]] = {
    val break = new EitherBreakable[L]
    break.eitherBreakable {
      as.map {
        a => break.onLeft(fn(a))
      }(collection.breakOut)
    }
  }

  def mapOrErrorLeft[A, L, R](seq: Traversable[A])(fn: A => Either[L, R]): Either[L, IndexedSeq[R]] = {
    val break = new EitherBreakable[L]
    break.eitherBreakable {
      seq.map {
        a => break.onLeft(fn(a))
      }(collection.breakOut)
    }
  }

  def mapValuesOrErrorLeft[K, V, L, R](m: Map[K, V])(fn : V => Either[L, R]): Either[L, Map[K, R]] = {
    buildMapOrLeft(m){
      case (k, v) => fn(v).map(k -> _)
    }
  }

  def flatMapOrErrorLeft[A, L, R](seq: Traversable[A])(fn: A => Either[L, Seq[R]]): Either[L, IndexedSeq[R]] = {
    mapOrErrorLeft(seq)(fn).map(_.flatten)
  }

  def foldOrErrorLeft[L, A, R](r: R, seq: Traversable[A])(fn: (R, A) => Either[L, R]): Either[L, R] = {
    seq.foldLeft(Right(r).asInstanceOf[Either[L, R]]){
      case (Left(l), _) => Left(l)
      case (Right(r1), r2) => fn(r1, r2)
    }
  }

  def reduceOrErrorLeft[L, R](seq: Traversable[R])(fn: (R, R) => Either[L, R]): Either[L, R] =
    foldOrErrorLeft[L, R, R](seq.head, seq.tail)(fn)

  def filterOrErrorLeft[L, A](seq: Traversable[A])(fn: A => Either[L, Boolean]): Either[L, IndexedSeq[A]] = {
    for {
      predicates <- mapOrErrorLeft(seq)(fn)
    } yield {
      seq.toSeq.zip(predicates).filter(_._2).map(_._1)(breakOut)
    }
  }


  def pushDownOption[L, R](opt: Option[Either[L, R]]): Either[L, Option[R]] = {
    opt match {
      case None => Right(None)
      case Some(Right(v)) => Right(Some(v))
      case Some(Left(e)) => Left(e)
    }
  }

  def liftOption[L, R](e: Either[L, Option[R]]): Option[Either[L, R]] = e match {
    case Right(Some(r)) =>
      Some(Right(r))
    case Right(None) =>
      None
    case Left(left) =>
      Some(Left(left))
  }

  def safe[R](f: => Either[TopazFail, R]): Either[TopazFail, R] = {
    try {
      f
    } catch {
      case NonFatal(t) => Left(ExceptionTopazFail(t))
    }
  }

  implicit class RichEitherSeq[L, A](seq: Either[L, Seq[A]]) {
    def ++(other: Either[L, Seq[A]]): Either[L, Seq[A]] = {
      for {
        a <- seq
        b <- other
      } yield a ++ b
    }
  }

  def codingErrorToLeft[R](body: => R): Either[ExceptionTopazFail, R] = {
    try {
      Right(body)
    } catch {
      case e: TopazCodingError =>
        Left(ExceptionTopazFail(e))
    }
  }
}

object EitherUtils extends EitherPimpsMixin

/**
  * Similar to scala.util.control.Breaks
  * Sometimes using mapOrErrorLeft creates hard to follow code and using this can
  * be clearer.
  */
class EitherBreakable[L] {

  private case class EitherBreakControl(l: L) extends ControlThrowable

  implicit class breakFromEither[R](e: Either[L, R]) {
    def orBreak: R = onLeft(e)
  }

  def eitherBreakable[T](op: => T): Either[L, T] = {
    try {
      Right(op)
    } catch {
      case ex: EitherBreakControl => Left(ex.l)
    }
  }

  def onLeft[R](f: => Either[L, R]): R = {
    f match {
      case Left(value) => throw EitherBreakControl(value)
      case Right(value) => value
    }
  }

  def require(f: => Boolean, l: => L): Unit = {
    if (!f) throw EitherBreakControl(l)
  }
}
