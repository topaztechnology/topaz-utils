package com.topaz.utils

import akka.actor.{Actor, ActorSystem, Props, Scheduler}
import akka.pattern.after
import com.topaz.TopazCodingError

import scala.concurrent.duration.{FiniteDuration, _}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Random
import scala.util.control.NonFatal

trait FutureUtils {

  // https://gist.github.com/viktorklang/9414163
  object RetryDelays {
    def withJitter(
      delays: Seq[FiniteDuration],
      maxJitter: Double,
      minJitter: Double,
      maxDelay: FiniteDuration
    ): Seq[FiniteDuration] =
      delays.map(_ * (minJitter + (maxJitter - minJitter) * Random.nextDouble)).map {
        case delay: FiniteDuration if delay > maxDelay => maxDelay
        case o: FiniteDuration => o
        case o: Duration => throw TopazCodingError(s"Can't have infinite durations: $o")
      }

    val fibonacci: Stream[FiniteDuration] = 0.seconds #:: 1.seconds #:: (fibonacci zip fibonacci.tail).map { t => t._1 + t._2 }
  }

  def retry[T](
    f: => Future[T], delays: Seq[FiniteDuration], retries: Int, callbackOnRecovery: Throwable => Unit = _ => Unit
  )(
    implicit ec: ExecutionContext, s: Scheduler): Future[T] = {
    f recoverWith {
      case e if retries > 0 && delays.nonEmpty =>
        callbackOnRecovery(e)
        after(delays.head, s)(retry(f, delays.tail, retries - 1, callbackOnRecovery))
    }
  }

  def maybeRetry[T](
    f: => Future[T], delay: FiniteDuration, retries: Int, shouldRetry: Throwable => Boolean
  )(
    implicit ec: ExecutionContext, s: Scheduler): Future[T] = {
    f recoverWith {
      case e if retries > 0 && shouldRetry(e) =>
        after(delay, s)(maybeRetry(f, delay, retries - 1, shouldRetry))
    }
  }

  implicit class FutureExtensions[T](f: Future[T]) {
    def withTimeout(
      duration: FiniteDuration, timeout: => Throwable
    )(implicit scheduler: Scheduler, dispatcher: ExecutionContext): Future[T] = {
      Future firstCompletedOf Seq(f, after(duration, scheduler)(Future.failed(timeout)))
    }
  }

  def flatten[T](list: Seq[Future[Seq[T]]])(implicit ec: ExecutionContext): Future[Seq[T]] = {
    Future.sequence(list).map(_.flatten)
  }
}


object FutureUtils extends FutureUtils {

  /**
    * If a block of code that returns a Future can throw you can wrap it in this and it will
    * make sure you get a failed Future instead of an exception. Handy for code like:
    * def m(p: P): Future[T] = FutureUtils.safe {
    *   require(p == ...)
    *   doSomethingThatReturnsAFuture(p)
    * }
    */
  def safe[T](f: => Future[T]): Future[T] = {
    try {
      f
    } catch {
      case NonFatal(e) =>
        Future.failed(e)
    }
  }

  /**
    * Similar to FutureFirstAndLastQueue. Only create a Future using `f` after `completeFirst` has completed
    * and if `ifCond` is true.
    * 
    * If `ifCond` is false after `completeFirst` finishes then the result of `completeFirst` is returned.
    */
  object LazyConditionalFuture extends Log {

    def apply[T](f: () => Future[T], completeFirst: Future[T], ifCond: () => Boolean): Future[T] = {
      val promise = Promise[T]()

      completeFirst.onComplete {
        result =>
          if (ifCond()) {
            promise.tryCompleteWith(try {
              f()
            } catch {
              case NonFatal(e) =>
                log.warn("The passed work to run shouldn't have thrown.", e)
                Future.failed(e)
            })
          } else {
            log.debug("Not running future as condition is false.")
            promise.complete(result)
          }
      }(scala.concurrent.ExecutionContext.Implicits.global)

      promise.future
    }
  }
  
  object ResultNotNeededException extends Exception("Future failed as result wasn't needed")
  
  /**
    * In some cases when we have work to do we only care if the first piece of work finishes
    * and the last piece finishes. Any work we would have scheduled in between can be dropped.
    */
  class FutureFirstAndLastQueue()(implicit system: ActorSystem) extends Log {
    private case class Work[T](func: () => Future[T], promise: Promise[T], running: Boolean)
    private val actor = system.actorOf(Props(new QueueActor))

    def add[T](func: () => Future[T]): Future[T] = {
      val promise = Promise[T]()
      actor ! Work(func, promise, running = false)
      promise.future
    }

    class QueueActor extends Actor {
      private var queue = List[Work[_]]()

      def receive: Receive = {
        case work: Work[_] =>

          // just keep the head and the new piece of work
          queue = queue match {
            case Nil => work :: Nil
            case h :: others => 
              others.foreach(_.promise.tryFailure(ResultNotNeededException))
              h :: work :: Nil
          }
          
          self ! 'flush

        case 'flush =>
          queue = queue.headOption match {
            case Some(w @ Work(toRun, promise, false)) =>
              val future = try {
                toRun()
              } catch {
                case NonFatal(e) =>
                  log.warn("The passed work to run shouldn't have thrown.", e)
                  Future.failed(e)
              }
              future.onComplete {
                r =>
                  self ! 'completedWork
                  promise.tryComplete(r)
              }(context.dispatcher)
              w.copy(running = true) :: queue.tail
            case _ =>
              queue
          }

        case 'completedWork =>
          queue = queue.drop(1)
          self ! 'flush
      }
    }
  }
}
