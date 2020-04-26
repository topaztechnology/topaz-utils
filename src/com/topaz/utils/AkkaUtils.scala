package com.topaz.utils

import akka.actor.{ActorSystem, Cancellable, Scheduler, Terminated}
import com.topaz.TopazCodingError

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Try}

object AkkaUtils {
  def runIfTerminateFailed(
    terminatedFuture: Future[Terminated], duration: FiniteDuration
  )(f: () => Unit): Unit = {
    new Thread {
      setDaemon(true)
      setName("AkkaUtils.runIfTerminateFailed")
      override def run(): Unit = {
        Try(Await.ready(terminatedFuture, duration)) match {
          case Failure(_) =>
            f()
          case _ =>
        }
      }
    }.start()
  }

  def runIfTerminateFailed(
    system: ActorSystem, duration: FiniteDuration
  )(f: () => Unit): Unit = {
    runIfTerminateFailed(system.whenTerminated, duration)(f)
  }
  
  def scheduleOnce(
    duration: FiniteDuration
  )(f: ⇒ Unit)(implicit scheduler: Scheduler, executor: ExecutionContext): Unit ={
    try {
      scheduler.scheduleOnce(duration) {
        f
      }
    } catch {
      case e: IllegalStateException if e.getMessage.contains("cannot enqueue after timer shutdown") =>
      // Ignore this exception. Akka runs anything scheduled on shutdown so this method is run during
      // a shutdown and then when it tries to reschedule itself, fails.
      // https://stackoverflow.com/questions/23464194/why-scheduleonce-runs-everytime-on-app-shutdown
    }
  }

  case object DummyScheduler extends Scheduler {
    def schedule(initialDelay: FiniteDuration, interval: FiniteDuration, runnable: Runnable)(implicit executor: ExecutionContext): Cancellable = {
      throw TopazCodingError("DummyScheduler shouldn't be called: schedule")
    }

    def scheduleOnce(delay: FiniteDuration, runnable: Runnable)(implicit executor: ExecutionContext): Cancellable = {
      throw TopazCodingError("DummyScheduler shouldn't be called: scheduleOnce")
    }

    def maxFrequency: Double = {
      throw TopazCodingError("DummyScheduler shouldn't be called: maxFrequency")
    }
  }
}
