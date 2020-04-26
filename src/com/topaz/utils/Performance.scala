package com.topaz.utils

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Performance extends Log {

  /**
    * When testing performance it can be necessary to synchronize futures in order
    * to get accurate timings
    */
  def blockedFutureWithTime[A](msg: String)(fn : => Future[A]): Future[A] = {
    log.infoWithTime(s"Future - $msg"){
      val res = fn
      Await.result(res, Duration.Inf)
      res
    }
  }
}
