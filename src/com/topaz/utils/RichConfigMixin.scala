package com.topaz.utils

import java.io.File

import akka.util.Timeout
import com.typesafe.config.Config

import scala.concurrent.duration.{Duration, FiniteDuration}

trait RichConfigMixin {
  implicit class RC(conf: Config) {
    def getFiniteDuration(path: String): FiniteDuration = {
      Duration.fromNanos(conf.getDuration(path).toNanos).toCoarsest
    }
    def getTimeout(path: String): Timeout = {
      Timeout(getFiniteDuration(path))
    }
    def getFile(path: String): File = {
      new File(conf.getString(path))
    }
  }
}
