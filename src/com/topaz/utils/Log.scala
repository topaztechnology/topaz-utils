package com.topaz.utils

import ch.qos.logback.classic.pattern.ClassicConverter
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.util.OptionHelper.extractDefaultReplacement
import com.topaz.utils.Log.Level
import com.topaz.utils.Log.Level.{Debug, Error, Info, Trace, Warn}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.immutable
import scala.concurrent.Future
import scala.util.control.NonFatal

trait Log {
  private val logger: Logger = LoggerFactory.getLogger(this.getClass)
  val log: RichLog = RichLog(logger)

  case class RichLog(logger: Logger) {
    def log(level: Level, msg: => String, t: Throwable = null): Unit = {
      def fullMsg = Log.prefix + msg
      level match {
        case Trace =>
          if (logger.isTraceEnabled)
            logger.trace(fullMsg)
        case Debug =>
          if (logger.isDebugEnabled)
            logger.debug(fullMsg)
        case Info =>
          if (logger.isInfoEnabled())
            logger.info(fullMsg)
        case Warn if t == null =>
          if (logger.isWarnEnabled())
            logger.warn(fullMsg)
        case Warn if t == null =>
          if (logger.isWarnEnabled())
            logger.warn(fullMsg)
        case Warn =>
          if (logger.isWarnEnabled())
            logger.warn(fullMsg, t)
        case Error if t == null =>
          if (logger.isErrorEnabled())
            logger.error(fullMsg)
        case Error =>
          if (logger.isErrorEnabled())
            logger.error(fullMsg, t)
      }
    }

    def trace(msg: => String): Unit = {
      log(Trace, msg)
    }

    def debug(msg: => String): Unit = {
      log(Debug, msg)
    }

    def info(msg: => String): Unit = {
      log(Info, msg)
    }

    def warn(msg: => String): Unit = {
      log(Warn, msg)
    }

    def warn(msg: => String, t: Throwable): Unit = {
      log(Warn, msg, t)
    }

    def error(msg: => String): Unit = {
      log(Error, msg)
    }

    def error(msg: => String, t: Throwable): Unit = {
      log(Error, msg, t)
    }

    def logWithTime[A](level: Level, msg: String)(fn: => A): A = {
      val stopwatch = Stopwatch()
      log(level, s"Starting: $msg")
      val result = try {
        fn
      } catch {
        case NonFatal(e) =>
          log(level, s"Finished (failed): $msg. Took ${stopwatch.elapsedString()}")
          throw e
      }
      log(level, s"Finished: $msg. Took ${stopwatch.elapsedString()}")
      result
    }

    def debugWithTime[A](msg: String)(fn: => A): A = logWithTime(Debug, msg)(fn)

    def infoWithTime[A](msg: String)(fn: => A): A = logWithTime(Info, msg)(fn)

    def logIfLongerThan[A](level: Level, msg: => String, timeMS: Int)(fn: => A): A = {
      val stopwatch = Stopwatch()
      val result = fn
      if (stopwatch.elapsedMs() > timeMS) {
        log(level, s"$msg. Took ${stopwatch.elapsedString()}")
      }
      result
    }

    def infoIfLongerThan[A](msg: => String, timeMS: Int)(fn: => A): A = logIfLongerThan(Info, msg, timeMS)(fn)

    def logIfFutureLongerThan[A](level: Level, msg: => String, timeMS: Int)(fn: => Future[A]): Future[A] = {
      import scala.concurrent.ExecutionContext.Implicits.global
      val stopwatch = Stopwatch()
      val future = fn
      future.onComplete {
        t =>
          if (stopwatch.elapsedMs() > timeMS) {
            log(level, s"$msg took ${stopwatch.elapsedString()}." + (if (t.isFailure) " Result was failure." else ""))
          }
      }
      future
    }

    def logFutureTime[A](level: Level, msg: => String)(fn: => Future[A]): Future[A] = {
      logIfFutureLongerThan(level, msg, timeMS = -1)(fn)
    }

    def infoWithFutureTime[A](msg: => String)(fn: => Future[A]): Future[A] = {
      logFutureTime(Info, msg)(fn)
    }

    def infoIfFutureLongerThan[A](msg: => String, timeMS: Int)(fn: => Future[A]): Future[A] = {
      logIfFutureLongerThan(Info, msg, timeMS)(fn)
    }
    
    def warnIfFutureLongerThan[A](msg: => String, timeMS: Int)(fn: => Future[A]): Future[A] = {
      logIfFutureLongerThan(Warn, msg, timeMS)(fn)
    }
  }
}

object Log {
  // Can set a prefix for all log messages by setting -Dcom.topaz.logging.prefix=some_prefix
  val prefixKey: String = "com.topaz.logging.prefix"
  val prefix: String = Option(sys.props(prefixKey)).getOrElse("")
  
  sealed abstract class Level(val value: Int, val name: String) extends NamedIntEnumEntry

  object Level extends NamedIntEnum[Level] {
    case object Trace extends Level(1, "Trace")
    case object Debug extends Level(2, "Debug")
    case object Info extends Level(3, "Info")
    case object Warn extends Level(4, "Warn")
    case object Error extends Level(5, "Error")

    def values: immutable.IndexedSeq[Level] = findValues

    def fromLogbackLevel(level: ch.qos.logback.classic.Level): Level = {
      level match {
        case ch.qos.logback.classic.Level.TRACE => Trace
        case ch.qos.logback.classic.Level.DEBUG => Debug
        case ch.qos.logback.classic.Level.INFO => Info
        case ch.qos.logback.classic.Level.WARN => Warn
        case ch.qos.logback.classic.Level.ERROR => Error
      }
    }
  }
}

/***
  * This converter can be used in Logback patterns to deal with the case where we want to include environment
  * variables, but not spit out XXX_IS_UNDEFINED in the logs if the variable is not defined. Instead we can
  * provide a default, or if omitted default to the empty string.
  *
  * To use insert this line into the Logback file configuration section
  *
  * <conversionRule conversionWord="optenv" converterClass="com.topaz.utils.OptionalEnvironmentVariableConverter" />
  *
  * and then you can have logging patterns such as
  *
  *  <pattern>%optenv{JVM_NAME:-Unknown}</pattern>
  *
  *  or just
  *
  *  <pattern>%optenv{JVM_NAME}</pattern>
  *
  *  which will lookup the JVM_NAME environment variable, and if not defined, use the default if specified
  *  (i.e. "Unknown" in the first example)
  */
class OptionalEnvironmentVariableConverter extends ClassicConverter {
  // We are following the un-thread safe pattern here per
  // https://logback.qos.ch/xref/ch/qos/logback/classic/pattern/MDCConverter.html
  var envVarValue: String = ""

  override def start(): Unit = {
    val keyInfo = extractDefaultReplacement(getFirstOption)
    val maybeEnvVarValue = Option(keyInfo(0)).flatMap(varName => Option(sys.props(varName)))
    val maybeDefaultValue = Option(keyInfo(1))
    envVarValue = maybeEnvVarValue orElse maybeDefaultValue getOrElse ""

    super.start()
  }

  override def stop(): Unit = {
    envVarValue = ""
    super.stop()
  }

  def convert(event: ILoggingEvent): String = envVarValue
}
