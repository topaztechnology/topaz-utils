package com.topaz.event

import scala.concurrent.duration.FiniteDuration

trait Event

case object NoOp extends Event

trait EventPublisher {
  def publish(event: Event): Unit
}

object EventPublisher {
  val ignore: EventPublisher = _ => Unit
}

/**
  * Special Events used to notify the user
  */
trait Notification extends Event {
  def i18nText: String
  def hideDelay: Option[FiniteDuration]
  def extraText: Option[String]
}

trait ErrorNotification extends Notification {
  def error: Option[String]
  def extraText: Option[String] = None
  def hideDelay: Option[FiniteDuration] = None
}
