package com.topaz.utils

import java.util.concurrent.atomic.AtomicReference

import akka.actor.Actor
import com.codahale.metrics.{Gauge, Metric}
import nl.grons.metrics.scala._

trait InstrumentedActor
  extends DefaultInstrumented with Actor with Instrumentation with RichConcurrent {
  private val gauges: AtomicReference[Seq[Gauge[_]]] = new AtomicReference(Seq())

  import scala.language.reflectiveCalls

  lazy val counter: Counter = metrics.counter("receiveCounter", self.path.name)
  lazy val timer: Timer = metrics.timer("receiveTimer", self.path.name)
  lazy val meter: Meter = metrics.meter("receiveExceptionMeter", self.path.name)

  def become(r: Receive): Unit = {
    context.become(
      counter.count(timer.timePF(meter.exceptionMarkerPF(r)))
    )
  }
  
  def settableGauge(name: String): SettableGauge = {
    val sg = metrics.settableGauge(name, self.path.name)
    gauges.getAndTransform(_ :+ sg)
    sg
  }

  private lazy val wrappedReceive = counter.count(timer.timePF(meter.exceptionMarkerPF(instrumentedReceive)))

  def receive: Receive = wrappedReceive

  def instrumentedReceive: Receive

  abstract override def preRestart(reason: Throwable, message: Option[Any]) = {
    // If the actor gets restarted we need to remove the gauges as only the names
    // have to be unique. A new one will be registered once the actor restarts.
    val toUnregister = gauges.getAndTransform(_ => Seq.empty)
    metricRegistry.removeMatching((_: String, metric: Metric) => 
      metric.isInstanceOf[Gauge[_]] && toUnregister.contains(metric)
    )
    super.preRestart(reason, message)
  }
}
