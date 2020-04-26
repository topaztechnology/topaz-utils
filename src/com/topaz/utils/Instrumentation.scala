package com.topaz.utils

import com.codahale.metrics.Metric
import nl.grons.metrics.scala.MetricBuilder

trait Instrumentation extends Log {
  implicit class PimpedMetricBuilder(m: MetricBuilder) {
    def settableGauge(name: String, scope: String = null): SettableGauge = {
      val sg = new SettableGauge()
      registerScoped(name, sg, scope)
      sg
    }

    def registerScoped(name: String, metric: Metric, scope: String = null): Unit = {
      val metricName = metricNameFor(name, scope)
      try {
        m.registry.register(metricName, metric)
      } catch {
        case e: IllegalArgumentException if e.toString.contains("already exists") =>
          // don't rethrow. failing to register the metric is annoying, but killing the
          // actor is far worse.
          // Also this can happen a lot in tests as we create instrumented actors over and over.
          log.warn(s"Failed to register '$metricName'")
      }
    }

    def metricNameFor(name: String, scope: String = null): String =
      m.baseName.append(name, scope).name
  }
}
