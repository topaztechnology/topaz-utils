package com.topaz.utils

import java.util.concurrent.atomic.AtomicLong

import com.codahale.metrics.{Gauge, Metric}

class SettableGauge extends Metric with Gauge[Long] {
  private val value = new AtomicLong(0)

  def getValue: Long = value.get()
  
  def setValue(l: Long): Unit = {
    value.set(l)
  }
}
