package com.topaz.utils

import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec

trait RichConcurrent {

  /**
    * From this blog posting
    * https://alexn.org/blog/2013/05/07/towards-better-atomicreference-scala.html
    */
  implicit class RichAtomicReference[T](r: AtomicReference[T]) {
    @tailrec
    final def transformAndGet(cb: T => T): T = {
      val oldValue = r.get
      val newValue = cb(oldValue)

      if (!r.compareAndSet(oldValue, newValue))
      // tail-recursive call
        transformAndGet(cb)
      else
        newValue
    }

    @tailrec
    final def getAndTransform(cb: T => T): T = {
      val oldValue = r.get
      val update = cb(oldValue)

      if (!r.compareAndSet(oldValue, update))
      // tail-recursive call
        getAndTransform(cb)
      else
        oldValue
    }
  }

}
