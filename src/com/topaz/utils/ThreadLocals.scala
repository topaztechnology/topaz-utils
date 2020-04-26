package com.topaz.utils

/**
  * ThreadLocals are almost always the wrong answer to a problem.
  */
object ThreadLocals {

  private case class ThreadLocalVals(map: Map[String, Any]) {
    def +(nameAndValue: (String, Any)): ThreadLocalVals = copy(map + nameAndValue)

    def -(name: String): ThreadLocalVals = copy(map - name)

    def get(name: String): Option[Any] = map.get(name)
  }

  private val threadLocals = new ThreadLocal[ThreadLocalVals]() {
    override def initialValue(): ThreadLocalVals = ThreadLocalVals(Map.empty)
  }

  def withThreadLocal[R](name: String, value: Any)(f: => R): R = {
    try {
      val tl = threadLocals.get() + (name -> value)
      threadLocals.set(tl)
      f
    } finally {
      val tl = threadLocals.get() - name
      threadLocals.set(tl)
    }
  }

  def value(name: String): Option[Any] = threadLocals.get().get(name)
}
