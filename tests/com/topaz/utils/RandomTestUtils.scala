package com.topaz.utils

trait RandomTestUtils {
  protected def untilDifferent[T](t1: T, next: () => T): T = {
    satisfying[T](next, t2 => t1 != t2)
  }

  protected def satisfying[T](next: () => T, predicate: T => Boolean, msg: String = ""): T = {
    var numTries = 0
    var thing = next()
    var satisfied = false
    while (numTries < 100 && !satisfied) {
      if (predicate(thing))
        satisfied = true
      else {
        numTries += 1
        thing = next()
      }
    }

    require(satisfied, s"${msg}Couldn't pass predicate")
    thing
  }
}
