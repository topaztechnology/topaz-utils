package com.topaz.utils

trait TupleUtils {

  implicit class Tuple4Zipped[A, B, C, D](val t: (Iterable[A], Iterable[B], Iterable[C], Iterable[D])) {
    def zipped: Seq[(A, B, C, D)] = {
      t._1.zip(t._2).zip(t._3).zip(t._4).map { case (((a, b), c), d) => (a, b, c, d) }(collection.breakOut)
    }
  }

}

object TupleUtils extends TupleUtils
