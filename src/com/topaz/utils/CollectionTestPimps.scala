package com.topaz.utils

import scala.language.implicitConversions

/**
  * TODO - move this to test code, but not as part of the
  * flat buffers PR, which is already huge
 */
trait CollectionTestPimps {

  implicit def toPimpedCollection[A](seq: Seq[A]) = new {
    def %(i: Int): A = seq(i % seq.size)
  }

}
