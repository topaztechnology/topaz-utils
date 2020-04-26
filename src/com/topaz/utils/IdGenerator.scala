package com.topaz.utils

import java.util.concurrent.atomic.AtomicLong
import java.util.function.LongUnaryOperator

case class GeneratedId(id: Long) extends AnyVal

/**
  * The startSeq param is just so we can test the ids wrap around at maxSequence. It should probably be left
  * as the default of zero in non-test code.
  */
case class IdGenerator(uniquePart: Long, startSeq: Long = 0l) extends Log {

  import IdGenerator._

  val sequence = new AtomicLong(startSeq)

  require(uniquePart <= maxUniquePart && uniquePart >= 0, s"Unique part of IdGenerator is invalid: $uniquePart")

  log.debug(s"IdGenerator started with with unique part set to: $uniquePart")

  def nextId(): GeneratedId = {
    val seq = sequence.getAndUpdate((current: Long) => if (current == maxSequence)
      0l
    else
      current + 1)
    val newId = seq << uniquePartBits | uniquePart
    GeneratedId(newId)
  }
}

object IdGenerator {
  def fromJobNumber(jobNumber: Long): IdGenerator = {
    val truncated = jobNumber & maxUniquePart
    IdGenerator(truncated)
  }
  
  val uniquePartBits: Int = 32
  val maxUniquePart: Long = -1l ^ (-1l << uniquePartBits)
  val sequenceBits: Long = 63 - uniquePartBits
  val maxSequence: Long = -1l ^ (-1l << sequenceBits)
}
