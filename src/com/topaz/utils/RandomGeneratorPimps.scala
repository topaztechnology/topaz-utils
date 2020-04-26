package com.topaz.utils

import com.topaz.TopazCodingError
import org.apache.commons.math3.random.RandomGenerator

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.math.BigDecimal.RoundingMode

trait RandomGeneratorPimps {
  implicit class PimpedRandomGenerator(rng: RandomGenerator) {
    /**
     * Returns a value in the range
     *  [x1, x2] when called with two parameters
     *  [0, x1] when called with one
     */
    def uniform(x1: Double, x2: Double = Double.NaN): Double = {
      if (x2.isNaN) {
        TopazCodingError.require(x1 >= 0, f"Single value call to `uniform` requires a non-negative value")
        rng.nextDouble() * x1
      } else {
        TopazCodingError.require(x1 <= x2, f"Parameters ordered incorrectly")
        x1 + rng.nextDouble() * (x2 - x1)
      }
    }
    def nextThing[T](things: Seq[T]): T = things(rng.nextInt(things.size))
    def nextThing[T](things: Set[T]): T = nextThing(things.toSeq)
    def nextThing[T](thing: T, otherThings: T*): T = nextThing(thing +: otherThings)
    def isHeads(): Boolean = rng.nextDouble() < 0.5
    def nextPositiveInt(): Int = rng.nextInt(Int.MaxValue - 1) + 1
    def nextInt(from: Int, to: Int): Int = rng.nextInt(to - from) + from
    def shuffle[T](things: Traversable[T]): Vector[T] = {
      // https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#The_modern_algorithm
      val array = new ArrayBuffer[T] ++ things
      val N = array.size
      for (i <- N - 1 to 1 by -1) {
        val j = rng.nextInt(i + 1)
        val tmp = array(i)
        array(i) = array(j)
        array(j) = tmp
      }
      array.toVector
    }
    
    def seqOfThings[T](f: => T, howMany: Int): immutable.IndexedSeq[T] = {
      (0 until howMany).map(_ => f)
    }

    def nextThings[T](howMany: Int, things: Traversable[T]): Seq[T] = {
      val shuffled = rng.shuffle(things)
      shuffled.take(howMany)
    }
    
    def nextSubsequence[T](things: Traversable[T]): Seq[T] = {
      things.toSeq.filter(_ => rng.isHeads())
    }

    def nextNonEmptySubsequence[T](things: Traversable[T]): Seq[T] = {
      things.toSeq.filter(_ => rng.isHeads()) match {
        case Nil => Seq(rng.nextThing(things.toSeq))
        case o => o
      }
    }

    def maybe[T](thing: T): Option[T] = if (isHeads()) Some(thing) else None
    def maybeNextThing[T](things: Seq[T]): Option[T] = if (isHeads() && things.nonEmpty) Some(nextThing(things)) else None
    def maybeNextThing[T](thing: T, otherThings: T*): Option[T] = maybeNextThing(thing +: otherThings)
    def maybeNextNonNegativeLong(bound: Long): Option[Long] = if (isHeads()) Some(rng.nextLong().abs % bound) else None
    def nextString(len: Int, extraChars: Seq[Char] = Nil): String = {
      val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789".toCharArray.toSeq ++ extraChars
      Vector.fill(len)(rng.nextThing(chars)).mkString("")
    }

    def partition[T](things: Seq[T], nPartitions: Int): Seq[Seq[T]] = {
      if (nPartitions == 1)
        Seq(things)
      else {
        val N = things.size
        val nGaps = nPartitions - 1
        val gapPositions = shuffle(1 until N).take(nGaps).sorted

        val headSeq = things.take(gapPositions.head)
        val lastSeq = things.drop(gapPositions.last)
        val midSeqs = gapPositions.zip(gapPositions.tail).map {
          case (l, r) => things.drop(l).take(r - l)
        }
        headSeq +: (midSeqs :+ lastSeq)
      }
    }

    def nextDouble(min: Double, max: Double): Double = {
      min + rng.nextDouble() * (max - min)
    }

    def nextDoubleWithRounding(min: Double, max: Double, nDP: Int): Double = {
      val unrounded = nextDouble(min, max)
      BigDecimal(unrounded)
        .setScale(nDP, RoundingMode.HALF_UP)
        .doubleValue
    }
  }
}

object RandomGeneratorPimps extends RandomGeneratorPimps
