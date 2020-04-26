package com.topaz.utils

import java.util.concurrent.PriorityBlockingQueue

import com.topaz.TopazCodingError

import scala.collection.breakOut
import scala.concurrent.{Future, Promise}
import scala.language.{higherKinds, implicitConversions}
import scala.reflect.ClassTag

trait CollectionUtils extends OptionPimpsMixin {
  def requireNoDups[T](t: Traversable[T])(msg: Iterable[T] => String): Unit = {
    val dups = t.duplicates
    if (dups.nonEmpty)
      throw new IllegalArgumentException(msg(dups))
  }

  def groupRanges(vs: Seq[Int]): Seq[Seq[Int]] = {
    if (vs.isEmpty)
      Seq()
    else
      vs.zip(vs.tail).foldLeft(List(List(vs.head))) {
        case (l, (a, b)) =>
          if (a + 1 == b)
            l.dropRight(1) :+ List(l.last.head, b)
          else
            l :+ List(b)
      }
  }

  /**
    * Given `case class Person(name: String, age: Int)` and `equals` is comparing ages
    * Groups
    * as = Seq(Person("a", 1), Person("b", 2)), bs = Seq(Person("c", 2), Person("d", 4)), into =
    * Seq(Some(Person("a", 1)) -> None, Some(Person("b", 2)) -> Some(Person("c", 2)), None -> Some(Person("d", 4)))
    */
  def groupPairs[T](as_ : Seq[T], bs_ : Seq[T])(equals: (T, T) => Boolean): Seq[(Option[T], Option[T])] = {
    var result = Vector.newBuilder[(Option[T], Option[T])]
    var as = as_
    var bs = bs_
    while(as.nonEmpty && bs.nonEmpty) {
      val a = as.head
      as = as.tail
      bs = bs.indexWhere(equals(a, _)) match {
        case -1 =>
          result += (Some(a) -> None)
          bs
        case i =>
          result += (Some(a) -> Some(bs(i)))
          bs.take(i) ++ bs.takeRight(bs.length - i - 1)
      }
    }
    as.foreach {
      a =>
        result += (Some(a) -> None)
    }
    bs.foreach {
      b =>
        result += (None -> Some(b))
    }
    result.result()
  }
  
  class CollectionPimps[T, C[T] <: Iterable[T]](ca: C[T]) {
    import collection.generic.CanBuildFrom
    def removeAt(n: Int)(implicit cbf: CanBuildFrom[C[T],T,C[T]]): C[T] = {
      val builder = cbf()

      ca.zipWithIndex.foreach {
        case (t, i) if i != n =>
          builder += t
        case _ =>
      }

      builder.result
    }
  }
  
  implicit def coll_pimp[A, C[A] <: Iterable[A]](ca: C[A]): CollectionPimps[A, C] = {
    new CollectionPimps[A,C](ca)
  }

  def setUnion[T](l: Set[_ <: T], r: Set[_ <: T]): Set[T] = {
    (l.toSeq ++ r.toSeq).toSet
  }
  def setDifference[T](l: Set[_ <: T], r: Set[_ <: T]): Set[T] = {
    l.toSeq.filterNot(r.toSet).toSet
  }

 def cartesianProduct[T](l: Seq[Seq[T]]): Seq[Seq[T]] = l match {
    case h +: t => for (xh <- h; xt <- cartesianProduct(t)) yield xh +: xt
    case Nil => Seq(Nil)
  }

  implicit class TraversableUnzip4[+A](collection: Traversable[A]) {
    def unzip4[A1, A2, A3, A4](implicit asTuple4: A => (A1, A2, A3, A4)): (IndexedSeq[A1], IndexedSeq[A2], IndexedSeq[A3], IndexedSeq[A4]) = {
      val b1 = IndexedSeq.newBuilder[A1]
      val b2 = IndexedSeq.newBuilder[A2]
      val b3 = IndexedSeq.newBuilder[A3]
      val b4 = IndexedSeq.newBuilder[A4]

      for (abcd <- collection) {
        val (a, b, c, d) = asTuple4(abcd)
        b1 += a
        b2 += b
        b3 += c
        b4 += d
      }
      (b1.result(), b2.result(), b3.result(), b4.result())
    }
  }

  def substituteAll[A](substituteMap: Map[(A, A), A], collection: Traversable[A]): Traversable[A] = {
    substituteMap.foldLeft(collection) {
      case (seq, (from, to)) =>
        seq.substitute(from, to)
    }
  }

  implicit class RichTraversable[A](collection: Traversable[A])   {
    def maybeMin(implicit ord: Ordering[A]): Option[A] = {
      if (collection.isEmpty)
        None
      else
        Some(collection.min)
    }

    def indexesWhere(p: A => Boolean): Seq[Int] = {
      collection.toSeq.zipWithIndex.collect { case (a, i) if p(a) => i }
    }

    /**
      * Substitute from with to. From can appear in any order and anywhere in the sequence.
      */
    def substitute(from: (A, A), to: A): Traversable[A] = {
      val (fromA, fromB) = from
      val bldr = Vector.newBuilder[A]
      var seenA = false
      var seenB = false

      collection.foreach {
        c =>
          if (fromA == c && !seenA)
            seenA = true
          else if (fromB == c && !seenB)
            seenB = true
          else
            bldr += c
      }

      if (seenA && seenB)
        bldr.result() :+ to
      else
        collection
    }

    def maybeMax(implicit ord: Ordering[A]): Option[A] = {
      if (collection.isEmpty)
        None
      else
        Some(collection.max)
    }
    def duplicates: Seq[A] = {
      collection.groupBy(identity).collect { case (x, Seq(_,_,_*)) => x }.toSeq
    }

    def filterOnType[T <: A](implicit t: ClassTag[T]): Vector[T] = {
      collection.filter(a => t.runtimeClass.isAssignableFrom(a.getClass)).map(_.asInstanceOf[T])(breakOut)
    }

    def containsType[T <: A](implicit t: ClassTag[T]): Boolean = {
      collection.exists(a => t.runtimeClass.isAssignableFrom(a.getClass))
    }

    def filterOnTypeIntoSet[T <: A](implicit t: ClassTag[T]): Set[T] = {
      filterOnType[T].toSet
    }

    def partitionOnType[T <: A](implicit t: ClassTag[T]): (Vector[T], Vector[A]) = {
      val tBuilder = Vector.newBuilder[T]
      val aBuilder = Vector.newBuilder[A]
      collection.foreach {
        case a if t.runtimeClass.isAssignableFrom(a.getClass) =>
          tBuilder += a.asInstanceOf[T]
        case a =>
          aBuilder += a
      }
      (tBuilder.result(), aBuilder.result())
    }

    def partitionOnTypeIntoSets[T <: A](implicit t: ClassTag[T]): (Set[T], Set[A]) = {
      val (vec1, vec2) = partitionOnType[T]
      (vec1.toSet, vec2.toSet)
    }

    def partitionOnTypes[T1 <: A, T2 <: A](implicit t1: ClassTag[T1], t2: ClassTag[T2]): (Vector[T1], Vector[T2]) = {
      val t1Builder = Vector.newBuilder[T1]
      val t2Builder = Vector.newBuilder[T2]
      collection.foreach {
        case a if t1.runtimeClass.isAssignableFrom(a.getClass) =>
          t1Builder += a.asInstanceOf[T1]
        case a if t2.runtimeClass.isAssignableFrom(a.getClass) =>
          t2Builder += a.asInstanceOf[T2]
        case o => throw TopazCodingError(s"Unexpected type, $o, ${o.getClass} - expected ${t1.runtimeClass} or ${t2.runtimeClass}")
      }
      (t1Builder.result(), t2Builder.result())
    }

    def partitionOnTypesIntoSets2[T1 <: A, T2 <: A](implicit t1: ClassTag[T1], t2: ClassTag[T2]): (Set[T1], Set[T2]) = {
      val (vec1, vec2) = partitionOnTypes[T1, T2]
      (vec1.toSet, vec2.toSet)
    }

    def partitionOnTypesIntoSets3[T1 <: A, T2 <: A, T3 <: A](implicit t1: ClassTag[T1], t2: ClassTag[T2], t3: ClassTag[T3]): (Set[T1], Set[T2], Set[T3]) = {
      val set1 = filterOnTypeIntoSet[T1]
      val (set2, set3) = collection.filterNot(set1.toSet).partitionOnTypesIntoSets2[T2, T3]
      (set1, set2, set3)
    }

    def partitionOnTypesIntoSets4[T1 <: A, T2 <: A, T3 <: A, T4 <: A](
      implicit t1: ClassTag[T1], t2: ClassTag[T2],
      t3: ClassTag[T3], t4: ClassTag[T4]
    ): (Set[T1], Set[T2], Set[T3], Set[T4]) = {
      val set1 = filterOnTypeIntoSet[T1]
      val (set2, set3, set4) = collection.filterNot(set1.toSet).partitionOnTypesIntoSets3[T2, T3, T4]
      (set1, set2, set3, set4)
    }

    def partitionOnTypesIntoSets5[T1 <: A, T2 <: A, T3 <: A, T4 <: A, T5 <: A](
      implicit t1: ClassTag[T1], t2: ClassTag[T2],
      t3: ClassTag[T3], t4: ClassTag[T4],
      t5: ClassTag[T5]
    ): (Set[T1], Set[T2], Set[T3], Set[T4], Set[T5]) = {
      val set1 = filterOnTypeIntoSet[T1]
      val (set2, set3, set4, set5) = collection.filterNot(set1.toSet).partitionOnTypesIntoSets4[T2, T3, T4, T5]
      (set1, set2, set3, set4, set5)
    }

    def partitionOnTypesIntoSets6[T1 <: A, T2 <: A, T3 <: A, T4 <: A, T5 <: A, T6 <: A](
      implicit t1: ClassTag[T1], t2: ClassTag[T2],
      t3: ClassTag[T3], t4: ClassTag[T4],
      t5: ClassTag[T5], t6: ClassTag[T6], 
    ): (Set[T1], Set[T2], Set[T3], Set[T4], Set[T5], Set[T6]) = {
      val set1 = filterOnTypeIntoSet[T1]
      val (set2, set3, set4, set5, set6) = collection.filterNot(set1.toSet).partitionOnTypesIntoSets5[T2, T3, T4, T5, T6]
      (set1, set2, set3, set4, set5, set6)
    }

    def maybeSingleElementOrThrow(): Option[A] = collection match {
      case Nil => None
      case Seq(a) => Some(a)
      case other => throw TopazCodingError(s"Expected collection to have 0 or 1 elements, has ${other.size}, ${other.take(2)}...")
    }
    def singleElement: A = {
      if (collection.size == 1)
        collection.head
      else
        throw TopazCodingError(s"Expected collection to have 1 element, has ${collection.size}, ${collection.take(2)}...")
    }
  }

  implicit class RichSeq[A](seq: Seq[A]) {
    def distinctBy[B](f: A => B): Seq[A] = seq.groupBy(f).map(_._2.head)(breakOut)

    def findAtMostOne(f: A => Boolean): Either[TopazFail, Option[A]] = {
      val res = seq.filter(f)
      if (res.size > 1)
        TopazFail("Found multiple entries when expecting only one")
      else
        Right(res.headOption)
    }

    def distinctHead: A = {
      val res = seq.distinct
      if (res.size > 1)
        throw new NoSuchElementException(s"Expected single distinct value but got multiple: ${res.mkString(", ")}")
      else
        res.head
    }

    def distinctHeadOption: Option[A] = {
      if (seq.isEmpty)
        None
      else
        Some(distinctHead)
    }

    def singleValue: Either[TopazFail, A] = {
      seq match {
        case Seq(a) => Right(a)
        case other => TopazFail(s"Expected single element, got $other")
      }
    }
    
    def emptyOrSingleValue[L](l: => L): Either[L, Option[A]] = {
      seq match {
        case Seq() => Right(None)
        case Seq(a) => Right(Some(a))
        case _ => Left(l)
      }
    }

    def cartesian: Seq[(A, A)] = for(a <- seq; b <- seq) yield (a, b)

    def toOption: Option[Seq[A]] = if(seq.isEmpty) None else Some(seq)

    /**
      * Unlike `Seq.lastIndexWhere`, this goes forward from the start point
      * Returns the index the last element of
      *   Seq.drop(startFrom).takeWhile(predicate)
      * would correspond to
      */
    def lastIndexWhile(predicate: A => Boolean, startFrom: Int): Int = {
      TopazCodingError.require(
        startFrom < seq.size,
        s"lastIndexWhere called on a Seq of size ${seq.size} starting from $startFrom"
      )
      seq.indexWhere(! predicate(_), startFrom) match {
        case -1 => seq.size - 1
        case n => n - 1
      }
    }
    def lastIndexWhile(predicate: A => Boolean): Int = {
      lastIndexWhile(predicate, 0)
    }
    def earliestIndexWhile(predicate: A => Boolean, startFrom: Int): Int = {
      var i = startFrom
      while(i >= 0 && predicate(seq(i))) {
        i -= 1
      }
      i + 1
    }
    def takeUptoFirstWhere(predicate: A => Boolean): Seq[A] = {
      seq.indexWhere(predicate) match {
        case -1 => seq
        case n => seq.take(n + 1)
      }
    }
    
    def forallAndNonEmpty(p: A => Boolean): Boolean = {
      seq.nonEmpty && seq.forall(p)
    }

    /**
      * Equivalence relation is an operator, ~, satisfying
      *
      * a ~ a
      * a ~ b <=> b ~ a
      * a ~ b and b ~ c => a ~ c
      *
      * See https://en.wikipedia.org/wiki/Equivalence_relation
      */
    def partitionByEquivalenceRelation(relation : (A, A) => Boolean): Seq[Seq[A]] = {
      var accumulator = Map[A, Seq[A]]()
      seq.foreach{ a =>
        accumulator.keys.find(relation(_, a)) match {
          case Some(k) =>
            accumulator = accumulator.updated(k, a +: accumulator(k))
          case None =>
            accumulator = accumulator.updated(a, Seq(a))
        }
      }
      accumulator.values.toSeq
    }
  }

  implicit class RichMap[K, V](map: Map[K, V]) {
    def +?(optPair: Option[(K, V)]): Map[K, V] = optPair.map(map + _).getOrElse(map)
    
    def getOrTopazFail(k: K, msg: => String): Either[TopazFail, V] = {
      map.get(k).toFail(msg)
    }

    def filterOnValueType[T <: V](implicit t: ClassTag[T]): Map[K, T] =
      map.filter { case (_, v) => t.runtimeClass.isAssignableFrom(v.getClass) }
        .map { case (k, v) => (k, v.asInstanceOf[T]) }
  }

  /**
    * For an ordered collection of T, you can use this to get a Future which will complete
    * once the element you are looking for, or a larger element, has been added.
    * After each update the method `afterUpdate` is called and we check if anyone is waiting for
    * that element.
    */
  trait WaitForOrderedEntry[T <: Ordered[T]] {

    def containsT(t: T): Future[T] = {
      if (latestT() >= Some(t)) {
        Future.successful(t)
      } else {
        val promise = Promise[T]()
        waiting.add(Entry(t, promise))
        checkWaiting(latestT())
        promise.future
      }
    }

    private case class Entry(t: T, p: Promise[T]) extends Comparable[Entry] {
      override def compareTo(o: Entry): Int = t.compareTo(o.t)

      override def toString: String = s"$p is waiting for $t"
    }
    private val waiting = new PriorityBlockingQueue[Entry]()

    /**
      * This needs to be called by the implementing class after each update
      */
    protected def afterUpdate(t: T): Unit = checkWaiting(Some(t))
    protected def latestT(): Option[T]

    private def checkWaiting(currentT: Option[T]): Unit = {
      @scala.annotation.tailrec
      def rec(): Unit = {
        Option(waiting.poll()) match {
          case Some(Entry(t, p)) if currentT >= Some(t) =>
            p.success(t)
            rec()
          case Some(e) => waiting.put(e)
          case _ =>
        }
      }
      rec()
    }
  }

}

object CollectionUtils extends CollectionUtils
