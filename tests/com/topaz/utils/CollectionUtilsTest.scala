package com.topaz.utils

import java.util.concurrent.LinkedBlockingDeque

import com.topaz.TopazCodingError
import com.topaz.utils.CollectionUtils.WaitForOrderedEntry
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CollectionUtilsTest extends AnyFreeSpec with ScalaFutures with Matchers {
  case class A(i: Int) extends Ordered[A] {
    override def compare(that: A): Int = i.compare(that.i)
  }

  "Equivalence relation splitting" - {

    import CollectionUtils._
    val relation = (x: Int, y : Int) => (x - y) % 5 == 0

    "with empty seq" in {
      Nil.partitionByEquivalenceRelation(relation) shouldBe 'empty
    }

    "with Single element" in {
      Seq(3).partitionByEquivalenceRelation(relation) shouldBe Seq(Seq(3))
    }

    "General test" in {
      val seq = List(1, 2, 3, 4, 5, 5, 5, 6, 7)
      val partition = seq.partitionByEquivalenceRelation(relation).map(s => s.sorted)

      partition.size shouldBe 5
      partition.contains(Seq(1, 6)) should be (true)
      partition.contains(Seq(5, 5, 5)) should be (true)
      partition.contains(Seq(2, 7)) should be (true)
      partition.contains(Seq(3)) should be (true)
      partition.contains(Seq(4)) should be (true)

    }
  }

  "group into ranges" in {
    val seq = List(1, 2, 3, 4, 6, 8, 9, 10, 12)

    CollectionUtils.groupRanges(seq) shouldEqual List(List(1, 4), List(6), List(8, 10), List(12))
  }

  "substitute from map" in {
    val substitute = Map((1, 3) -> 13, (12, 6) -> 126, (12, 4) -> 125)

    val seq = List(1, 1, 2, 3, 4, 6, 8, 9, 10, 12)

    CollectionUtils.substituteAll(substitute, seq) shouldEqual List(1, 2, 4, 8, 9, 10, 13, 126)
  }

  "contains with completed future" in {
    class Test extends WaitForOrderedEntry[A] {
      val deque = new LinkedBlockingDeque[A]()
      override protected def latestT(): Option[A] = {
        Option(deque.peekLast())
      }
      def add(a: A) = {
        deque.addLast(a)
        afterUpdate(a)
      }
    }

    val test = new Test
    val c1 = test.containsT(A(1))
    val c2 = test.containsT(A(2))

    c1.isCompleted shouldBe false
    c2.isCompleted shouldBe false
    test.add(A(1))
    c1.futureValue shouldBe A(1)
    c2.isCompleted shouldBe false

    test.add(A(2))
    c2.futureValue shouldBe A(2)
  }

  "lastIndexWhile" in {
    import CollectionUtils._

    List(1, 2, 3).lastIndexWhile(_ < 3) should be (1)
    List(1, 3, 1, 3).lastIndexWhile(_ < 3, startFrom = 2) should be (2)
    List(1, 3, 1, 3).lastIndexWhile(_ > 0, startFrom = 2) should be (3)
    List(1, 3, 1, 3).lastIndexWhile(_ < 3) should be (0)

    intercept[TopazCodingError] {
      List[Int]().lastIndexWhile(_ => true)
    }
  }

  "cartesian product" in {
    import CollectionUtils._

    cartesianProduct(List(List(1, 2), List(3, 4), List(5, 6))) shouldEqual
      List(List(1, 3, 5), List(1, 3, 6), List(1, 4, 5), List(1, 4, 6), List(2, 3, 5), List(2, 3, 6), List(2, 4, 5), List(2, 4, 6))
  }

  "group pairs" in {
    import CollectionUtils._

    val test = Seq((
      Seq(1, 2, 3, 4), Seq(2, 4, 6, 8),
      Seq(Some(1) -> None, Some(2) -> Some(2), Some(3) -> None, Some(4) -> Some(4), None -> Some(6), None -> Some(8))
    ), (
      Seq(), Seq(2, 4),
      Seq(None -> Some(2), None -> Some(4))
    ), (
      Seq(1, 2), Seq(),
      Seq(Some(1) -> None, Some(2) -> None)
    ),(
      Seq(1, 2, 2), Seq(2),
      Seq(Some(1) -> None, Some(2) -> Some(2), Some(2) -> None)
    ),
    )

    test.foreach {
      case (a, b, expected) =>
        groupPairs(a, b)((x, y) => x == y) shouldEqual expected
    }
  }

  "group pairs uses correct comparison" in {
    import CollectionUtils._

    case class Person(name: String, age: Int)

    val test = Seq((
      Seq(Person("a", 1), Person("b", 2)), Seq(Person("c", 2), Person("d", 4)),
      Seq(Some(Person("a", 1)) -> None, Some(Person("b", 2)) -> Some(Person("c", 2)), None -> Some(Person("d", 4)))
    ),(
      Seq(Person("a", 1)), Seq(Person("b", 1)),
      Seq(Some(Person("a", 1)) -> Some(Person("b", 1)))
    ),(
      Seq(Person("a", 1), Person("aa", 1)), Seq(Person("b", 1)),
      Seq(Some(Person("a", 1)) -> Some(Person("b", 1)), Some(Person("aa", 1)) -> None)
      )
    )

    test.foreach {
      case (a, b, expected) =>
        groupPairs(a, b)((x, y) => x.age == y.age) shouldEqual expected
    }
  }
}
