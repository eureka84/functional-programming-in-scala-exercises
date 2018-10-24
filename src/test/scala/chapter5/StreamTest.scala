package chapter5

import chapter5.Stream._
import org.scalatest.FunSuite

class StreamTest extends FunSuite {

  test("toList") {
    assert(Stream(1, 2, 3).toList == List(1, 2, 3))
    assert(empty.toList == Nil)
  }

  test("take n") {
    assert(Stream(1, 2, 3, 4, 5, 6).take(3).toList == List(1, 2, 3))
    assert(Stream(1, 2, 3, 4, 5, 6).take(7).toList == List(1, 2, 3, 4, 5, 6))
    assert(Stream().take(7).toList == Nil)
  }

  test("drop n") {
    assert(Stream(1, 2, 3, 4, 5, 6).drop(3).toList == List(4, 5, 6))
    assert(Stream(1, 2, 3, 4, 5, 6).drop(0).toList == List(1, 2, 3, 4, 5, 6))
    assert(Stream(1, 2, 3, 4, 5, 6).drop(7).toList == Nil)
    assert(Stream().drop(7).toList == Nil)
  }


  val isEven: Int => Boolean = (x: Int) => x % 2 == 0

  test("forAll") {
    val empty: Stream[Int] = Stream()

    assert(empty.forAll(isEven))
    assert(Stream(2, 4).forAll(isEven))
    assert(!Stream(2, 4, 5).forAll(isEven))
  }

  test("take while") {
    assert(Stream(2, 4, 5).takeWhile(isEven).toList == List(2, 4))
    assert(Stream().takeWhile(isEven).toList == Nil)
    assert(Stream(2, 4, 6).takeWhile(isEven).toList == List(2, 4, 6))
    assert(Stream(1, 2, 3).takeWhile(isEven).toList == Nil)
  }

  test("headOption") {
    assert(Stream().headOption.isEmpty)
    assert(Stream(1, 2, 3).headOption.contains(1))
  }

  test("map") {
    assert(empty[Int].map(_ * 2).toList == Nil)
    assert(Stream(1, 2, 3).map(_ * 2).toList == List(2, 4, 6))
  }

  test("filter") {
    assert(Stream(1, 2, 3).filter(_ % 2 == 1).toList == List(1, 3))
  }

  test("append") {
    assert(Stream(1, 2).append(Stream(3, 4)).append(Stream()).toList == List(1, 2, 3, 4))
  }

  test("flatMap") {
    val result: Stream[Int] = Stream(1, 2, 3).flatMap(i => Stream(i, i))
    assert(result.toList == List(1, 1, 2, 2, 3, 3))
  }

  test("constant") {
    assert(constant(2).take(3).toList == List(2, 2, 2))
    assert(constant(2).take(5).toList == List(2, 2, 2, 2, 2))
  }

  test("from") {
    assert(from(2).take(3).toList == List(2, 3, 4))
    assert(from(3).take(5).toList == List(3, 4, 5, 6, 7))
  }

  test("fibs") {
    assert(fibs().take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
  }

  test("zipAll") {
    assert(
      Stream(1, 2, 3).zipAll(Stream()).toList ==
        List(
          (Some(1), None),
          (Some(2), None),
          (Some(3), None)
        )
    )

    assert(
      Stream().zipAll(Stream(1, 2, 3)).toList ==
        List(
          (None, Some(1)),
          (None, Some(2)),
          (None, Some(3))
        )
    )
    assert(
      Stream(1, 2, 3).zipAll(Stream(1, 2, 3)).toList ==
        List(
          (Some(1), Some(1)),
          (Some(2), Some(2)),
          (Some(3), Some(3))
        )
    )
    assert(Stream().zipAll(Stream()).toList == Nil)
  }

  test("zipWith") {
    val sum: (Int, Int) => Int = _ + _

    assert(Stream(1, 2, 3).zipWith(empty[Int])(sum).toList == Nil)
    assert(empty[Int].zipWith(Stream(1, 2, 3))(sum).toList == Nil)
    assert(Stream(1, 2, 3).zipWith(Stream(1, 2))(sum).toList == List(2, 4))
    assert(Stream(1, 3).zipWith(Stream(1, 2, 3))(sum).toList == List(2, 5))
    assert(Stream(1, 2, 3).zipWith(Stream(4, 5, 6))(sum).toList == List(5, 7, 9))

  }

  test("startsWith") {
    assert(Stream(1, 2, 3).startsWith(Stream(1, 2)))
    assert(Stream(1, 2, 3).startsWith(Stream()))
    assert(Stream().startsWith(Stream()))
    assert(!Stream().startsWith(Stream(1, 2, 3)))
    assert(!Stream(12, 3, 4).startsWith(Stream(1, 2, 3)))
    assert(!Stream(1, 2, 3, 4).startsWith(Stream(1, 2, 4)))
  }

  test("tails") {
    val expected = Array(List(1, 2, 3), List(2, 3), List(3), List())
    Stream(1, 2, 3).tails.zipWith(from(0) take 4)((_, _)) map {
      case (stream, index) => assert(stream.toList == expected(index))
    }
  }

  test("exists") {
    assert(Stream(1, 2, 3).exists(_ % 3 == 0))
  }

  test("hasSubsequence") {
    assert(Stream(1, 2, 3).hasSubsequence(Stream(1, 2)))
    assert(!Stream(1, 2, 3).hasSubsequence(Stream(2, 1)))
  }

}
