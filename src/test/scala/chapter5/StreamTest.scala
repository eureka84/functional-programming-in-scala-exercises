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
}
