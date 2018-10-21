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
    assert(Stream().headOption == None)
    assert(Stream(1, 2, 3).headOption == Some(1))
  }
}
