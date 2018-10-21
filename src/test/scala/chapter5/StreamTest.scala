package chapter5

import org.scalatest.FunSuite
import Stream._

class StreamTest extends FunSuite {

  test("toList"){
    assert(Stream(1, 2, 3).toList == List(1,2,3))
    assert(empty.toList == Nil)
  }

  test("take n"){
    assert(Stream(1,2,3,4,5,6).take(3).toList == List(1, 2, 3))
    assert(Stream(1,2,3,4,5,6).take(7).toList == List(1, 2, 3, 4, 5, 6))
    assert(Stream().take(7).toList == Nil)
  }

  test("drop n"){
    assert(Stream(1,2,3,4,5,6).drop(3).toList == List(4, 5, 6))
    assert(Stream(1,2,3,4,5,6).drop(0).toList == List(1, 2, 3, 4, 5, 6))
    assert(Stream(1,2,3,4,5,6).drop(7).toList == Nil)
    assert(Stream().drop(7).toList == Nil)
  }

}
