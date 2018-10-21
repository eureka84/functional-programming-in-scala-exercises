package chapter5

import org.scalatest.FunSuite
import Stream._

class StreamTest extends FunSuite {

  test("toList"){
    assert(cons(1, cons(2, cons(3, empty))).toList == List(1,2,3))
    assert(empty.toList == Nil)
  }

}
