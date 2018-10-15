package chapter2

import chapter2.HOF._
import org.scalatest.FunSuite

/**
  * @author asciarra 
  */
class HOFTests extends FunSuite {

  val naturalOrder: (Int, Int) => Boolean = (a: Int, b: Int) => a < b

  test("sorted") {
    assert(isSorted(Array(1, 2, 3, 4, 5), naturalOrder))
    assert(!isSorted(Array(1, 4, 3, 5, 2), naturalOrder))
  }

  test("Empty Array is ordered") {
    assert(isSorted(Array(), naturalOrder))
  }

  test("Single element Array is ordered") {
    assert(isSorted(Array(1), naturalOrder))
  }
}
