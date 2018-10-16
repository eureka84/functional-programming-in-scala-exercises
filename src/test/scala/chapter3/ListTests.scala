package chapter3

import java.util.NoSuchElementException

import org.scalatest.FunSuite
import chapter3.List._

class ListTests extends FunSuite {

  test("List pattern matching"){
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    assert(x == 3)
  }

  test("tail of non empty list") {
    val xs = List(1, 2, 3)

    assert(tail(xs) == List(2, 3))
  }

  test("tail of an empty list") {
    val xs = Nil
    assert(tail(xs) == Nil)
  }

  test("set head"){
    val xs = List(1, 2, 3)

    assert(setHead(xs, 4) == List(4, 2, 3))
  }


  test("drop n elem from xs of m length where m > n") {
    val xs = List(1, 2, 3, 4, 5, 6, 7, 8, 9)

    assert(drop(xs, 4) == List(5, 6, 7, 8, 9))
  }

  test("drop n elem from xs of m length where n > m") {
    val xs = List(1, 2)

    assert(drop(xs, 4) == Nil)
  }
}
