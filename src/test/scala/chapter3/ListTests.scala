package chapter3

import chapter3.List._
import chapter3.IntList._
import org.scalatest.FunSuite

class ListTests extends FunSuite {

  test("List pattern matching") {
    val res = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    assert(res == 3)
  }

  test("tail of non empty list") {
    val xs = List(1, 2, 3)

    assert(tail(xs) == List(2, 3))
  }

  test("tail of an empty list") {
    val xs = Nil
    assert(tail(xs) == Nil)
  }

  test("set head") {
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

  test("drop n elements from m where m > n for which p holds") {
    val xs: List[Int] = List(2, 4, 6, 1, 2, 3)

    assert(dropWhile(xs)(x => (x % 2) == 0) == List(1, 2, 3))
  }

  test("drop all elements from xs because p holds for all of them") {
    val xs: List[Int] = List(2, 4, 6)

    assert(dropWhile(xs)(x => (x % 2) == 0) == Nil)
  }

  test("init of non empty list") {
    val xs: List[Int] = List(1, 2, 3, 4)

    assert(init(xs) == List(1, 2, 3))
  }

  test("init of empty list") {
    val xs: List[Int] = Nil

    assert(init(xs) == Nil)
  }

  test("init of singleton list") {
    val xs: List[Int] = List(1)

    assert(init(xs) == Nil)
  }

  test("foldRight") {
    assert(foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) == List(1, 2, 3))
  }

  test("length") {
    assert(length(List(1, 2, 3)) == 3)
  }
}
