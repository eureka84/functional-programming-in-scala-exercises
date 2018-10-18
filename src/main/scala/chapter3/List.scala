package chapter3

import chapter3.IntList.zipSum

sealed trait List[+A] {

  import List._

  override def toString: String = this match {
    case Nil => "()"
    case Cons(_, _) => s"(${
      foldLeft(this, "")((acc, elem) =>
        if (acc.isEmpty) elem.toString
        else s"$acc, $elem"
      )
    })"
  }
}

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](xs: List[A], newHead: A): List[A] = xs match {
    case Nil => Cons(newHead, Nil)
    case Cons(_, t) => Cons(newHead, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else drop(tail(l), n - 1)

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) => if (f(h)) dropWhile(t)(f) else l
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, Cons(_, Nil)) => Cons(h, Nil)
    case Cons(h, t) => Cons(h, init(t))
  }


  //  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)
  def length[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  // Theoretically we could rewrite foldLeft in terms of foldRight like this
  // foldRight(reverse(as), z)((x, y) => f(y, x))


  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  //    as match {
  //      case Nil => z
  //      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  //    }


  def reverse[A](xs: List[A]): List[A] =
    foldLeft(xs, Nil: List[A])((tail, head) => Cons(head, tail))

  def append[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def flatten[A](l: List[List[A]]): List[A] =
    foldLeft(l, Nil: List[A])(append)

  def filter[A](as: List[A])(p: A => Boolean): List[A] =
    flatMap(as)(x => if (p(x)) List(x) else Nil)

  //    foldRight(as, Nil: List[A])((elem, acc) => if (p(elem)) Cons(elem, acc) else acc)

  def map[A, B](xs: List[A])(f: A => B): List[B] =
    foldRight(xs, Nil: List[B])((h: A, t: List[B]) => Cons(f(h), t))

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  def zipWith[A, B, C](xs: List[A], ys: List[B])(f: (A, B) => C): List[C] = xs match {
    case Nil => Nil
    case Cons(hx, tx) => ys match {
      case Nil => Nil
      case Cons(hy, ty) => Cons(f(hx, hy), zipWith(tx, ty)(f))
    }
  }

}

object IntList {

  import List._

  def sum(xs: List[Int]): Int = foldLeft(xs, 0)(_ + _)

  def product(xs: List[Int]): Int = foldLeft(xs, 1)(_ * _)

  def addOne(xs: List[Int]): List[Int] =
    map(xs)(_ + 1)

  def listToString(xs: List[Double]): List[String] =
    map(xs)(_.toString)

  def zipSum(xs: List[Int], ys: List[Int]): List[Int] =
    zipWith(xs, ys)(_ + _)
}
