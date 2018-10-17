package chapter3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[A](head: A, tail: List[A]) extends List[A]

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

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  //  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)
  def length[A](as: List[A]): Int = foldLeft(as, 0)((acc, _) => acc + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def reverse[A](xs: List[A]): List[A] = foldLeft(xs, Nil: List[A])((tail, head) => Cons(head, tail))

}

object IntList {

  import List._

  def sum(xs: List[Int]): Int = foldLeft(xs, 0)(_ + _)

  def product(xs: List[Int]): Int = foldLeft(xs, 1)(_ * _)
}
