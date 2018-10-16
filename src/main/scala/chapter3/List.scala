package chapter3

import java.util.NoSuchElementException

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

  def sum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else drop(tail(l), n-1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }
}
