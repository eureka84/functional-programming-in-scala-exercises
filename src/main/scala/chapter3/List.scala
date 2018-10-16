package chapter3

import java.util.NoSuchElementException

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[A](head: A, tail: List[A]) extends List[A]

object List {

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => throw new NoSuchElementException
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

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}
