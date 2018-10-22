package chapter5

import chapter5.Stream._

sealed trait Stream[+A] {

  def toListRecursive: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListRecursive
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  //  def take(n: Int): Stream[A] = this match {
  //    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
  //    case _ => empty
  //  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }
//  def takeWhile(p: A => Boolean): Stream[A] =
//    foldRight(empty[A])((elem, acc) => if (p(elem)) cons(elem, acc) else empty)

  //  def takeWhile(f: A => Boolean): Stream[A] = this match {
  //    case Cons(h, t) if f(h()) => cons(h(), t() takeWhile f)
  //    case _ => empty
  //  }

  def headOption: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

  //  def headOption: Option[A] = this match {
  //    case Empty => None
  //    case Cons(h, _) => Some(h())
  //  }

  def map[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  //  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = ???

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) map { case (nextValue, nextState) => cons(nextValue, unfold(nextState)(f)) } getOrElse empty[A]

  def constant[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  //  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))

  // def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  //  def fibs(): Stream[Int] = {
  //    def go(f0: Int, f1: Int): Stream[Int] =
  //      cons(f0, go(f1, f0 + f1))
  //
  //    go(0, 1)
  //  }
  //  def fibs(): Stream[Int] = from(0).map(fib)
  def fibs(): Stream[Int] =
    unfold((0, 1)) { case (prev, curr) => Some(prev, (curr, prev + curr)) }

}
