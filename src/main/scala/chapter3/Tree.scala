package chapter3

sealed trait Tree[+A] {
  override def toString: String = this match {
    case Leaf(value) => s"($value)"
    case Branch(left, right) => s"[$left, $right]"
  }
}

case class Leaf[+A](value: A) extends Tree[A]

case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((l, r) => 1 + l + r)
//  t match {
//    case Leaf(_) => 1
//    case Branch(left, right) => 1 + size(left) + size(right)
//  }

  def depth[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((l, r) => 1 + (l max r))
  //  t match {
  //    case Leaf(_) => 0
  //    case Branch(left, right) => 1 + (depth(left) max depth(right))
  //  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)((x: A) => Leaf(f(x)): Tree[B])((leftTree, rightTree) => Branch(leftTree, rightTree))
  //  t match {
  //    case Leaf(value) => Leaf(f(value))
  //    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  //  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

}

object IntTrees {

  import Tree._

  def maximum(t: Tree[Int]): Int =
    fold(t)(identity)(_ max _)
  //  t match {
  //    case Leaf(value) => value
  //    case Branch(left, right) => maximum(left) max maximum(right)
  //  }

}
