package chapter4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None
//  this match {
//    case None => None
//    case Some(v) => f(v)
//  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map(Some(_)) getOrElse ob
//  this match {
//    case None => ob
//    case _ => _
//  }

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
//  this match {
//    case None => None
//    case Some(v) => if (f(v)) Some(v) else None
//  }
}

case class Some[+A](v: A) extends Option[A]
case object None extends Option[Nothing]

