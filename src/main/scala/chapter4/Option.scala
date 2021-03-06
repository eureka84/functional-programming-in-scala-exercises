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
    this map (Some(_)) getOrElse ob

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

object Option {

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (valueA => b map (valueB => f(valueA, valueB)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)

//  a match {
//    case Nil => Some(Nil)
//    case hOpt :: tOpts => hOpt flatMap (h => sequence(tOpts) map (t => h :: t))
//  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => f(h) flatMap (h1 => traverse(t)(f) map (t1 => h1::t1))
  }
}

object Math {

  def mean(xs: Seq[Double]): Option[Double] = xs match {
    case Nil => None
    case _ => Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs)
      .flatMap(m =>
        mean(xs.map(x => math.pow(x - m, 2)))
      )

}
