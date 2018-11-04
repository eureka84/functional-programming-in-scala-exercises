package chapter7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  //  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
  //    (es: ExecutorService) => {
  //      val af = a(es)
  //      val bf = b(es)
  //      UnitFuture(f(af.get, bf.get))
  //    }


  def map[A, B](parA: Par[A])(f: A => B): Par[B] = map2(parA, unit(()))((a,_) => f(a))

//  def map[A, B](parA: Par[A])(f: A => B): Par[B] = es => {
//    val value: Future[A] = parA(es)
//    UnitFuture(f(value.get))
//  }

  /* This version respects timeouts. See `Map2Future` below. */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val (af, bf) = (a(es), b(es))
      Map2Future(af, bf, f)
    }

  /*
    Note: this implementation will not prevent repeated evaluation if multiple threads call `get` in parallel.
    We could prevent this using synchronization, but it isn't needed for our purposes here (also, repeated evaluation
    of pure values won't affect results).
  */
  case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {

    @volatile var cache: Option[C] = None

    def isDone: Boolean = cache.isDefined

    def isCancelled: Boolean = a.isCancelled || b.isCancelled

    def cancel(evenIfRunning: Boolean): Boolean =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

    def get: C = compute(Long.MaxValue)

    def get(timeout: Long, units: TimeUnit): C =
      compute(TimeUnit.NANOSECONDS.convert(timeout, units))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime;
        val aTime = stop - start
        val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call: A = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
//    es => UnitFuture(ps.map(par => par(es).get))
    ps.foldRight[Par[List[A]]](unit(List()))((parA, parOfListOfA) => map2(parA, parOfListOfA)(_ :: _))
}

object Examples {

  import Par._

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => map(lazyUnit(a))(f)

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] =
    fork(sequence(ps.map(asyncF(f))))
//  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = ps match {
//    case Nil => lazyUnit(Nil)
//    case x::xs => map2(lazyUnit(f(x)), fork(parMap(xs)(f)))(_ :: _)
//  }

}
