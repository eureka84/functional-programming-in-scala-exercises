package chapter6

import chapter6.RNG.Rand

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a,b), rng2)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((currRand, accRand) => map2(currRand, accRand)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???

//  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng =>
//    fs.foldRight((Nil: List[A], rng))((currRand, accRand) => {
//      val (tail, prevRng) = accRand
//      val (head, newRng) = currRand(prevRng)
//      (head::tail, newRng)
//    })

  val int: Rand[Int] = _.nextInt

  def nonNegativeInt: Rand[Int] =
    map(int)(i => if (i < 0) -(i + 1) else i)
//  def nonNegativeInt: Rand[Int] = (rng: RNG) => {
//    val (nextInt, nextRNG) = rng.nextInt
//    val nextVal = if (nextInt < 0) -(nextInt + 1) else nextInt
//    (nextVal, nextRNG)
//  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double: Rand[Double] =
    map(nonNegativeInt)(i => if (i == Int.MaxValue) 0.0 else i.toDouble / Int.MaxValue)

//  def double: Rand[Double] = (rng: RNG) => {
//    val (nextInt, nextRNG) = nonNegativeInt(rng)
//    val nextVal  = if (nextInt == Int.MaxValue) 0.0 else nextInt.toDouble / Int.MaxValue
//    (nextVal , nextRNG)
//  }

    def intDouble: Rand[(Int,Double)] =
      both(nonNegativeInt, double)

//  def intDouble: Rand[(Int,Double)] = (rng: RNG) =>  {
//    val (nextInt, nextRng) = nonNegativeInt(rng)
//    val (nextDouble, finalRng) = double(nextRng)
//    ((nextInt, nextDouble), finalRng)
//  }

  def doubleInt: Rand[(Double,Int)] =
    both(double, nonNegativeInt)

//  def doubleInt: Rand[(Double,Int)] = (rng: RNG) => {
//    val (nextDouble, nextRng) = double(rng)
//    val (nextInt, finalRng) = nonNegativeInt(nextRng)
//    ((nextDouble, nextInt), finalRng)
//  }

  def double3: Rand[(Double,Double,Double)] = (rng: RNG) => {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(nonNegativeInt))
//  def ints(count: Int): Rand[List[Int]] = (rng: RNG) => {
//    @tailrec
//    def loop(n: Int, acc: List[Int], rng: RNG): (List[Int], RNG) = {
//      if (n ==0) (acc, rng)
//      else {
//        val (nextInt, nextRng) = rng.nextInt
//        loop(n -1, nextInt::acc, nextRng)
//      }
//    }
//    loop(count, Nil, rng)
//  }

}
