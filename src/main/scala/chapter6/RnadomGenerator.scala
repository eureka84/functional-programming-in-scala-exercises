package chapter6

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

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextInt, nextRNG) = rng.nextInt
    val nextVal = if (nextInt < 0) -(nextInt + 1) else nextInt
    (nextVal, nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (nextInt, nextRNG) = nonNegativeInt(rng)
    val nextVal  = if (nextInt == Int.MaxValue) 0.0 else nextInt.toDouble / Int.MaxValue
    (nextVal , nextRNG)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (nextInt, nextRng) = nonNegativeInt(rng)
    val (nextDouble, finalRng) = double(nextRng)
    ((nextInt, nextDouble), finalRng)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (nextDouble, nextRng) = double(rng)
    val (nextInt, finalRng) = nonNegativeInt(nextRng)
    ((nextDouble, nextInt), finalRng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(n: Int, acc: List[Int], rng: RNG): (List[Int], RNG) = {
      if (n ==0) (acc, rng)
      else {
        val (nextInt, nextRng) = rng.nextInt
        loop(n -1, nextInt::acc, nextRng)
      }
    }
    loop(count, Nil, rng)
  }

}
