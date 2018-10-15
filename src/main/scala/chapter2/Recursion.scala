package chapter2

/**
  * @author asciarra 
  */
object Recursion {

  def fib(n: Int): Int = {
    def loop(counter: Int, last: Int, secondToLast: Int): Int =
      if (counter == n) last
      else loop(counter + 1, last + secondToLast, last)

    if (n <= 1) n
    else loop(1, 1, 0)
  }

}
