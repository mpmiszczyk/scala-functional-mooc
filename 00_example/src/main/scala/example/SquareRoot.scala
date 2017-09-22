package example

object session {
  def sqrt(x: Double): Double = {

    def sqrtIter(guess: Double) : Double =
      if (guessGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def guessGoodEnough(guess: Double): Boolean =
      abs(guess * guess - x) < (x * 1e-8)

    def abs(n: Double): Double =
      if (n < 0) -n else n

    def improve(guess: Double): Double =
      mean(guess, x / guess)

    def mean(j: Double, k: Double): Double =
      (j + k) / 2

    sqrtIter(1.0)
  }

  def factorial(n: Int): Int =
    if (n == 1) n else n * factorial(n-1)

  def factorial2(n: Int): Int = {
    def tail_factorial(acc: Int, n: Int): Int =
      if (n == 1) acc
      else tail_factorial(acc * n, n-1)

    tail_factorial(1, n)
  }

}
