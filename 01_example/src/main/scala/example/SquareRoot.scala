package example

object session {
  def sqrt(x: Double): Double =
    sqrtIter(1.0, x)

  def sqrtIter(guess: Double, x: Double) : Double =
    if (guessGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  def guessGoodEnough(guess: Double, x: Double): Boolean =
    abs(guess * guess - x) < (x * 0.000000001)

  def abs(x: Double): Double =
    if (x < 0) -x else x

  def improve(guess: Double, x: Double): Double =
    mean(guess, x / guess)

  def mean(x: Double, y: Double): Double =
    (x + y) / 2
}
