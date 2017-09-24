package example

object Notes{

  def sum(f: Int => Int, from: Int, to: Int) = {

    def loop(i: Int, acc: Int): Int  =
      if (i > to)
        acc
      else
        loop(i+1, acc + f(i))

    loop(from,0)
  }

  def sum_curried(f: Int=> Int): (Int, Int) => Int = {
    def loop(from: Int, to: Int): Int =
      if (from>to) 0
      else f(from) + loop(from+1, to)
    loop
  }

  def sum_anonymus_curried(f: Int => Int) (from: Int, to: Int): Int =
    if (from>to) 0
    else f(from) + sum_anonymus_curried(f)(from+1, to)

  def product(f: Int => Int)(from: Int, to: Int): Int =
    if (from>to) 0
    else f(from) * product(f)(from,to)

  def factorial(n: Int) = product((x: Int) => x)(1,n)

  def combine_with_series
    (combinator: (Int, Int) => Int,
      neutral_value: Int)
    (f: Int => Int)
    (from: Int, to: Int): Int =

    if (from > to) neutral_value
    else
      combinator(
        f(from),
        combine_with_series(combinator, neutral_value)(f)(from,to))

  def sum_from_general = combine_with_series(
    (x: Int, y: Int) => x+y,
    0
  )_


  def product_from_generator =
    combine_with_series((x, y) => x*y, 1)_

  def product2(f: Int => Int)(from: Int,to: Int) =
    combine_with_series((x, y) => x*y, 1)(f)(from, to)

}

object FixedPointNotes {
  import math.abs

  def fixedPoint(f: Double => Double, initial_guess: Double) : Double = {
    val tolarance = 1e-8

    def iter(guess: Double): Double = {
      val next = f(guess)
      if (closeEnough(guess, next))
        next
      else iter(next)
    }

    def closeEnough(a: Double, b: Double) =
      abs(a - b) < tolarance

    iter(initial_guess)
  }

  def averageDump(f: Double => Double)(x: Double) =
    (x + f(x)) / 2


  def sqrt(x: Double): Double =
    fixedPoint(averageDump(y => x/y), 1.0)
}



object RationalNubmersNotes {
  object Rational {
    def apply(x: Int, y: Int): Rational =
      new Rational(x, y)
  }

  class Rational(x: Int, y: Int)  {
    private def gcd(a: Int, b: Int): Int =
      if (b == 0) a
      else gcd(b, a % b)

    val numer: Int = x / gcd(x,y)
    val denom: Int = y / gcd(x,y)

    def + (other: Rational): Rational =
      Rational(
        (numer * other.denom + other.numer * denom),
        (denom * other.denom)
      )

    def unary_- =
      Rational( - (numer), denom)

    def - (other: Rational): Rational =
      this + (-other)

    override def equals(other: Any): Boolean = other match {
      case other: Rational =>
        (numer == other.numer && denom == other.denom)
      case _ => false
    }

    override def hashCode() =
      numer.hashCode + denom.hashCode

    def < (other: Rational): Boolean =
      (numer * other.denom) < (other.numer * denom)


    val strRepresantaion = f"Rational ${numer}/${denom}"

    override def toString() = strRepresantaion
  }

  def max(a: Rational, b: Rational): Rational =
    if (a < b) b
    else a
}
