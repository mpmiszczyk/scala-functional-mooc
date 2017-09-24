package example

import org.scalatest._


class RationalSpec extends FlatSpec with Matchers {
  import example.RationalNubmersNotes._

  "Two rationals" should "be comperable" in {
    assert( Rational(1,2) == Rational(1,2))
    assert( Rational(1,2) != Rational(1,4))
  }

  "Rational" should "be reduced to simplest form" in {
    assert( new Rational(1,2) == new Rational(2,4))
  }

  "You" should "be able to add two Rationals" in {
    Rational(1,3) + Rational(2,4) should be (Rational(5,6))
  }

  "You" should "be able to negate rational" in {
    -Rational(2,3) should be (Rational(-2,3))
  }

  "You" should "be able to subtract two rationals" in {
    Rational(3,5) - Rational(1,2) should be (Rational(1,10))
  }

  "You" should "be able to compare two rationlas" in {
    Rational(3,5) < Rational(1,2) should be (false)
    Rational(1,2) < Rational(3,5) should be (true)
    Rational(3,5) < Rational(3,5) should be (false)
  }

  "You" should "be able to find greater of two Rationals" in {
    max(Rational(2,3), Rational(3,5)) should be (Rational(2,3))
    max(Rational(3,5), Rational(2,3)) should be (Rational(2,3))
  }
}
