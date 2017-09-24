package example

import org.scalatest._


class RationalSpec extends FlatSpec with Matchers {
  import example.RationalNubmersNotes._

  "Two rationals" should "be comperable" in {
    assert( new Rational(1,2) == new Rational(1,2))
    assert( new Rational(1,2) != new Rational(1,4))
  }

  "Rational" should "be reduced to simplest form" in {
    assert( new Rational(1,2) == new Rational(2,4))
  }
}
