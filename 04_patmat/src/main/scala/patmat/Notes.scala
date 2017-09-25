package notes

object Session {


  trait Nat {
    def isZero: Boolean
    def predecessor: Nat

    def sucessor: Nat =
      new Sucessor(this)

    def + (other: Nat): Nat =
      if (isZero) other
      else predecessor + (new Sucessor(this))

    def - (other: Nat): Nat =
      if (other isZero) this
      else predecessor - (other predecessor)
  }

  object Zero extends Nat {
    def isZero = true
    def predecessor =
      throw new Error("0 does not heve predecessor")
  }

  class Sucessor(n: Nat) extends Nat {
    def isZero = false
    def predecessor =  n
  }



  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys =>
      if (x > y) y :: insert(x, ys)
      else x :: xs
  }
}
