package example

import org.scalatest._


class ScratchpadSuite extends FlatSpec with Matchers {

  import session._

  "An factorial function" should "return correct values" in {
    factorial(1) should be (1)
    factorial(2) should be (1 * 2)
    factorial(3) should be (1 * 2 * 3)
  }

  "An recursive implementatoin" should "return same values" in {
    val r = new scala.util.Random
    val randomInt = r.nextInt(10000)

    factorial(randomInt) should be (factorial2(randomInt))
  }


}
