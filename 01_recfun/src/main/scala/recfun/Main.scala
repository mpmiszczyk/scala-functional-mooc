package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(column: Int, row: Int): Int = {
    if (column < 0)
      0
    else if (row < 0)
      0
    else if (column == 0 && row == 0)
      1
    else
      pascal(column-1, row-1) + pascal(column, row-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def iterBalanced(opened: Int, closed: Int, chars: List[Char]): Boolean =
      if (closed > opened)
        false
      else if (chars.isEmpty)
        (opened == closed)
      else {
        val char = chars.head
        iterBalanced(
          increaseIf(char, '(', opened),
          increaseIf(char, ')', closed),
          chars.tail)
    }

    def increaseIf(charA: Char, charB: Char, count: Int): Int =
      if (charA==charB) count + 1
      else count

    iterBalanced(0, 0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int ={
    if (money < 0) return 0
    if (money == 0) return 1  // found some combitaion of coins that sums up to money
    if (coins.isEmpty) return 0

    (countChange(money - coins.head, coins)
      + countChange(money, coins.tail))
  }
  // Not to bad implementation
  //
  // StyleChecer complains that I'm using `return` which I don't find
  // odd at all.  It's better that nesting `if else if`.  I could
  // pattern match, which would be cleanest and simplest solition
  //
  // But one thinkg I don't like now is how I have to describe `return
  // 1` line.
}
