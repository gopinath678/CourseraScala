package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println("hello, world")
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1 else pascal(c-1, r-1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balanceHelper(chars: List[Char], count: Int): Boolean = {
    if (chars.isEmpty) { if (count == 0) true else false} else {
      if (chars.head != '(' && chars.head != ')') balanceHelper(chars.tail, count) else {
        if (chars.head == '(') balanceHelper(chars.tail, count + 1) else {
          if (count == 0) false else balanceHelper(chars.tail, count - 1)
        }
      }
    }
  }
  def balance(chars: List[Char]): Boolean = {
    balanceHelper(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1 else {
      if (money < 0 || coins.isEmpty) 0 else{
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      }
    }
  }
}
