package recfun

import scala.collection.mutable.ListBuffer

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
    def pascal(c: Int, r: Int): Int = {
      if (c == 0) {
        1
      } else if (r == 0) {
        0
      } else {
        pascal(c, r-1) + pascal(c-1, r-1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balance2(chars: List[Char], stack:ListBuffer[Char]): Boolean = {
        if (chars.isEmpty) {
          stack.isEmpty
        } else if (chars.head == '(') {
          stack.insert(0, '(')
          balance2(chars.tail, stack)
        } else if (chars.head == ')') {
          if (!stack.isEmpty && balance2(chars.tail, stack.tail)) true else false
        } else {
          balance2(chars.tail, stack)
        }
      }
      balance2(chars, new ListBuffer[Char])
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countChange2(money: Int, coins: List[Int], i: Int): Int = {
        if (money < 0) {
          0
        } else if (money == 0) {
          1
        } else if (i == coins.length && money > 0) { // no coins left over!
          0
        } else {
          countChange2(money - coins(i), coins, i) + countChange2(money, coins, i + 1)

        }
      }
      countChange2(money, coins, 0)
    }
  }
