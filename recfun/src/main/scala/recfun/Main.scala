package recfun

import scala.annotation.tailrec

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
      if(c == r) 1
      else if(c == 0) 1
      else pascal(c,  r - 1) + pascal(c - 1,  r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      @tailrec
      def balanceStack(chars: List[Char], currentStack: List[Char]): List[Char] = {
        if (chars.isEmpty) currentStack
        else if (chars.head == ')' && currentStack.nonEmpty && currentStack.head == '(') balanceStack(chars.tail, currentStack.tail)
        else if (chars.head == '(' || chars.head == ')') balanceStack(chars.tail, chars.head :: currentStack)
        else balanceStack(chars.tail, currentStack)
      }
      balanceStack(chars, List()).isEmpty
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) 0
      else if (money == 0) 1
      else if (money - coins.head >= 0) countChange(money - coins.head, coins) + countChange(money, coins.tail)
      else countChange(money, coins.tail)
    }
  }
