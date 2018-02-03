package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(row, col) + " ")
      println()
    }
  }

  /**
   Print any input (row, col) of pascal's triangle
        1
       1 1
      1 2 1
     1 3 3 1
    1 4 6 4 1
   */
  def pascal(row: Int, col: Int): Int = {
    if(row == 0 || col == 0)
      1
    else
      pascal(row - 1, col - 1) + pascal(row - 1, col)
  }
  
  /**
    Write a recursive function which verifies the balancing of parentheses in a string, which we represent as a
    List[Char] not a String.
   */
  def balance(chars: List[Char]): Boolean = {
    def f(chars: List[Char], num: Int): Boolean = {
      (chars, num) match {
        case (Nil, num) => return num == 0
        case (c :: tail, num) =>
          if (num < 0) return false
          if (c == '(') return f(tail, num + 1)
          if (c == ')') return f(tail, num - 1)
          else return f(tail, num)
      }
    }
    f(chars, 0)
  }
  
  /**
  Write a recursive function that counts how many different ways you can make change for an amount, given a list of coin
  denominations. For example, there are 3 ways to give change for 4 if you have coins with denomination 1 and 2:
  1+1+1+1, 1+1+2, 2+2.

  Do this exercise by implementing the countChange function inMain.scala. This function takes an amount to change, and a
  list of unique denominations for the coins.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(m: Int, c: List[Int]): Int = {
      if(c.isEmpty) 0
      else if (m - c.head == 0) 1
      else if (m - c.head < 0) 0
      else countChange(m - c.head, c) + countChange(m, c.tail)
    }
    count(money, coins.sorted)
  }
}
