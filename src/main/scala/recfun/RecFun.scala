package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()


  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = 

    @tailrec
    def loop(column: Int, row: Int, acct: List[Int]): List[Int] =
      if(column == c && row == r) then 
        if(column == row) then acct.::(1)
        else if(column == 0) then acct.::(1)
        else acct.::(pascalSum(row, acct))
      else
        if(column == row) then loop(0, row+1, acct.::(1))
        else if(column == 0) then loop(column+1, row, acct.::(1))
        else loop(column+1, row, acct.::(pascalSum(row, acct)))

    def pascalSum(row: Int, pascal: List[Int]) = pascal.apply(row-1) + pascal.apply(row)

    if (c > r) then -1
    else loop(0,0,List()).head


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = 

    def checkParentheses(test: Char) =
      if(test == '(' || test == ')') then true
      else false

    def cancelParetheses(head: Char, current: Char) =
      if(head == '(' && current == ')') then true
      else false

    @tailrec
    def loop(chars: List[Char], acct: List[Char]): List[Char] =
      if(chars.isEmpty) then acct
      else
        if(acct.isEmpty) then 
          if(checkParentheses(chars.head)) then loop(chars.tail, acct.::(chars.head))
          else loop(chars.tail, acct)
        else
          if(checkParentheses(chars.head)) then
            if(cancelParetheses(acct.head, chars.head)) then loop(chars.tail, acct.tail)
            else loop(chars.tail, acct.::(chars.head))
          else 
            loop(chars.tail, acct)

    if(loop(chars, List()).isEmpty) then true
    else false

    
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = 

    def recur(money: Int, coins: List[Int]): Int =
      if(money < 0 || coins.isEmpty) then 0
      else if(money == 0) then 1
      else recur(money, coins.tail) + recur(money - coins.head, coins)

    recur(money, coins)
