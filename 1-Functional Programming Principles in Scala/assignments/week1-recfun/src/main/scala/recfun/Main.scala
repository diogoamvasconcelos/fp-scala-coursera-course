package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(countChange(4, List(1, 2)))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (r == 0 || c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def BalanceCount(chars: List[Char]): Integer = {
      if (chars.isEmpty) 0 else {
        var fc = chars.head
        var value = if (fc == '(') 1 else if (fc == ')') -1 else 0
        var rest = BalanceCount(chars.tail)

        if (rest > 0) -1 else value + rest
      }
    }

    BalanceCount(chars) == 0
  }


  /**
    * Exercise 3
    */

  def countChange(money: Int, coins: List[Int]): Int =
  {
    if (coins.isEmpty || money <= 0)
      0
    else
    {
      var res_skip_coin = countChange(money, coins.tail)

      var left = money - coins.head
      if (left == 0)
      {
        res_skip_coin + 1
      }
      else
      {
        var res_use_coin_next_coin = 0 //countChange(left, coins.tail) no need to calculate this cases as they will be covered by res_skip_coin
        var res_use_coin_same_coin = countChange(left, coins)

        res_skip_coin + res_use_coin_next_coin + res_use_coin_same_coin
      }
    }
  }
}
