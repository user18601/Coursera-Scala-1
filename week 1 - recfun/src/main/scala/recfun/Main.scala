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
    def pascal(c: Int, r: Int): Int = {
      if (c == r || c == 0) 1 else pascal(c-1, r-1) + pascal(c,r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def check(chars: List[Char], counter: Int): Boolean = {
        val brackets = chars.filter(char => char == '(' || char == ')')
        if (counter < 0) {
          false
        } else if (brackets.isEmpty && counter == 0) {
          true
        } else {
          brackets.headOption.getOrElse(' ') match {
            case '(' => check(brackets.tail, counter + 1)
            case ')' => check(brackets.tail, counter - 1)
            case _ => false
          }
        }
      }
      check(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      val smallest = coins.sortWith(_ < _).headOption.getOrElse(-1)
      if (coins.isEmpty || money == 0 || smallest > money) {
        0
      } else if (smallest == money){
        1
      } else {
        countChange(money - smallest, coins) + countChange(money, coins.filter(_ != smallest))
      }
    }
  }
