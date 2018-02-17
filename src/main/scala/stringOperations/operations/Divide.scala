package stringOperations.operations

import scala.annotation.tailrec

/**
  * Created by Robert-PC on 9/21/2017.
  */

private[stringOperations] object Divide {
  def apply(x: String, y: String): String = {
    @tailrec
    def divide(x: String, y: String, quotient: String): String = {
      val x1 = Subtract(x.dropWhile(_ == '0'), y)

      if (isSmaller(x1, y))
        Add(quotient, "1")
      else
        divide(x1, y, Add(quotient, "1"))
    }

    if (y.equals("1")) x
    else if (isSmaller(x, y)) "0"
    else divide(x.dropWhile(_ == '0'), y.dropWhile(_ == '0'), "0")
  }

  private def isSmaller(x: String, y: String): Boolean = x.length < y.length || (x.length == y.length && x < y)
}
