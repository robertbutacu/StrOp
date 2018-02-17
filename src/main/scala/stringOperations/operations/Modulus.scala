package stringOperations.operations

import scala.annotation.tailrec

/**
  * Created by Robert-PC on 9/25/2017.
  */
private[stringOperations] object Modulus {
  def apply(x: String, y: String): String = {
    @tailrec
    def mod(x: String, y: String): String = {
      val x1 = Subtract(x.dropWhile(_ == '0'), y)

      if (isSmaller(x1, y)) x1
      else mod(x1, y)
    }

    if (y.equals("1")) x
    else if (isSmaller(x, y)) x
    else mod(x.dropWhile(_ == '0'), y.dropWhile(_ == '0'))
  }

  private def isSmaller(x: String, y: String): Boolean = x.length < y.length || (x.length == y.length && x < y)
}
