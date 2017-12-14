package stringOperations.operations

import stringOperations.utils.{Total, Utils}

/**
  * Created by Robert-PC on 9/21/2017.
  */

/*
    Instead of checking whether "borrowing" is needed when a subtraction between 2 digits is negative,
      it is proceeded by borrowing no matter what and subtracting the carry later on, if needed.

    Better explained on an example:
      20 - 9 => we zip it and get (2, 0), (0, 9).
      Proceeding to subtract the 2 members of each tuple, from right to left.

      Subtraction result: 0 becomes 10 => 10 - 9 => 1 -> thats the last unit of the subtraction, we keep it.
      Carry result : 0 - 9 = -9, which is negative -> we know we needed to borrow, so we keep that in mind for the next subtraction.

      2 we check for carry, we did indeed had a carry => 2 - 1 => 1
      1 - 0 = 1

      Result : 11.
 */

object Sub {
  private[stringOperations] def apply(x: String, y: String): String = {
    def compute(x: String, y: String): String = {
      x.zip(y)
        .foldRight(Total())((curr, acc) =>
          Total(
            subtract(curr._1, curr._2, acc.carry) ++ acc.total,
            carry(curr._1, curr._2, acc.carry)
          )
        ).total
    }

    if(x == y)
      "0"
    else
      compute(Utils.equalizeLengthIntegerPart(x, y), Utils.equalizeLengthIntegerPart(y, x))
  }

  private def subtract(x: Char, y: Char, carry: Int): String = {
    ((x.asDigit - y.asDigit - carry + 10) % 10).toString
  }

  private def carry(x: Char, y: Char, carry: Int): Int = {
    if (x.asDigit - y.asDigit - carry >= 0)
      (x.asDigit - y.asDigit - carry) / 10
    else
      1
  }
}
