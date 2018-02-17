package stringOperations.operations

import stringOperations.operators.{Positive, StringNumber}

/**
  * Created by Robert-PC on 9/25/2017.
  */
private[stringOperations] object Sqrt{
  def apply(x: String): Option[StringNumber] =
    Positive("1") to Positive(x) find ((i: StringNumber) => Square(i.integerPart) == x)
}
