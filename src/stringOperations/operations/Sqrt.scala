package stringOperations.operations

import stringOperations.operators.{Positive, StringNumber}

/**
  * Created by Robert-PC on 9/25/2017.
  */
object Sqrt{
  private[stringOperations] def apply(x: String) = Positive("1") to Positive(x) find ((i: StringNumber) => Sq(i()) == x)
}
