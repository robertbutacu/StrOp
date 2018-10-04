package stringOperations.instances

import javax.management.BadStringOperationException
import stringOperations.operators.{Positive, StringNumber}

object NumericStringNumber {
  implicit def numericStringNumber: Numeric[StringNumber] = new Numeric[StringNumber] {

    override def plus(x: StringNumber, y: StringNumber): StringNumber = x + y

    override def minus(x: StringNumber, y: StringNumber): StringNumber = x - y

    override def times(x: StringNumber, y: StringNumber): StringNumber = x * y

    override def negate(x: StringNumber): StringNumber = x negate

    override def fromInt(x: Int): StringNumber = Positive(x.toString)

    override def toInt(x: StringNumber): Int =
    if(x.integerPart.length > Int.MaxValue.toString.length)
      throw new BadStringOperationException("StringNumber is too big!")
    else x.integerPart.toInt

    override def toLong(x: StringNumber): Long =
      if(x.integerPart.length > Long.MaxValue.toString.length)
        throw new BadStringOperationException("StringNumber is too big!")
      else x.integerPart.toInt

    override def toFloat(x: StringNumber): Float =
      if(x.integerPart.length > Float.MaxValue.toString.length)
        throw new BadStringOperationException("StringNumber is too big!")
      else x.integerPart.toInt

    override def toDouble(x: StringNumber): Double =
      if(x.integerPart.length > Double.MaxValue.toString.length)
        throw new BadStringOperationException("StringNumber is too big!")
      else x.integerPart.toInt


    override def compare(x: StringNumber, y: StringNumber): Int = if(x == y) 0 else if(x > y) 1 else -1
  }
}
