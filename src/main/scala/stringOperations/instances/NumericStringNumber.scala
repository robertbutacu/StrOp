package stringOperations.instances

import stringOperations.operators.StringNumber

object NumericStringNumber {
  implicit def numericStringNumber: Numeric[StringNumber] = new Numeric[StringNumber] {
    override def plus(x: StringNumber, y: StringNumber): StringNumber = ???

    override def minus(x: StringNumber, y: StringNumber): StringNumber = ???

    override def times(x: StringNumber, y: StringNumber): StringNumber = ???

    override def negate(x: StringNumber): StringNumber = ???

    override def fromInt(x: Int): StringNumber = ???

    override def toInt(x: StringNumber): Int = ???

    override def toLong(x: StringNumber): Long = ???

    override def toFloat(x: StringNumber): Float = ???

    override def toDouble(x: StringNumber): Double = ???

    override def compare(x: StringNumber, y: StringNumber): Int = ???
  }
}
