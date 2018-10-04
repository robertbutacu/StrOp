package stringOperations.instances

import stringOperations.operators.{Positive, StringNumber}

import scala.language.postfixOps

object FractionalStringNumber {
  implicit def fractionalStringNumber: Fractional[StringNumber] = new Fractional[StringNumber] {
    val n = implicitly[Numeric[StringNumber]]

    override def div(x: StringNumber, y: StringNumber): StringNumber = x./(y)()

    override def plus(x: StringNumber, y: StringNumber): StringNumber = n.plus(x, y)

    override def minus(x: StringNumber, y: StringNumber): StringNumber = n.minus(x, y)

    override def times(x: StringNumber, y: StringNumber): StringNumber = n.times(x, y)

    override def negate(x: StringNumber): StringNumber = n.negate(x)

    override def fromInt(x: Int): StringNumber = n.fromInt(x)

    override def toInt(x: StringNumber): Int = n.toInt(x)

    override def toLong(x: StringNumber): Long = n.toLong(x)

    override def toFloat(x: StringNumber): Float = n.toFloat(x)

    override def toDouble(x: StringNumber): Double = n.toDouble(x)

    override def compare(x: StringNumber, y: StringNumber): Int = n.compare(x, y)
  }
}
