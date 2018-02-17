package stringOperations.utils

import stringOperations.operators.{Negative, Positive, StringNumber}
import stringOperations.utils.Utils.{getFractionalPart, getIntegerPart}

object Implicits {
  implicit def numericToStringNumber[T: Numeric](i: T): StringNumber =
    i match {
      case a: Int =>
        if (a < 0) Negative(a.toString.tail)
        else Positive(a.toString)
      case a: Double =>
        if (a < 0) Negative(getIntegerPart(a.toString.tail), getFractionalPart(a.toString.tail))
        else Positive(getIntegerPart(a.toString), getFractionalPart(a.toString))
      case a: Float =>
        if (a < 0) Negative(getIntegerPart(a.toString.tail), getFractionalPart(a.toString.tail))
        else Positive(getIntegerPart(a.floor.toString), getFractionalPart((a.ceil - a).toString))
      case a: Short =>
        if (a < 0) Negative(a.toString.tail)
        else Positive(a.toString)
    }
}
