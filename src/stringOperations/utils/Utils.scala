package stringOperations.utils

import stringOperations.operators.{Negative, Positive, StringNumber}

/**
  * Created by Robert-PC on 9/22/2017.
  */
case class Total(total: String = "", carry: Int = 0)

object Utils {
  def equalizeLengthIntegerPart(first: String, second: String): String = {
    if (first.length >= second.length)
      first
    else
      "0" * (second.length - first.length) ++ first
  }

  def equalizeLengthFractionalPart(first: String, second: String): String = {
    if (first.length >= second.length)
      first
    else
      first ++ ("0" * (second.length - first.length))
  }

  def isDivisorZero(x: StringNumber): Boolean = x.integerPart == "0" && x.fractionalPart == "0"

  def getIntegerPart(a: String): String = a.span(_ != '.')._1

  def getFractionalPart(a: String): String = a.span(_ != '.')._2.drop(1)

  def isBigger(x: String, y: String): Boolean =
    x.length > y.length || (x.length == y.length && x > y)

  /*
   x is bigger than y in 2 cases:
   1. integer part is bigger
   2. integer parts are equal, fractional part is bigger
   */
  def isBigger(x: StringNumber, y: StringNumber): Boolean = {
    def isIntegerPartBigger: Boolean =
      compare(x.integerPart, y.integerPart)

    def isFractionalPartBigger: Boolean =
      x.integerPart == y.integerPart && compare(x.fractionalPart, y.fractionalPart)

    def compare(a: String, b: String): Boolean =
      a.length > b.length || (a.length == b.length && a > b)

    if (isIntegerPartBigger || isFractionalPartBigger)
      true
    else
      false
  }

  def not(number: StringNumber): StringNumber =
    number match {
      case Positive(i, f) => Negative(i, f)
      case Negative(i, f) => Positive(i, f)
    }
}
