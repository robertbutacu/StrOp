package stringOperations.utils

import stringOperations.operations.Modulus
import stringOperations.operators.{Negative, Positive, StringNumber}

/**
  * Created by Robert-PC on 9/22/2017.
  */
case class Total(total: String = "", carry: Int = 0)

object Utils {
  def not(number: StringNumber): StringNumber =
    number match {
      case Positive(i, f) => Negative(i, f)
      case Negative(i, f) => Positive(i, f)
    }

  def max(one: StringNumber, two: StringNumber): StringNumber =
    if (isBigger(one, two)) one
    else two

  def min(one: StringNumber, two: StringNumber): StringNumber =
    if (isBigger(one, two)) two
    else one

  def isPrime(number: StringNumber): Boolean =
    if(number == Positive("2")) true
    else (Positive("2") to (number--)) forall { n => number % n != Positive()}

  private[stringOperations] def equalizeLengthIntegerPart(first: String, second: String): String =
    if (first.length >= second.length) first
    else "0" * (second.length - first.length) ++ first


  private[stringOperations] def equalizeLengthFractionalPart(first: String, second: String): String =
    if (first.length >= second.length) first
    else first ++ ("0" * (second.length - first.length))


  private[stringOperations] def isDivisorZero(x: StringNumber): Boolean = x.integerPart == "0" && x.fractionalPart == "0"

  private[stringOperations] def getIntegerPart(a: String): String = a.span(_ != '.')._1

  private[stringOperations] def getFractionalPart(a: String): String = a.span(_ != '.')._2.drop(1)

  private[stringOperations] def isBigger(x: String, y: String): Boolean =
    x.length > y.length || (x.length == y.length && x > y)

  /*
   x is bigger than y in 2 cases:
   1. integer part is bigger
   2. integer parts are equal, fractional part is bigger
   */
  private[stringOperations] def isBigger(x: StringNumber, y: StringNumber): Boolean = {
    def compare(a: String, b: String): Boolean =
      a.length > b.length || (a.length == b.length && a > b)

    def isIntegerPartBigger: Boolean =
      compare(x.integerPart, y.integerPart)

    def isFractionalPartBigger: Boolean =
      x.integerPart == y.integerPart && compare(x.fractionalPart, y.fractionalPart)

    if (isIntegerPartBigger || isFractionalPartBigger) true
    else false
  }
}
