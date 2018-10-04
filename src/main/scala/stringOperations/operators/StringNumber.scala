package stringOperations.operators

import stringOperations.operations.Increment

import scala.annotation.tailrec

/**
  * Created by Robert-PC on 9/21/2017.
  */

trait StringNumber extends Serializable {
  def integerPart: String

  def fractionalPart: String

  def negate: StringNumber = {
    this match {
      case Positive(i, f) => Negative(i, f)
      case Negative(i, f) => Positive(i, f)
    }
  }

  def +(other: StringNumber): StringNumber
  def -(other: StringNumber): StringNumber
  def %(other: StringNumber): StringNumber
  def /(other: StringNumber)(numberOfDecimalApproximation: Int = 5): StringNumber
  def ^(other: StringNumber): StringNumber
  def ++ : StringNumber
  def -- : StringNumber
  def square: StringNumber
  def *(other: StringNumber): StringNumber


  def ==(other: StringNumber): Boolean
  def >(other: StringNumber): Boolean
  def <(other: StringNumber): Boolean
  def >=(other: StringNumber): Boolean
  def <=(other: StringNumber): Boolean

  def numberOfDigits: Int = integerPart.length


  def to(another: StringNumber): List[StringNumber] =
    if( this < another) getList(this, another, List())
    else List(this)

  @tailrec
  private def getList(start: StringNumber, end: StringNumber, result: List[StringNumber]): List[StringNumber] = {
    if (start == end) result :+ start
    else getList(Positive(Increment(start.integerPart)), end, result :+ start)
  }

  protected[this] def cleanIntegerPart: String =
    integerPart.slice(0, integerPart.length - 1)
      .dropWhile { d => d == '0' } + integerPart.last

  protected[this] def cleanFractionalPart: String = {
    val reversed = fractionalPart.reverse
    if (reversed.length == 1)
      reversed
    else {
      reversed.dropWhile(_ == '0').reverse
    }
  }

  protected[this] def getFractionalLength(a: String): Int = a.length

  protected[this] def getFractionalPart(i: String, f1: String, f2: String): Int =
    i.length - getFractionalLength(f1) - getFractionalLength(f2)

  protected[this] def normalizeForIntegerPart(product: String, fractional: String, otherFractional: String): String =
    product.dropRight(getFractionalLength(fractional) + getFractionalLength(otherFractional))

  protected[this] def normalizeForFractionalPart(product: String, fractional: String, otherFractional: String): String =
    product.drop(getFractionalPart(product, fractional, otherFractional))

  protected[this] def join(a: String, b: String): String = a ++ b
}