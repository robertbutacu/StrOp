package stringOperations.utils

import stringOperations.operations.{Addi, Mul, Sub}
import stringOperations.utils.Utils.isBigger

case class Negative(number: String = "0") extends StringNumber {
  require(number forall { e => e.toInt >= 0 && e.toInt <= 9} )

  val n = number

  def *(that: StringNumber): StringNumber =
    that match {
      case Positive(otherNumber) => Negative(Mul(this.n, otherNumber))
      case Negative(otherNumber) => Positive(Mul(this.n, otherNumber))
    }

  def +(that: StringNumber): StringNumber =
    that match {
      case Negative(otherNumber) => Negative(Addi(this.n, otherNumber))
      case Positive(otherNumber) =>
        if (isBigger(this, that)) Negative(Sub(this.n, otherNumber))
        else Positive(Sub(this.n, otherNumber))
    }

  def -(that: StringNumber): StringNumber = ???

  def %(that: StringNumber): StringNumber = ???

  def /(that: StringNumber): StringNumber = ???

  def ++ : StringNumber = ???

  def -- : StringNumber = ???

  def ^ : StringNumber = ???

  def sqrt : StringNumber = ???
}
