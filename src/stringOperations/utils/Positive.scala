package stringOperations.utils

import stringOperations.operations._
import stringOperations.utils.Utils._


case class Positive(number: String = "0") extends StringNumber {
  require(number forall { p => p.toInt >= 0 && p.toInt <= 9 })

  val n = number

  def *(that: StringNumber): StringNumber =
    that match {
      case Positive(otherNumber) => Positive(Mul(this.n, otherNumber))
      case Negative(otherNumber) => Negative(Mul(this.n, otherNumber))
    }

  def +(that: StringNumber): StringNumber =
    that match {
      case Positive(otherNumber) => Positive(Addi(this.n, otherNumber))
      case Negative(otherNumber) =>
        if (isBigger(this, that)) Positive(Sub(this.n, otherNumber))
        else Negative(Sub(this.n, otherNumber))
    }

  def -(that: StringNumber): StringNumber = ???

  def %(that: StringNumber): StringNumber = ???

  def /(that: StringNumber): StringNumber = ???

  def ++ : StringNumber = ???

  def -- : StringNumber = ???

  def ^ : StringNumber = ???

  def sqrt : StringNumber = ???
}
