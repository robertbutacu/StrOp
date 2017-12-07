package stringOperations.utils

import stringOperations.operations._
import stringOperations.utils.Utils.isBigger

case class Negative(number: String = "0") extends StringNumber {
  require(number forall { e => e.isDigit })

  val n: String = number.slice(0, number.length - 1) dropWhile { _ == "0" } + number.last

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


  def -(that: StringNumber): StringNumber =
    that match {
      case Negative(otherNumber) =>
        if(isBigger(this, that)) Negative(Sub(this.n, otherNumber))
        else Positive(Sub(otherNumber, this.n))
      case Positive(otherNumber) =>
        if (isBigger(this, that)) Negative(Sub(this.n, otherNumber))
        else Positive(Sub(otherNumber, this.n))
    }


  def %(that: StringNumber): StringNumber = {
    require(that.number != "0")
    that match {
      case Negative(otherNumber) => Positive(Mod(this.n, otherNumber))
      case Positive(otherNumber) => Negative(Mod(this.n, otherNumber))
    }
  }


  def /(that: StringNumber): StringNumber = {
    require(that.number != "0")

    that match {
      case Negative(otherNumber) => Positive(Div(this.n, otherNumber))
      case Positive(otherNumber) => Negative(Div(this.n, otherNumber))
    }
  }


  def ++ : StringNumber =
    if(this.number == "1") Positive()
    else Negative(Dec(this.n))


  def -- : StringNumber = Negative(Inc(this.n))


  def ^(that: StringNumber) : StringNumber = {
    require(that match { case Positive(_) => true; case _ => false})

    if(Mod(that.number, "2") == "0") Positive(FastExp(this.n, that.number))
    else                             Negative(FastExp(this.n, that.number))
  }


  def square: StringNumber = Positive(Sq(this.n))
}
