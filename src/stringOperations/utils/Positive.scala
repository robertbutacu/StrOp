package stringOperations.utils

import stringOperations.operations._
import stringOperations.utils.Utils._


case class Positive(number: String = "0") extends StringNumber {
  require(number forall { p => p.isDigit })

  val n: String = number.slice(0, number.length - 1) dropWhile { _ == "0" } + number.last

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


  def -(that: StringNumber): StringNumber = {
    that match {
      case Positive(otherNumber) =>
        if(isBigger(this, that)) Positive(Sub(this.n, otherNumber))
        else Negative(Sub(otherNumber, this.n))
      case Negative(otherNumber) => Positive(Addi(this.n, otherNumber))
    }
  }


  def %(that: StringNumber): StringNumber = {
    require(that.number != "0")

    that match {
      case Positive(otherNumber) => Positive(Mod(this.n, otherNumber))
      case Negative(otherNumber) => Negative(Mod(this.n, otherNumber))
    }
  }


  def /(that: StringNumber): StringNumber = {
    require(that.number != "0")

    that match {
      case Positive(otherNumber) => Positive(Div(this.n, otherNumber))
      case Negative(otherNumber) => Negative(Div(this.n, otherNumber))
    }
  }


  def ++ : StringNumber = ???


  def -- : StringNumber = ???


  def ^(other: StringNumber): StringNumber = {
    require(other match { case Positive(n) => true; case _ => false})

    Positive(FastExp(this.n, other.number))
  }


  def sqrt: StringNumber = ???


  def square: StringNumber = Positive(Sq(this.n))
}
