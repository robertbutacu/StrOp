package stringOperations.operators

import stringOperations.operations._
import stringOperations.utils.Utils._
import stringOperations.utils.{Pos, StringNumber}


case class Positive(number: String = "0") extends StringNumber {
  require(number forall { p => p.isDigit })

  val n: String = number.slice(0, number.length - 1).dropWhile{d => d == '0' } + number.last

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

    if (isBigger(this, that))
      that match {
        case Positive(otherNumber) => Positive(Mod(this.n, otherNumber))
        case Negative(otherNumber) => Negative(Mod(this.n, otherNumber))
      }
    else
      that match {
        case Negative(_) => Negative(this.n)
        case _ => this
      }
  }


  def /(that: StringNumber): StringNumber = {
    require(that.number != "0")

    that match {
      case Positive(otherNumber) => Positive(Div(this.n, otherNumber))
      case Negative(otherNumber) => Negative(Div(this.n, otherNumber))
    }
  }


  def ++ : StringNumber = Positive(Inc(this.n))


  def -- : StringNumber =
    if(this.number == "0") Negative("1")
    else Pos(Dec(this.n))


  def ^(other: StringNumber): StringNumber = {
    require(other match { case Positive(_) => true; case _ => false})

    Positive(FastExp(this.n, other.number))
  }

  def square: StringNumber = Positive(Sq(this.n))
}
