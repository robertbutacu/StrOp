package stringOperations.operators

import stringOperations.operations._
import stringOperations.utils.Utils.isBigger

case class Negative(integerPart: String = "0", fractionalPart: String = "0") extends StringNumber {
  require(integerPart.forall(_.isDigit) && fractionalPart.forall(_.isDigit))

  val n: String = integerPart.slice(0, integerPart.length - 1).dropWhile(_ == '0') + integerPart.last
  val m: String = fractionalPart

  override def *(that: StringNumber): StringNumber =
    that match {
      case Positive(i, f) => Negative(Mul(this.n, i))
      case Negative(i, f) => Positive(Mul(this.n, i))
    }


  override def +(that: StringNumber): StringNumber =
    that match {
      case Negative(i, f) => Negative(Addi(this.n, i))
      case Positive(i, f) =>
        if (isBigger(this, that)) Negative(Sub(this.n, i))
        else Positive(Sub(i, this.n))
    }


  override def -(that: StringNumber): StringNumber =
    that match {
      case Negative(i, f) =>
        if (isBigger(this, that)) Negative(Sub(this.n, i))
        else Positive(Sub(i, this.n))
      case Positive(i, f) =>
        if (isBigger(this, that)) Negative(Sub(this.n, i))
        else Positive(Sub(i, this.n))
    }


  override def %(that: StringNumber): StringNumber = {
    require(that.integerPart != "0")

    if (isBigger(this, that))
      that match {
        case Negative(i, f) => Positive(Mod(this.n, i))
        case Positive(i, f) => Negative(Mod(this.n, i))
      }
    else
      that match {
        case Negative(_, _) => Positive(this.n)
        case Positive(_, _) => this
      }
  }


  override def /(that: StringNumber): StringNumber = {
    require(that.integerPart != "0")

    that match {
      case Negative(i, f) => Positive(Div(this.n, i))
      case Positive(i, f) => Negative(Div(this.n, i))
    }
  }


  override def ++ : StringNumber =
    if (this.integerPart == "1") Positive()
    else Negative(Dec(this.n))


  override def -- : StringNumber = Negative(Inc(this.n))


  override def ^(that: StringNumber): StringNumber = {
    require(that match { case Positive(_, _) => true; case _ => false })

    if (Mod(that.integerPart, "2") == "0") Positive(FastExp(this.n, that.integerPart))
    else Negative(FastExp(this.n, that.integerPart))
  }


  override def square: StringNumber = Positive(Sq(this.n))

  override def ==(other: StringNumber): Boolean =
    other match {
      case _: Positive => false
      case _: Negative => this.n == other.integerPart && this.m == other.fractionalPart
    }
}
