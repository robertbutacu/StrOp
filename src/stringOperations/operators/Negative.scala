package stringOperations.operators

import stringOperations.operations._
import stringOperations.utils.Utils._

case class Negative(integerPart: String = "0", fractionalPart: String = "0") extends StringNumber {
  require(integerPart.forall(_.isDigit) && fractionalPart.forall(_.isDigit))

  val n: String = cleanIntegerPart
  val m: String = cleanFractionalPart

  override def *(that: StringNumber): StringNumber =
    that match {
      case _: Positive => that * this
      case _: Negative => not(this) * not(that)
    }


  override def +(that: StringNumber): StringNumber =
    that match {
      case _: Negative => not(this) + not(that)
      case o: Positive => o + this
    }


  override def -(that: StringNumber): StringNumber =
    that match {
      case _: Negative => not(that) + this
      case _: Positive => that + this
    }


  override def %(that: StringNumber): StringNumber = {
    require(that.integerPart != "0")

    if (this > that)
      that match {
        case Negative(i, f) => Positive(Modulus(this.n, i), f)
        case Positive(i, f) => Negative(Modulus(this.n, i), f)
      }
    else
      that match {
        case Negative(_, _) => Positive(this.n, this.fractionalPart)
        case Positive(_, _) => this
      }
  }


  override def /(that: StringNumber)(numberOfDecimalApproximation: Int = 0): StringNumber = {
    require(that.integerPart != "0")

    (not(this) / not(that))(numberOfDecimalApproximation)
  }


  override def ++ : StringNumber =
    if (this.integerPart == "1") Positive()
    else Negative(Decrement(this.n), this.m)

  override def -- : StringNumber = Negative(Increment(this.n), this.m)

  override def ^(that: StringNumber): StringNumber = {
    require(that match { case Positive(_, _) => true; case _ => false })

    if (Modulus(that.integerPart, "2") == "0") Positive(FastExp(this.n, that.integerPart))
    else Negative(FastExp(this.n, that.integerPart))
  }

  override def square: StringNumber = Positive(Square(this.n))

  override def ==(other: StringNumber): Boolean =
    other match {
      case _: Positive => false
      case _: Negative => this.n == other.integerPart && this.m == other.fractionalPart
    }

  override def >(other: StringNumber): Boolean =
    other match {
      case _: Positive    => false
      case Negative(i, f) => isBigger(i, this.n) || (this.n == i && isBigger(f, this.m))
    }

  override def <(other: StringNumber): Boolean =
    other match {
      case _: Positive => false
      case _: Negative => ! (this > other)
    }

  override def >=(other: StringNumber): Boolean = this > other || this == other

  override def <=(other: StringNumber): Boolean = this < other || this == other

  override def toString = s"""Negative(${this.n}, ${this.m})"""

  def apply(other: Positive) = Negative(other.integerPart, other.fractionalPart)
}
