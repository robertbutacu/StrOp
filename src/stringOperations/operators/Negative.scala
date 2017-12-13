package stringOperations.operators

import stringOperations.operations._
import stringOperations.utils.Utils._

case class Negative(integerPart: String = "0", fractionalPart: String = "0") extends StringNumber {
  require(integerPart.forall(_.isDigit) && fractionalPart.forall(_.isDigit))

  val n: String = cleanIntegerPart
  val m: String = cleanFractionalPart

  override def *(that: StringNumber): StringNumber =
    that match {
      case Positive(i, f) =>
        val product = Mul(join(this.n, this.m), join(i, f))

        Negative(normalizeForIntegerPart(product, this.m, f),
          normalizeForFractionalPart(product, this.m, f))

      case Negative(i, f) =>
        val product = Mul(join(this.n, this.m), join(i, f))

        Positive(normalizeForIntegerPart(product, this.m, f),
          normalizeForFractionalPart(product, this.m, f))
    }


  override def +(that: StringNumber): StringNumber =
    that match {
      case Negative(i, f) => Negative(Addi(this.n, i), f)
      case Positive(i, f) =>
        if (isBigger(this, that)) Negative(Sub(this.n, i), f)
        else Positive(Sub(i, this.n), f)
    }


  override def -(that: StringNumber): StringNumber =
    that match {
      case Negative(i, f) =>
        if (isBigger(this, that)) Negative(Sub(this.n, i), f)
        else Positive(Sub(i, this.n), f)
      case Positive(i, f) =>
        if (isBigger(this, that)) Negative(Sub(this.n, i), f)
        else Positive(Sub(i, this.n), f)
    }


  override def %(that: StringNumber): StringNumber = {
    require(that.integerPart != "0")

    if (isBigger(this, that))
      that match {
        case Negative(i, f) => Positive(Mod(this.n, i), f)
        case Positive(i, f) => Negative(Mod(this.n, i), f)
      }
    else
      that match {
        case Negative(_, _) => Positive(this.n, this.fractionalPart)
        case Positive(_, _) => this
      }
  }


  override def /(that: StringNumber): StringNumber = {
    require(that.integerPart != "0")

    that match {
      case Negative(i, f) => Positive(Div(this.n, i), f)
      case Positive(i, f) => Negative(Div(this.n, i), f)
    }
  }


  override def ++ : StringNumber =
    if (this.integerPart == "1") Positive()
    else Negative(Dec(this.n), this.m)

  override def -- : StringNumber = Negative(Inc(this.n), this.m)

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
}
