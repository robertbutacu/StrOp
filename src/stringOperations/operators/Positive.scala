package stringOperations.operators

import stringOperations.operations._
import stringOperations.utils.Utils._


case class Positive(integerPart: String = "0", fractionalPart: String = "0") extends StringNumber {
  require(integerPart.forall { p => p.isDigit } && fractionalPart.forall(_.isDigit))

  val n: String = cleanIntegerPart
  val m: String = cleanFractionalPart

  override def *(that: StringNumber): StringNumber = {
    that match {
      case Positive(i, f) =>
        val product = Mul(join(this.n, this.m), join(i, f))

        Positive(normalizeForIntegerPart(product, this.m, f),
          normalizeForFractionalPart(product, this.m, f))

      case Negative(i, f) =>
        val product = Mul(join(this.n, this.m), join(i, f))

        Negative(normalizeForIntegerPart(product, this.m, f),
          normalizeForFractionalPart(product, this.m, f))
    }
  }


  override def +(that: StringNumber): StringNumber =
    that match {
      case Positive(i, f) => Positive(Addi(this.n, i), f)
      case Negative(i, f) =>
        if (this > that) Positive(Sub(this.n, i), f)
        else Negative(Sub(i, this.n), f)
    }


  override def -(that: StringNumber): StringNumber = {
    that match {
      case Positive(i, f) =>
        if (this > that) Positive(Sub(this.n, i), f)
        else Negative(Sub(i, this.n), f)
      case Negative(i, f) => Positive(Addi(this.n, i), f)
    }
  }


  override def %(that: StringNumber): StringNumber = {
    require(that.integerPart != "0")

    if (this > that)
      that match {
        case Positive(i, f) => Positive(Mod(this.n, i), f)
        case Negative(i, f) => Negative(Mod(this.n, i), f)
      }
    else
      that match {
        case _: Negative => Negative(this.n)
        case _           => this
      }
  }


  override def /(that: StringNumber): StringNumber = {
    require(that.integerPart != "0")

    that match {
      case Positive(i, f) => Positive(Div(this.n, i), f)
      case Negative(i, f) => Negative(Div(this.n, i), f)
    }
  }


  override def ++ : StringNumber = Positive(Inc(this.n), this.fractionalPart)


  override def -- : StringNumber =
    if (this.integerPart == "0") Negative("1", this.fractionalPart)
    else Positive(Dec(this.n), this.fractionalPart)


  override def ^(other: StringNumber): StringNumber = {
    require(other match { case Positive(_, _) => true; case _ => false })

    Positive(FastExp(this.n, other.integerPart), this.fractionalPart)
  }

  override def square: StringNumber = Positive(Sq(this.n))

  override def ==(other: StringNumber): Boolean =
    other match {
      case _: Positive => this.n == other.integerPart && this.m == other.fractionalPart
      case _: Negative => false
    }

  override def >(other: StringNumber): Boolean =
    other match {
      case _: Negative => true
      case Positive(i, f) => isBigger(this.n, i) || (this.n == i && isBigger(this.m, f))
    }

  override def <(other: StringNumber): Boolean =
    other match {
      case _: Negative => false
      case _: Positive => ! (this > other)
    }

  override def >=(other: StringNumber): Boolean  = this > other || this == other

  override def <=(other: StringNumber): Boolean = this < other || this == other

  override def toString = s"""Positive(${this.n}, ${this.m})"""
}
