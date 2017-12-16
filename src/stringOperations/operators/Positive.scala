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
        val product = Multiply(join(this.n, this.m), join(i, f))

        Positive(normalizeForIntegerPart(product, this.m, f),
          normalizeForFractionalPart(product, this.m, f))
      case _: Negative =>
        not(this * not(that))
    }
  }


  override def +(that: StringNumber): StringNumber =
    that match {
      case Positive(i, f) =>
        val fractionalPartsSum = Add(
          equalizeLengthFractionalPart(this.m, f),
          equalizeLengthFractionalPart(f, this.m)
        )

        val maxLengthFractionalPart = Math.max(this.m.length, f.length)
        val fractionalSumLength     = fractionalPartsSum.length

        //addition can only carry 1
        if(fractionalSumLength > maxLengthFractionalPart){
          val carry = fractionalPartsSum.head.toString
          Positive(Add(Add(this.n, carry), i), fractionalPartsSum.tail)
        }
        else Positive(Add(this.n, i), fractionalPartsSum)

      case _: Negative =>
        if (this > that) this - not(that)
        else that - this
    }


  override def -(that: StringNumber): StringNumber = {
    that match {
      case Positive(i, f) =>
        val finalFractionalDigits = Math.max(this.m.length, f.length)
        val thisNoFractional  = this.n ++ equalizeLengthFractionalPart(this.m, f)
        val otherNoFractional = i ++ equalizeLengthFractionalPart(f, this.m)

        if(isBigger(thisNoFractional, otherNoFractional)){
          val subtractionResult = Subtract(thisNoFractional, otherNoFractional)
          Positive(subtractionResult.take(subtractionResult.length - finalFractionalDigits),
            subtractionResult.takeRight(finalFractionalDigits))
        }
        else {
          val subtractionResult = Subtract(otherNoFractional, thisNoFractional)

          Negative(subtractionResult.take(subtractionResult.length - finalFractionalDigits),
            subtractionResult.takeRight(finalFractionalDigits))
        }
      case _: Negative => this + not(that)
    }
  }


  override def %(that: StringNumber): StringNumber = {
    require(that.integerPart != "0")
    that match {
      case Positive(i, f) => Positive(Modulus(this.n, i), f)
      case Negative(i, f) => Negative(Modulus(this.n, i), f)
    }
  }


  override def /(that: StringNumber)(numberOfDecimalApproximation: Int = 0): StringNumber = {
    require(that.integerPart != "0" || (that.integerPart == "0" && that.fractionalPart != "0"))

    that match {
      case Positive(i, f) =>
        val wholeDivisor = (i ++ f).dropWhile(_ == '0')
        val updatedDividendIntegerPart = this.n ++ this.m.slice(0, f.length)
        val updatedDividendFractionalPart = this.m.slice(f.length, this.m.length) ++
          ("0" * numberOfDecimalApproximation)

        //println(s"${updatedDividendIntegerPart} / ${wholeDivisor} \n" +
        //s"${updatedDividendIntegerPart} % ${wholeDivisor} ++ ${updatedDividendFractionalPart} / ${wholeDivisor}\n\n")
        Positive(Divide(updatedDividendIntegerPart, wholeDivisor),
          Divide(Modulus(updatedDividendIntegerPart, wholeDivisor) ++ updatedDividendFractionalPart, wholeDivisor))

      case _: Negative =>
        not((this / not(that))(numberOfDecimalApproximation))
    }
  }


  override def ++ : StringNumber = Positive(Increment(this.n), this.fractionalPart)


  override def -- : StringNumber =
    if (this.integerPart == "0") Negative("1", this.fractionalPart)
    else Positive(Decrement(this.n), this.fractionalPart)


  override def ^(other: StringNumber): StringNumber = {
    require(other match { case Positive(_, _) => true; case _ => false })

    Positive(FastExp(this.n, other.integerPart), this.fractionalPart)
  }

  override def square: StringNumber = Positive(Square(this.n))

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
