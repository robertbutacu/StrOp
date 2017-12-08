package stringOperations.operators

import stringOperations.operations._
import stringOperations.utils.Utils._


case class Positive(integerPart: String = "0", fractionalPart: String = "0") extends StringNumber {
  require(integerPart.forall{ p => p.isDigit } && fractionalPart.forall(_.isDigit))

  val n: String = integerPart.slice(0, integerPart.length - 1).dropWhile{d => d == '0' } + integerPart.last
  val m: String = fractionalPart

  def *(that: StringNumber): StringNumber =
    that match {
      case Positive(i, f) => Positive(Mul(this.n, i))
      case Negative(i, f) => Negative(Mul(this.n, i))
    }


  def +(that: StringNumber): StringNumber =
    that match {
      case Positive(i, f) => Positive(Addi(this.n, i), f)
      case Negative(i, f) =>
        if (isBigger(this, that)) Positive(Sub(this.n, i), f)
        else Negative(Sub(this.n, i), f)
    }


  def -(that: StringNumber): StringNumber = {
    that match {
      case Positive(i, f) =>
        if(isBigger(this, that)) Positive(Sub(this.n, i), f)
        else Negative(Sub(i, this.n), f)
      case Negative(i, f) => Positive(Addi(this.n, i), f)
    }
  }


  def %(that: StringNumber): StringNumber = {
    require(that.integerPart != "0")

    if (isBigger(this, that))
      that match {
        case Positive(i, f) => Positive(Mod(this.n, i), f)
        case Negative(i, f) => Negative(Mod(this.n, i), f)
      }
    else
      that match {
        case Negative(_, _) => Negative(this.n)
        case _ => this
      }
  }


  def /(that: StringNumber): StringNumber = {
    require(that.integerPart != "0")

    that match {
      case Positive(i, f) => Positive(Div(this.n, i), f)
      case Negative(i, f) => Negative(Div(this.n, i), f)
    }
  }


  def ++ : StringNumber = Positive(Inc(this.n), this.fractionalPart)


  def -- : StringNumber =
    if(this.integerPart == "0") Negative("1", this.fractionalPart)
    else Positive(Dec(this.n), this.fractionalPart)


  def ^(other: StringNumber): StringNumber = {
    require(other match { case Positive(_, _) => true; case _ => false})

    Positive(FastExp(this.n, other.integerPart), this.fractionalPart)
  }

  def +[T: Numeric](other: T): StringNumber = {
    def getIntegerPart(a: String): String = a.span(_ != '.')._1
    def getFractionalPart(a: String): String = a.span(_ != '.')._2.drop(1)

    other match {
      case a: Int =>
        if(a < 0) this + Negative(a.toString.drop(1))
        else this + Positive(a.toString)
      case a: Double =>
        if(a < 0) this + Negative(getIntegerPart(a.toString),  getFractionalPart(a.toString) + ".")
        else this + Positive(getIntegerPart(a.toString),  getFractionalPart(a.toString))
      case a: Float =>
        if(a < 0) this + Negative(getIntegerPart(a.toString), getFractionalPart(a.toString))
        else this + Positive(getIntegerPart(a.floor.toString),  getFractionalPart((a.ceil - a).toString))
      case a: Short =>
        if(a < 0) this + Negative(a.toString.drop(1))
        else this + Positive(a.toString)
      case a: String =>
        if(a.head == '-') this + Negative(a.drop(1))
        else this + Positive(a)
    }
  }


  def square: StringNumber = Positive(Sq(this.n))
}
