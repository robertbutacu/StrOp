package stringOperations.examples

import stringOperations.operations.Square
import stringOperations.operators.{Positive, StringNumber}
import stringOperations.utils.Utils._
import stringOperations.utils.Implicits.numericToStringNumber

/**
  * Created by Robert-PC on 9/25/2017.
  */

trait StreamsExamples{
  lazy val squares: Stream[StringNumber] = stringNumberStream.map(n => n * n)

  lazy val stringNumberStream: Stream[StringNumber] = Positive() #:: stringNumberStream.map(e => e++)

  lazy val primes: Stream[StringNumber] = Positive("2") #:: stringNumberStream.drop(2).filter(n => isPrime(n))

  lazy val fibsString: Stream[StringNumber] = Positive() #::
    Positive("1") #::
    fibsString zip fibsString.tail map { e => e._1 + e._2}

  def squaresUpUntil(i: StringNumber): Stream[StringNumber] =
    (Positive("1") to i).map(n => Positive(Square(n.integerPart))).toStream


  def squaresBetween(start: StringNumber, end: StringNumber): Stream[StringNumber] = {
    if ((start square) >= end)
      Stream.empty
    else
      Stream.cons(
        start square,
        squaresBetween(
          start ++, end
        )
      )
    }
}
