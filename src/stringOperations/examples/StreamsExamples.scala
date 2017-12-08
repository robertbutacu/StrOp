package stringOperations.examples

import stringOperations.operators.{Positive, StringNumber}
import stringOperations.utils.Implicits.numericToStringNumber

/**
  * Created by Robert-PC on 9/25/2017.
  */

trait StreamsExamples{
  //TODO
  lazy val squares: Stream[StringNumber] = Stream.empty

  def squaresUpUntil(i: StringNumber): Stream[StringNumber] = {
    if (i == Positive())
      Stream.empty
    else
      Stream.cons(
        i,
        squaresUpUntil(
          i --)
        )
  }

  def squaresBetween(start: StringNumber, end: StringNumber): Stream[StringNumber] = {
    if (start == end)
      Stream.empty
    else
      Stream.cons(
        start square,
        squaresBetween(
          start ++, end
        )
      )
    }

  lazy val fibsString: Stream[StringNumber] = Positive() #::
    Positive("1") #::
    fibsString.zip(fibsString.tail).map { e =>
      e._1 + e._2
    }
}
