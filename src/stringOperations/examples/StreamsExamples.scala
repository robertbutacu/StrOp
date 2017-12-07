package stringOperations.examples

import stringOperations.utils._

/**
  * Created by Robert-PC on 9/25/2017.
  */

trait StreamsExamples{
  //TODO
  lazy val squares: Stream[StringNumber] = Stream.empty

  def squaresUpUntil(i: StringNumber): Stream[StringNumber] = ??? /* {
    if (i == Pos())
      Stream.empty
    else
      Stream.cons(
        i,
        squaresUpUntil(
          compute(Decrement, Some(i)).getOrElse(Pos())
        )
      )
  }*/

  def squaresBetween(start: StringNumber, end: StringNumber): Stream[StringNumber] = ??? /* {
    if (start == end)
      Stream.empty
    else
      Stream.cons(
        compute(Some(start), Multiply, Some(start)).get,
        squaresBetween(
          compute(Increment, Some(start)).get, end
        )
      )
    }

  lazy val fibsString: Stream[StringNumber] = Pos() #::
    Pos("1") #::
    fibsString.zip(fibsString.tail).map { e =>
      compute(
        Some(e._1),
        Add,
        Some(e._2)
      ).get
    }*/
}
