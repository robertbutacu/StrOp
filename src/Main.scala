import stringOperations.examples.StreamsExamples
import stringOperations.operators.{Negative, Positive, StringNumber}
import stringOperations.utils.Implicits.numericToStringNumber

object Main extends App with StreamsExamples{

  //TODO buggy division; speed up division/modulus
  println((Positive("0", "773") / Negative("5", "1"))(5))
  //println(squaresUpUntil(Positive("1000")).toList)
  println(primes take 20 toList)
}
