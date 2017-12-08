import stringOperations.examples.StreamsExamples
import stringOperations.operators.Positive
import stringOperations.utils.Implicits.numericToStringNumber

object Main extends App with StreamsExamples{
  println(Positive("2", "2") + 3.4)
  println(2 + Positive("2"))
  println(Positive("00"))
  println(fibsString take 10 toList)
}
