import stringOperations.examples.StreamsExamples
import stringOperations.operators.{Negative, Positive}
import stringOperations.utils.Implicits.numericToStringNumber

object Main extends App with StreamsExamples{
  //println(Positive("2", "9") - (-2.0000001))
  println((Negative("2", "92") / Negative("2","900011"))(10))
}
