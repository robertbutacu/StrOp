import stringOperations.examples.StreamsExamples
import stringOperations.operators.{Negative, Positive}
import stringOperations.utils.Implicits.numericToStringNumber

object Main extends App with StreamsExamples {
  println(Positive("2", "271") * -5.3 * 2 * 2)
  println(Positive("2123412341123123123123", "2") * Positive("1", "12412312341241234"))
  println(Positive("20") >= 20)
  println(Negative("20") > 30)
  println(Positive("20") > -30)
  println(Positive("2") ^ 5000)
}
