import stringOperations.operators.{Negative, Positive}
import stringOperations.utils.Implicits

object Main extends App with Implicits{
  println(Positive("2", "2") + 3.4)
  println(2 + Positive("2"))
  println(Negative("3") - 3)
}
