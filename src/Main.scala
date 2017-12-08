import stringOperations.operators.Positive
import stringOperations.utils.Implicits.numericToStringNumber

object Main extends App{
  println(Positive("2", "2") + 3.4)
  println(2 + Positive("2"))
  println(Positive("1") to 10)
}
