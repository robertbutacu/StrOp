import stringOperations.operators.{Positive, StringNumber}

object Main extends App{
  implicit def doubleToPositive[T: Numeric](i: T): Positive = Positive(i.toString.span(_ != '.')._1)
  println(Positive("2", "2") + 3.4)
  println(3.0 + Positive("1"))
}
