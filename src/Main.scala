import stringOperations.examples.StreamsExamples
import stringOperations.operators.{Negative, Positive, StringNumber}
import stringOperations.utils.Implicits.numericToStringNumber

object Main extends App with StreamsExamples {
  def go(input: StringNumber): Unit = {
    val newNumber = input * 2
    println(newNumber.numberOfDigits + "\t" + newNumber)
    go(newNumber)
  }

  //go(Positive("1"))
  println(Positive("123123123") + 123456)
}
