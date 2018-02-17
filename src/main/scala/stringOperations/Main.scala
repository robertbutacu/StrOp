package stringOperations

import stringOperations.operators.{Positive, StringNumber}
import stringOperations.utils.Implicits.numericToStringNumber
import stringOperations.utils.Utils._

object Main extends App {
  def go(input: StringNumber): Unit = {
    val newNumber = input * 2
    println(newNumber.numberOfDigits + "\t" + newNumber)
    go(newNumber)
  }

  //go(Positive("1"))
  println((Positive("2") to 1000).filter{sn: StringNumber => isPrime(sn)}.map{sn => sn * sn})
}
