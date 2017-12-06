import stringOperations.OperationFactory
import stringOperations.utils.{Pos, Positive, Pow}

object Main extends App with OperationFactory{
  println(compute(Some(Pos("123312351324123412341234")), Pow, Some(Pos("50"))).get().length)
  //Positive("asdas")
  //println(Pos("123123") to Pos("123128"))
}
