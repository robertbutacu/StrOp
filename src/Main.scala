import stringOperations.OperationFactory
import stringOperations.utils.{Pos, Pow}

object Main extends App with OperationFactory{
  println(compute(Some(Pos("123312351324123412341234")), Pow, Some(Pos("50"))).get().length)

}
