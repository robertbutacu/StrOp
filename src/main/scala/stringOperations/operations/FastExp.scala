package stringOperations.operations

import scala.annotation.tailrec

private[stringOperations] object FastExp {
  def apply(x: String, y: String): String = {
    computePowers(x, toBits(y)).zip(toBits(y))
      .foldRight("1")((curr, acc) =>
        if (curr._2 == '1') Multiply(curr._1, acc)
        else acc
      )
  }

  @tailrec
  def generatePowersOf2(start: String, goal: String, result: List[String]): List[String] = {
    if (isBigger(start, goal)) result
    else if (Multiply(result.last, "2") == start) generatePowersOf2(Increment(start), goal, result ::: List(start))
    else generatePowersOf2(Increment(start), goal, result)
  }

  def toBits(input: String): List[Char] = {
    generatePowersOf2("1", input, List("1"))
      .foldRight("", input)((curr, acc) =>
        if (isBigger(Subtract(acc._2, curr), acc._2)) (acc._1 ++ "0", acc._2)
        else (acc._1 ++ "1", Subtract(acc._2, curr))
      )._1.toList
  }

  def computePowers(base: String, power: List[Char]): List[String] = {
    power.scanRight("1")((_, acc) =>
      if (acc == "1") base
      else Multiply(acc, acc)
    )
  }

  private def isBigger(x: String, y: String): Boolean = {
    if (x.length > y.length || (x.length == y.length && x > y)) true
    else false
  }
}
