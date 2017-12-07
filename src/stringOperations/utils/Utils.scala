package stringOperations.utils

import stringOperations.operators.StringNumber

/**
  * Created by Robert-PC on 9/22/2017.
  */
case class Total(total: String = "", carry: Int = 0)

object Utils {
  def equalizeLength(first: String, second: String): String = {
    if (first.length >= second.length)
      first
    else
      "0" * (second.length - first.length) ++ first
  }

  def isDivisorZero(x: StringNumber): Boolean = x().dropWhile(_.equals('0')).isEmpty

  /*
  x is bigger than y in 2 cases:
    1. longer size
    2. same size, but, character for character, x is the first one to contain a bigger one.
 */

  def isBigger(x: StringNumber, y: StringNumber): Boolean = {
    if (x().length > y().length || (x().length == y().length && x() > y()))
      true
    else
      false
  }
}
