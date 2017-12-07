package stringOperations.operators

import stringOperations.operations.Inc

import scala.annotation.tailrec

/**
  * Created by Robert-PC on 9/21/2017.
  */

trait StringNumber extends Serializable{
  def number: String

  def apply(): String = number

  def to(another: StringNumber): List[StringNumber] =  getList(this, another, List())

  @tailrec
  private def getList(start: StringNumber, end: StringNumber, result: List[StringNumber]): List[StringNumber] = {
    if(start == end) result :+ start
    else getList(Positive(Inc(start.number)), end, result :+ start)
  }
}