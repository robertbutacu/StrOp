package stringOperations.operators

import stringOperations.operations.Inc

import scala.annotation.tailrec

/**
  * Created by Robert-PC on 9/21/2017.
  */

trait StringNumber extends Serializable{
  def integerPart: String
  def fractionalPart: String


  def apply(): String = integerPart

  def +(other: StringNumber): StringNumber
  def -(other: StringNumber): StringNumber
  def %(other: StringNumber): StringNumber
  def /(other: StringNumber): StringNumber
  def ^(other: StringNumber): StringNumber
  def ++ : StringNumber
  def -- : StringNumber
  def square: StringNumber
  def *(other: StringNumber): StringNumber
  def ==(other: StringNumber): Boolean

  def to(another: StringNumber): List[StringNumber] =  getList(this, another, List())

  @tailrec
  private def getList(start: StringNumber, end: StringNumber, result: List[StringNumber]): List[StringNumber] = {
    if(start == end) result :+ start
    else getList(Positive(Inc(start.integerPart)), end, result :+ start)
  }
}