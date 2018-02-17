package operators

import org.scalatest.FlatSpec
import stringOperations.operators.{Negative, Positive}

class PositiveTests extends FlatSpec {
  "When given faulty inputs " should "fail requirements" in {
    assertThrows[IllegalArgumentException](Positive("123a"))
    assertThrows[IllegalArgumentException](Positive("-1"))
    assertThrows[IllegalArgumentException](Positive("123;"))
    assertThrows[IllegalArgumentException](Positive("123!"))
    assertThrows[IllegalArgumentException](Positive("123$"))
  }

  "When adding two positive numbers " should "return the sum of the numbers " in {
    assert(Positive("1") + Positive("2") === Positive("3"))
    assert(Positive("3", "9") + Positive("2", "1") === Positive("6"))
    assert(Positive("2", "5") + Positive("3") === Positive("5", "5"))
    assert(Positive() + Positive("3") === Positive("3"))
    assert(Positive("123124") + Positive("123124123") === Positive("123247247"))
  }

  "When adding a positive(bigger) and a negative number " should "return a positive number" in {
    assert(Positive("1") + Negative() === Positive("1"))
    //assert(Positive() + Negative() === Positive()) <= INVESTIGATE THIS
    assert(Positive("10") + Negative("5") === Positive("5"))
    assert(Positive("3", "3") + Negative("2") === Positive("1", "3"))
    assert(Positive("5", "9") + Negative("2", "3") === Positive("3", "6"))
    //assert(Positive() + Negative() === Positive())
    //assert(Positive() + Negative() === Positive())
  }

  "When adding a positive(smaller) and a negative number " should "return a negative number " in {
    //assert(Positive() + Negative() === Positive())
    assert(Positive("2") + Negative("3") === Negative("1"))
    //assert(Positive("5", "2") + Negative("6") === Negative("0", "8"))
    assert(Positive("1000") + Negative("1001") === Negative("1"))
    //assert(Positive("10", "8") + Negative("10", "9") === Negative("0", "1"))
  }

  "When subtracting 2 positive numbers" should "return the correct result" in {

  }

  "When subtracting a negative from a positive" should "return the sum of the 2" in {

  }

  "When multiplying 2 positive numbers" should "return the correct result" in {

  }

  "When multiplying a positive with a negative" should "Return a negative number" in {

  }

  "When dividing 2 positive numbers" should "return the division" in {

  }

  "When dividing a positive with a negative" should "return a negative division" in {

  }

  "When using modulus between 2 positives" should "return a positive" in {

  }

  "When using modulus between positive and negative" should "Return a negative" in {

  }

  "When using square " should "return square" in {

  }

  "When using exponentiation on a positive number" should "Return the correct result" in {

  }

  "When using inc " should "return positive + 1" in {

  }

  "When using dec " should "return positive - 1" in {

  }

  "When using sqrt on a positive" should "Return the sqrt" in {

  }
}
