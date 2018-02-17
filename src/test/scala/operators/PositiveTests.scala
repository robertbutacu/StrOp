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

}
