package scala_sequences

import org.scalatest.freespec.AnyFreeSpec
import scala_sequences.Evaluator.CoverResult

class SequenceCoverSpec extends AnyFreeSpec {
  val isTrue = AtmProp[Boolean](i => i)
  val isFalse = AtmProp[Boolean](i => !i)
  val isTwo = AtmProp[Int](i => i == 2)
  val isThree = AtmProp[Int](i => i == 3)
  val delay = Delay(1)

  "cover should capture start and stop points for a sequence" in {
    val trueThanFalse = Concat(isTrue, isFalse) // isTrue ##1 isFalse
    val trace = Seq(1, 0, 0, 1, 0, 0, 1).map(_ == 1)
    val result = Evaluator.cover(trueThanFalse, trace)
    assert(
      result == CoverResult(
        completed = Seq((0, 1), (3, 4)),
        pending = Seq(6)
      )
    )
  }

  "cover 2 (nested concat)" in {
    val twoTrueOneFalse = Concat(isTrue, Concat(isTrue, isFalse)) // isTrue ##1 isTrue ##1 isFalse
    val trace = Seq(1, 1, 1, 0, 1, 1, 0, 1, 1).map(_ == 1)
    val result = Evaluator.cover(twoTrueOneFalse, trace)
    assert(
      result == CoverResult(
        completed = Seq((1, 3), (4, 6)),
        pending = Seq(7, 8)
      )
    )
  }

  "cover 3 (nested concat with Int)" in {
    val twoWaitThanThree: ScalaSeq[Int] = Concat(isTwo, isThree) // isTwo ##1 isThree
    val trace = Seq(1, 2, 3, 2, 4, 3, 2, 2, 3, 2)
    val result = Evaluator.cover(twoWaitThanThree, trace)
    assert(
      result == CoverResult(
        completed = Seq((1, 2), (7, 8)),
        pending = Seq(9)
      )
    )
  }

  "cover 4 (repeated sequence)" in {
    val repeatTrueThrice = Fuse(Repeated(Fuse(isTrue, Delay(1)), 2), isTrue) // isTrue[*3]
    val trace = Seq(1, 1, 1, 0, 1, 1, 1, 1, 0, 1).map(_ == 1)
    val result = Evaluator.cover(repeatTrueThrice, trace)
    assert(
      result == CoverResult(
        completed = Seq((0, 2), (4, 6), (5, 7)),
        pending = Seq(9)
      )
    )
  }

  "Cover 5 (Or sequence)" in {
    val twoTrue = Concat(isTrue, isTrue)
    val twoFalse = Concat(isFalse, isFalse)
    val nextUnchanged = Or(twoTrue, twoFalse)
    val trace = Seq(1, 0, 1, 0, 0, 1, 0, 1, 1, 0).map(_ == 1)
    val result = Evaluator.cover(nextUnchanged, trace)
    assert(
      result == CoverResult(
        completed = Seq((3, 4), (7, 8)),
        pending = Seq(9)
      )
    )
  }

  "Cover 6 (A Implies B sequence)" in {
    val twoImpliesThree = Implies(Fuse(isTwo, delay), isThree)
    val trace1 = Seq(1, 2, 3, 4, 3, 2, 4, 3) // with none pending
    val result1 = Evaluator.cover(twoImpliesThree, trace1)
    assert(
      result1 == CoverResult(
        completed = Seq((1, 2)),
        pending = Seq()
      )
    )

    val trace2 = Seq(1, 2, 2, 3, 2, 3, 2) // with pending result
    val result2 = Evaluator.cover(twoImpliesThree, trace2)
    assert(
      result2 == CoverResult(
        completed = Seq((2, 3), (4, 5)),
        pending = Seq(6)
      )
    )
  }

  "Cover 7 (combined or and implies)" in {
    // val twoTrue = Concat(isTrue, isTrue)
    // val twoFalse = Concat(isFalse, isFalse)
    // val nextUnchanged = Or(twoTrue, twoFalse)
    // val twoImpliesUnchange = Implies(Fuse(isTwo, delay), nextUnchanged)
    // val trace = Seq(1, 2, )

    val isOne = AtmProp[Int](i => i == 1)
    val isZero = AtmProp[Int](i => i == 0)
    val twoOnes = Concat(isOne, isOne)
    val twoZeros = Concat(isZero, isZero)
    val double = Or(twoOnes, twoZeros)
    val twoImpliesDouble = Implies(Fuse(isTwo, delay), double)
    val trace = Seq(1, 2, 1, 1, 0, 2, 0, 1, 2, 0, 0, 3, 1)
    val result = Evaluator.cover(twoImpliesDouble, trace)
    assert(
      result == CoverResult(
        completed = Seq((1, 3), (8, 10)),
        pending = Seq()
      )
    )
  }
}
