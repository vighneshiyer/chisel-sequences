package scala_sequences

import org.scalatest.freespec.AnyFreeSpec
import scala_sequences.Evaluator.AssertResult

class SequenceAssertSpec extends AnyFreeSpec {
    val isTrue = AtmProp[Boolean](i => i)
    val isFalse = AtmProp[Boolean](i => !i)
    val isTwo = AtmProp[Int](i => i == 2)
    val isThree = AtmProp[Int](i => i == 3)
    val delay = Delay(1)

    "assert should capture start and stop points for a sequence when it fails" in {
      val trueThanFalse = Concat(isTrue, isFalse) // isTrue ##1 isFalse
      val trace = Seq(1, 0, 1, 1, 0, 1, 0, 1).map(_ == 1)
      val result = Evaluator.assert(trueThanFalse, trace)
      assert(
        result == AssertResult(
          failed = Seq((2, 3)),
          pending = Seq(7)
        )
      )
    }

    "assert 1 simple assert all true" in {
      val trace = Seq(1, 1, 1, 1, 0, 0, 1, 0, 1).map(_ == 1)
      val result = Evaluator.assert(isTrue, trace)
      assert(
        result == AssertResult(
          failed = Seq((4, 4), (5, 5), (7, 7)),
          pending = Seq()
        )
      )
    }

    "assert 2 (nested concat)" in {
      val twoTrueOneFalse = Concat(isTrue, Concat(isTrue, isFalse)) // isTrue ##1 isTrue ##1 isFalse
      val trace = Seq(1, 1, 1, 0, 1, 1, 0, 1, 1).map(_ == 1)
      val result = Evaluator.assert(twoTrueOneFalse, trace)
      assert(
        result == AssertResult(
          failed = Seq((0, 2), (2, 3), (5, 6)),
          pending = Seq(7, 8)
        )
      )
    }

//   "cover 3 (nested concat with Int)" in {
//     val twoWaitThanThree: ScalaSeq[Int] = Concat(isTwo, isThree) // isTwo ##1 isThree
//     val trace = Seq(1, 2, 3, 2, 4, 3, 2, 2, 3, 2)
//     val result = Evaluator.cover(twoWaitThanThree, trace)
//     assert(
//       result == AssertResult(
//         failed = Seq((1, 2), (7, 8)),
//         pending = Seq(9)
//       )
//     )
//   }

//   "cover 4 (repeated sequence)" in {
//     val repeatTrueThrice = Fuse(Repeated(Fuse(isTrue, Delay(1)), 2), isTrue) // isTrue[*3]
//     val trace = Seq(1, 1, 1, 0, 1, 1, 1, 1, 0, 1).map(_ == 1)
//     val result = Evaluator.cover(repeatTrueThrice, trace)
//     assert(
//       result == AssertResult(
//         failed = Seq((0, 2), (4, 6), (5, 7)),
//         pending = Seq(9)
//       )
//     )
//   }

//   "Cover 5 (Or sequence)" in {
//     val twoTrue = Concat(isTrue, isTrue)
//     val twoFalse = Concat(isFalse, isFalse)
//     val nextUnchanged = Or(twoTrue, twoFalse)
//     val trace = Seq(1, 0, 1, 0, 0, 1, 0, 1, 1, 0).map(_ == 1)
//     val result = Evaluator.cover(nextUnchanged, trace)
//     assert(
//       result == AssertResult(
//         failed = Seq((3, 4), (7, 8)),
//         pending = Seq(9)
//       )
//     )
//       // add test case for seq2 fails when seq1 still running
//   }

//   "Cover 6 (A Implies B sequence)" in {
//     val twoImpliesThree = Implies(Fuse(isTwo, delay), isThree)
//     val trace1 = Seq(1, 2, 3, 4, 3, 2, 4, 3) // with none pending
//     val result1 = Evaluator.cover(twoImpliesThree, trace1)
//     assert(
//       result1 == AssertResult(
//         failed = Seq((1, 2)),
//         pending = Seq()
//       )
//     )

//     val trace2 = Seq(1, 2, 2, 3, 2, 3, 2) // with pending result
//     val result2 = Evaluator.cover(twoImpliesThree, trace2)
//     assert(
//       result2 == AssertResult(
//         failed = Seq((2, 3), (4, 5)),
//         pending = Seq(6)
//       )
//     )
//   }

}