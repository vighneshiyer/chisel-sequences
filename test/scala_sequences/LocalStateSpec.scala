package scala_sequences

import org.scalatest.freespec.AnyFreeSpec
import scala_sequences.Evaluator.CoverResult
import TestSeqs._

class LocalStateSpec extends AnyFreeSpec {
  //update changes the local state and carry over

  "test local state support with incrOne" in {
    val initial = 1
    val trace = Seq(3, 5, 6, 2, 3, 4, 1) // checking local state match at each txn?
    // val trace = Seq(3, 4) // checking local state match at each txn?
    val result = Evaluator.cover(incrOne, trace, initial)
    assert(
      result == CoverResult(
        completed = Seq((1, 2), (3, 4), (4, 5)), // check finish on the same cycle?
        pending = Seq(6)
      )
    )
  }

}

