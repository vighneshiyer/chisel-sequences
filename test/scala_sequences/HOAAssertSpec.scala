package scala_sequences

import org.scalatest.freespec.AnyFreeSpec
import TestSeqs._
import Spot._

class HOAAssertSpec extends AnyFreeSpec {
    
    "test convert ScalaSeq to PSL formula 1" in {
        val seqn = Concat(isTrue, isFalse)
        val (psl, map) = Evaluator.constructFormula(seqn)
        val targetMap = Map("a" -> isTrue, "b" -> isFalse)
        assert(
            psl == "G(a & X (b))" && targetMap.equals(map)
        )
    }

    "test convert ScalaSeq to PSL formula 2" in {
        val seqn = Concat(isTrue, Concat(isFalse, isTrue))
        val (psl, map) = Evaluator.constructFormula(seqn)
        val targetMap = Map("a" -> isTrue, "b" -> isFalse)
        assert(
            psl == "G(a & X (b & X (a)))" && targetMap.equals(map)
        )
    }

    "test convert ScalaSeq to PSL formula 3" in {
        val seqn = Implies(isTrue, isFalse)
        val (psl, map) = Evaluator.constructFormula(seqn)
        val targetMap = Map("a" -> isTrue, "b" -> isFalse)
        assert(
            psl == "G((a) |-> (b))" && targetMap.equals(map)
        )
    }

    "test convert ScalaSeq to PSL formula 4" in {
        val seqn = Fuse(isTrue, isFalse)
        val (psl, map) = Evaluator.constructFormula(seqn)
        val targetMap = Map("a" -> isTrue, "b" -> isFalse)
        assert(
            psl == "G(a & b)" && targetMap.equals(map)
        )
    }

    "assert sequence through HOA for Concat property" in {
        val trace         = Seq(1, 0).map(_ == 1)
        val trueThanFalse = Concat(isTrue, isFalse)
        val (psl, map)    = Evaluator.constructFormula(trueThanFalse)
        val hoaString     = callSpot(psl)
        val hoa           = HOAParser.parseHOA(hoaString)
        val result        = Evaluator.assertHOA(trace, hoa)
        assert(
            result.isSuccess == False &&
            result.failed.equals(Seq(1))
        )
    }

}