package scala_sequences

import org.scalatest.freespec.AnyFreeSpec
import TestSeqs._
import sequences.backend.Spot._
import sequences.backend.HOAParser

class HOAAssertSpec extends AnyFreeSpec {
    
    "test convert ScalaSeq to PSL formula 1" in {
        val seqn = Concat(isTrue, isFalse)
        val (psl, map) = Evaluator.constructFormula(seqn, prefix="G")
        val targetMap = Map("a" -> isTrue, "b" -> isFalse)
        assert(
            psl == "G(a & X (b))" && targetMap.equals(map)
        )
    }

    "test convert ScalaSeq to PSL formula 2" in {
        val seqn = Concat(isTrue, Concat(isFalse, isTrue))
        val (psl, map) = Evaluator.constructFormula(seqn, prefix="G")
        val targetMap = Map("a" -> isTrue, "b" -> isFalse)
        assert(
            psl == "G(a & X (b & X (a)))" && targetMap.equals(map)
        )
    }

    "test convert ScalaSeq to PSL formula 3" in {
        val seqn = Implies(isTrue, isFalse)
        val (psl, map) = Evaluator.constructFormula(seqn, prefix="G")
        val targetMap = Map("a" -> isTrue, "b" -> isFalse)
        assert(
            psl == "G((a) |-> (b))" && targetMap.equals(map)
        )
    }

    "test convert ScalaSeq to PSL formula 4" in {
        val seqn = Fuse(isTrue, isFalse)
        val (psl, map) = Evaluator.constructFormula(seqn, prefix="G")
        val targetMap = Map("a" -> isTrue, "b" -> isFalse)
        assert(
            psl == "G(a & b)" && targetMap.equals(map)
        )
    }

    "assert sequence through HOA for Concat property" in {
        val trace         = Seq(1, 0).map(_ == 1)
        val trueThanFalse = Concat(isTrue, isFalse)
        val (psl, map)    = Evaluator.constructFormula(trueThanFalse, prefix="G")
        val hoaString     = callSpot(psl)
        val hoa           = HOAParser.parseHOA(hoaString)
        val result        = Evaluator.assertHOA(trace, hoa, map)
        assert(
            result.isSuccess == false &&
            result.failed.equals(Seq(1))
        )
    }

    "assert sequence through HOA for all true" in {
        val trace = Seq(1, 1, 1, 1, 0, 0, 1, 0, 1).map(_ == 1)
        val allTrue = isTrue
        val (psl, map) = Evaluator.constructFormula(allTrue, prefix="G")
        val hoaString = callSpot(psl)
        val hoa = HOAParser.parseHOA(hoaString)
        val result = Evaluator.assertHOA(trace, hoa, map)
        assert(
            result.isSuccess == false &&
            result.failed.equals(Seq(4, 5, 7))
        )

        val trace2 = Seq(1, 1, 1, 1).map(_ == 1)
        val result2 = Evaluator.assertHOA(trace2, hoa, map)
        assert(
            result2.isSuccess == true &&
            result2.failed.equals(Seq())
        )
    }

    "assert sequence through HOA for (a & X(b & X(c)))" in {
        val trace = Seq(1, 2, 2, 1, 1, 4, 1, 3)
        val property = Concat(isNumber(1), Concat(isNumber(2), isNumber(3)))
        val (psl, map) = Evaluator.constructFormula(property, prefix="")
        val hoaString = callSpot(psl)
        val hoa = HOAParser.parseHOA(hoaString)
        val result = Evaluator.assertHOA(trace, hoa, map)
        assert(
            result.isSuccess == false &&
            result.failed.equals(Seq(2, 4, 5, 7))
        )
    }

    "assert sequence through HOA for (a & X(b | c))" in {
        // always true when enters the accepting state once.
        val trace = Seq(1, 2, 2, 1, 1, 4, 1, 3)
        val property = Concat(isNumber(1), Or(isNumber(2), isNumber(3)))
        val (psl, map) = Evaluator.constructFormula(property, prefix="")
        val hoaString = callSpot(psl)
        val hoa = HOAParser.parseHOA(hoaString)
        val result = Evaluator.assertHOA(trace, hoa, map)
        assert(
            result.isSuccess == true &&
            result.failed.equals(Seq())
        )
    }


}