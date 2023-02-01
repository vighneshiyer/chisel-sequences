package scala_sequences

import org.scalatest.freespec.AnyFreeSpec
import TestSeqs._

class HOAAssertSpec extends AnyFreeSpec {
    
    "test convert ScalaSeq to PSL formula 1" in {
        val seqn = Concat(isTrue, isFalse)
        val (psl, map) = Evaluator.constructFormula(seqn)
        val targetMap = Map("a" -> isTrue, "b" -> isFalse)
        assert(
            psl == "G(a & X b)" && targetMap.equals(map)
        )
    }

    "test convert ScalaSeq to PSL formula 2" in {
        val seqn = Concat(Concat(isTrue, isFalse), isTrue)
        val (psl, map) = Evaluator.constructFormula(seqn)
        val targetMap = Map("a" -> isTrue, "b" -> isFalse)
        assert(
            psl == "G(a & X b & X a)" && targetMap.equals(map)
        )
    }

    "test convert ScalaSeq to PSL formula 3" in {
        val seqn = Implies(isTrue, isFalse)
        val (psl, map) = Evaluator.constructFormula(seqn)
        val targetMap = Map("a" -> isTrue, "b" -> isFalse)
        assert(
            psl == "G({a} |-> {b})" && targetMap.equals(map)
        )
    }



}