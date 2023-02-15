package sequences.backend

import org.scalatest.freespec.AnyFreeSpec
import chiseltest._
import chisel3._

class SpotBackendTests extends AnyFreeSpec with ChiselScalatestTester {

  "Spot PSL serializer should emit legal PSL" in {
    val (a, b) = (SymbolExpr("a"), SymbolExpr("b"))
    assert(Spot.sequenceToPSL(SeqPred(a)) == "(a)")
    val expr = SeqConcat(SeqPred(a), SeqPred(b))
    assert(Spot.sequenceToPSL(expr) == "((a) & X((b)))")
  }

  "Spot backend should emit monitor circuit for G(a & X !b)" in {
    val a = SeqPred(SymbolExpr("a"))
    val b = SeqPred(SymbolExpr("b"))
    val notB = SeqNot(b)

    class Container extends Module {
      val mod = Spot.compile(PropertyInfo(PropSeq(SeqConcat(a, notB)), Seq("a", "b")))
      val io = IO(new PropertyAutomatonIO(Seq("a", "b")))
      io.predicates <> mod.io.predicates
      io.fail := mod.io.fail
    }

    test(new Container()).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.io.predicates.elements("a").poke(1.B) // a must always be true
      c.io.predicates.elements("b").poke(1.B) // b must be false after a (b must always be false after the first cycle)
      c.io.fail.expect(0.B)
      c.clock.step()
      c.io.predicates.elements("b").poke(0.B)
      c.io.fail.expect(0.B)
      c.clock.step(10)
      c.io.fail.expect(0.B)
    }

    test(new Container()).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.io.predicates.elements("a").poke(1.B)
      c.io.predicates.elements("b").poke(1.B)
      c.io.fail.expect(0.B)
      c.clock.step()
      c.io.predicates.elements("b").poke(1.B) // b should have gone low!
      c.io.fail.expect(1.B) // fail here and forever after
      c.clock.step(1)
      c.io.fail.expect(1.B)
      c.clock.step(10)
      c.io.fail.expect(1.B)
    }
  }

  "Spot PSL serializer check for multiple concat" in {
    val (a, b, c) = (SymbolExpr("a"), SymbolExpr("b"), SymbolExpr("c"))
    val expr = SeqConcat(SeqPred(a), SeqConcat(SeqPred(b), SeqPred(c)))
    assert(Spot.sequenceToPSL(expr) == "((a) & X(((b) & X((c)))))")
  }

  "Spot PSL serializer check for SeqOr" in {
    val (a, b, c) = (SymbolExpr("a"), SymbolExpr("b"), SymbolExpr("c"))
    val expr = SeqOr(SeqConcat(SeqPred(a), SeqPred(b)), SeqPred(c))
    assert(Spot.sequenceToPSL(expr) == "(((a) & X((b))) | (c))")
  }

  "Spot PSL serializer check for SeqFuse" in {
    val (a, b, c) = (SymbolExpr("a"), SymbolExpr("b"), SymbolExpr("c"))
    val expr = SeqFuse(SeqFuse(SeqPred(a), SeqPred(b)), SeqPred(c))
    assert(Spot.sequenceToPSL(expr) == "(((a) & ((b))) & ((c)))")
  }

  "Spot backend should emit monitor circuit G(a & X (b | !c))" in {
    val a = SeqPred(SymbolExpr("a"))
    val b = SeqPred(SymbolExpr("b"))
    val c = SeqPred(SymbolExpr("c"))
    val notC = SeqNot(c)
    val expr = SeqConcat(a, SeqOr(b, notC))
    assert(Spot.sequenceToPSL(expr) == "((a) & X(((b) | !(c))))")

    class Container extends Module {
      val mod = Spot.compile(PropertyInfo(PropSeq(SeqConcat(a, SeqOr(b, notC))), Seq("a", "b", "c")))
      val io = IO(new PropertyAutomatonIO(Seq("a", "b", "c")))
      io.predicates <> mod.io.predicates
      io.fail := mod.io.fail
    }

    test(new Container()).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.io.predicates.elements("a").poke(1.B)
      c.io.predicates.elements("b").poke(1.B)
      c.io.predicates.elements("c").poke(1.B)
      c.clock.step()
      c.io.fail.expect(0.B)
      c.io.predicates.elements("b").poke(0.B)
      c.clock.step()
      c.io.fail.expect(1.B)
    }

    test(new Container()).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      c.io.predicates.elements("a").poke(1.B)
      c.io.predicates.elements("b").poke(1.B)
      c.io.predicates.elements("c").poke(1.B)
      c.io.fail.expect(0.B)
      c.clock.step()
      c.io.predicates.elements("b").poke(0.B)
      c.io.predicates.elements("c").poke(1.B)
      c.io.fail.expect(1.B)
    }
  }
}
