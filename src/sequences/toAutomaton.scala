// Copyright 2022 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package sequences

import chisel3.Data
import chisel3.Bool

import scala.collection.mutable

object toAutomaton {
  def apply[S <: Data](prop: Property[S], backendImpl: backend.Backend, initialState: S): Bool = {
    val (info, pred) = ToIRConverter.toIR(prop, initialState)
    val mod = backendImpl.compileFSM(info)
    // connect predicates as inputs
    // Only for FSM backend
    //mod.io.predicates.foreach { case (name, input) => input := pred(name) }
    // return fail signal
    mod.io.fail
  }
}

private object ToIRConverter {
  def toIR[S <: Data](prop: Property[S], initialState: S): (backend.PropertyInfo, Map[String, (S) => Bool]) = new ToIRConverter().toIR(prop, initialState)
}

private class ToIRConverter private () {
  private val predMap = mutable.LinkedHashMap[(S) => Bool, String]()

  def toIR[S <: Data](prop: Property[S], initialState: S): (backend.PropertyInfo, Map[String, (S) => Bool]) = {
    predMap.clear()
    val propertyIR = convert(prop, initialState)
    val nameToPred = predMap.toSeq.map { case (a, b) => (b, a) }
    (backend.PropertyInfo(propertyIR, nameToPred.map(_._1)), nameToPred.toMap)
  }

  private def convert[S <: Data](prop: Property[S], initialState: S): backend.Property = prop match {
    case PropSeq(s) => backend.PropSeq(convert(s, initialState))
  }

  private def convert[S <: Data](seq: Sequence[S], state: S): backend.Sequence = seq match {
    case SeqExpr(predicate)     => backend.SeqPred(convert(predicate))
    case SeqStateExpr(predicate, update) => backend.SeqStatePred(convert(predicate(state)), update)
    case SeqOr(s1, s2)          => backend.SeqOr(convert(s1, state), convert(s2, state))
    case SeqConcat(s1, s2)      => backend.SeqConcat(convert(s1, state), convert(s2, state))
    case SeqIntersect(s1, s2)   => backend.SeqIntersect(convert(s1, state), convert(s2, state))
    case SeqNot(s1)             => backend.SeqNot(convert(s1, state))
    case SeqImplies(s1, p1)     => backend.SeqImplies(convert(s1, state), convert(p1, state))
    case SeqImpliesNext(s1, p1) => backend.SeqImpliesNext(convert(s1, state), convert(p1, state))
    case SeqFuse(s1, s2)        => backend.SeqFuse(convert(s1, state), convert(s2, state))
  }

  private def convert(e: chisel3.Bool): backend.BooleanExpr = {
    // TODO: is there a way to introspect the chisel to find when an expression is a and/or/not expression?
    val name = predMap.getOrElseUpdate(e, f"p${pred.size}")
    backend.SymbolExpr(name)
  }
}
