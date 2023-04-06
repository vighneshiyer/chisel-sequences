// Copyright 2022 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package sequences.backend

import chisel3._

import scala.collection.immutable.{SeqMap, VectorMap}

trait Backend {
  def name: String
  //def compile[S <: Data](prop: PropertyInfo[S]): PropertyAutomatonModule
  def compileFSM[S <: Data](prop: PropertyInfo[S], initialState: S): PropertyFSMAutomatonModule[S]
}

/** Contains a converted property and the name of all predicates used in it. */
case class PropertyInfo[S <: Data](prop: Property[S], predicates: Seq[String])


class PredicateBundle(predicates: Seq[String]) extends Record {
  override val elements:  SeqMap[String, Bool] = VectorMap[String, Bool](predicates.map(p => p -> Input(Bool())): _*)
  override def cloneType: PredicateBundle.this.type = new PredicateBundle(predicates).asInstanceOf[this.type]
}

class PropertyAutomatonIO(preds: Seq[String]) extends Bundle {
  val predicates = new PredicateBundle(preds)
  val fail = Output(Bool())
}

class PropertyFSMAutomatonIO[S <: Data](preds: Seq[String]) extends Bundle {
  val predicates: Map[String, (S) => Bool] = VectorMap[String, (S) => Bool](preds.map(p => (p, (_: S) => Bool())): _*)
  val fail = Output(Bool())
}

abstract class PropertyAutomatonModule extends Module {
  val io: PropertyAutomatonIO
}

abstract class PropertyFSMAutomatonModule[S <: Data] extends Module {
  val io: PropertyFSMAutomatonIO[S]
}

sealed trait BooleanExpr {}
case class SymbolExpr(name: String) extends BooleanExpr
case class NotExpr(e: BooleanExpr) extends BooleanExpr
case class AndExpr(a: BooleanExpr, b: BooleanExpr) extends BooleanExpr
case class OrExpr(a: BooleanExpr, b: BooleanExpr) extends BooleanExpr

sealed trait Sequence[+S <: Data] {}

case class SeqPred(predicate: BooleanExpr) extends Sequence[Bool]
case class SeqStatePred[S <: Data](predicate: BooleanExpr, update: (S) => S) extends Sequence[S]
case class SeqOr[S <: Data](s1: Sequence[S], s2: Sequence[S]) extends Sequence[S]
case class SeqConcat[S <: Data](s1: Sequence[S], s2: Sequence[S]) extends Sequence[S]
case class SeqIntersect[S <: Data](s1: Sequence[S], s2: Sequence[S]) extends Sequence[S]
case class SeqNot[S <: Data](s1: Sequence[S]) extends Sequence[S]
// case class SeqImplies[S <: Data](s1: Sequence[S], p1: Property[S]) extends Sequence[S]
// case class SeqImpliesNext[S <: Data](s1: Sequence[S], p1: Property[S]) extends Sequence[S]
case class SeqImplies[S <: Data](s1: Sequence[S], s2: Sequence[S]) extends Sequence[S]
case class SeqImpliesNext[S <: Data](s1: Sequence[S], s2: Sequence[S]) extends Sequence[S]
case class SeqFuse[S <: Data](s1: Sequence[S], s2: Sequence[S]) extends Sequence[S]

sealed trait Property[+S <: Data] {}

case class PropSeq[S <: Data](s: Sequence[S]) extends Property[S]

object serialize {
  def apply[S <: Data](p: Property[S]): String = {
    p match {
      case PropSeq(s) => apply(s)
    }
  }

  def apply[S <: Data](s: Sequence[S]): String = {
    s match {
      case SeqPred(predicate)     => apply(predicate)
      case SeqOr(s1, s2)          => apply(s1) + " or " + apply(s2)
      case SeqConcat(s1, s2)      => apply(s1) + " ##1 " + apply(s2)
      case SeqIntersect(s1, s2)   => apply(s1) + " and " + apply(s2)
      case SeqNot(s1)             => "not" + apply(s1)
      case SeqImplies(s1, p1)     => apply(s1) + " |-> " + apply(p1)
      case SeqImpliesNext(s1, p1) => apply(s1) + " |=> " + apply(p1)
      case SeqFuse(s1, s2)        => apply(s1) + " ##0 " + apply(s2)
    }
  }

  def apply(e: BooleanExpr): String = e match {
    case SymbolExpr(name) => name
    case NotExpr(e)       => s"!${apply(e)}"
    case AndExpr(a, b)    => s"(${apply(a)} && ${apply(b)})"
    case OrExpr(a, b)     => s"(${apply(a)} || ${apply(b)})"
  }
}
