// Copyright 2022 The Regents of the University of California
// released under BSD 3-Clause License
// author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package sequences

import chisel3._
import scala.language.implicitConversions

package object sequences {
  implicit def chiselBoolToSequence(b: chisel3.Bool): Sequence[Bool] = SeqExpr(b)
  implicit def chiselBoolToProperty(b: chisel3.Bool): Property[Bool] = PropSeq(SeqExpr(b))
  implicit def sequenceToProperty[S <: Data](s:   Sequence[S]):     Property[S] = PropSeq(s)
}

sealed trait Sequence[+S <: Data] {
  def ###(delay: Int = 1)(other: Sequence[S]): SeqConcat[S] = {
    require(delay >= 0, "negative delay is not supported")
    require(delay == 1, "TODO")
    SeqConcat(this, other)
  }
  def |->(p: Property[S]): SeqImplies[S] = SeqImplies(this, p)
  def |=>(p: Property[S]): SeqImpliesNext[S] = SeqImpliesNext(this, p)
}

case class SeqExpr(predicate: chisel3.Bool) extends Sequence[Bool]
case class SeqStateExpr[S <: Data](predicate: (S) => chisel3.Bool, update: (S) => S) extends Sequence[S]
case class SeqOr[S <: Data](s1: Sequence[S], s2: Sequence[S]) extends Sequence[S]
case class SeqConcat[S <: Data](s1: Sequence[S], s2: Sequence[S]) extends Sequence[S]
case class SeqIntersect[S <: Data](s1: Sequence[S], s2: Sequence[S]) extends Sequence[S]
case class SeqNot[S <: Data](s1: Sequence[S]) extends Sequence[S]
case class SeqImplies[S <: Data](s1: Sequence[S], p1: Property[S]) extends Sequence[S]
case class SeqImpliesNext[S <: Data](s1: Sequence[S], p1: Property[S]) extends Sequence[S]
case class SeqFuse[S <: Data](s1: Sequence[S], s2: Sequence[S]) extends Sequence[S]

sealed trait Property[+S <: Data] {}

case class PropSeq[S <: Data](s: Sequence[S]) extends Property[S]

sealed trait LocalState {}
case class StateBinding[T <: Data](state: T) extends LocalState