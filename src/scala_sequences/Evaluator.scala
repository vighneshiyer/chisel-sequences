package scala_sequences

import sequences.backend.HOAParser.HOA
import sequences.backend.HOAParser.Condition
import sequences.backend.HOAParser
import sequences.backend.HOAParser.True

object Evaluator {
  type Time = Int
  type StateId = Int
  case class CoverResult(completed: Seq[(Time, Time)], pending: Seq[Time])
  case class CoverState[T, S](seqsInFlight: Set[(Time, SequenceStatus[T, S])], time: Time, completed: Seq[(Time, Time)])
  case class AssertResult(failed: Seq[(Time, Time)], pending: Seq[Time])
  case class AssertState[T, S](seqsInFlight: Set[(Time, SequenceStatus[T, S])], time: Time, failed: Seq[(Time, Time)])

  // only provide whether assertion failed and timestamp of failure starting point
  case class AssertHOAResult(isSuccess: Boolean, failed: Seq[Time])
  case class AssertHOAState(currStateId: StateId, time: Time, failed: Seq[Time])

  case class CheckResult(result: Seq[(Time, Time)], pending: Seq[Time])
  case class CheckState[T, S](seqsInFlight: Set[(Time, SequenceStatus[T, S])], time: Time, result: Seq[(Time, Time)])

  sealed trait SequenceStatus[+T, +S]
  case object TerminatedFailed extends SequenceStatus[Nothing, Nothing]
  case class TerminatedDone[S](state: S) extends SequenceStatus[Nothing, S]
  case class Running[T, S](next: ScalaSeq[T, S], state: S) extends SequenceStatus[T, S]

  // Given this value, and a running sequence, does it terminate as successful or failed on this cycle,
  //   or is it still going to be running (with something to check later in time)?
  private def step[T, S](value: T, status: SequenceStatus[T, S]): SequenceStatus[T, S] = {
    status match {
      case Running(next, state) =>
        next match {
          case ap: AtmProp[T, S] =>
            if (ap.pred(value, state)) {
              TerminatedDone(ap.update(value, state))
            } else {
              TerminatedFailed
            }
          case fuse: Fuse[T, S] =>
            val seq1Step = step(value, Running(fuse.seq1, state))
            seq1Step match {
              case TerminatedFailed => TerminatedFailed
              case TerminatedDone(newState) =>
                val seq2Step = step(value, Running(fuse.seq2, newState))
                seq2Step match {
                  case TerminatedFailed => TerminatedFailed
                  case TerminatedDone(state)   => TerminatedDone(state)
                  case r @ Running(_, _)   => r
                }
              case r: Running[T, S] => Running(Fuse(r.next, fuse.seq2), r.state)
            }
          case Delay(n) =>
            if (n <= 0) {
              TerminatedDone(state)
            } else {
              Running(Delay(n - 1), state)
            }
          case Repeated(seq, repeats) =>
            if (repeats == 0) {
              TerminatedDone(state)
            } else if (repeats == 1) {
              step(value, Running(seq, state))
            } else {
              val seqStep = step(value, Running(seq, state))
              seqStep match {
                case TerminatedFailed => TerminatedFailed
                case TerminatedDone(newState)   => step(value, Running(Repeated(seq, repeats - 1), newState))
                case Running(next, newState)    => Running(Fuse(next, Repeated(seq, repeats - 1)), newState)
              }
            }
          case Or(seq1, seq2)     => 
            val seq1Step = step(value, Running(seq1, state))
            val seq2Step = step(value, Running(seq2, state))
            seq1Step match {
              case TerminatedFailed =>
                seq2Step match {
                  case TerminatedFailed => TerminatedFailed
                  case TerminatedDone(state)   => TerminatedDone(state)
                  case r @ Running(_, _)   => r
                }
              case TerminatedDone(state) => TerminatedDone(state)
              case r: Running[T, S]  =>
                // consider case when seq2 finish before seq1 here, check Seq2Step as well
                seq2Step match {
                  case TerminatedDone(state)   => TerminatedDone(state)
                  case TerminatedFailed => r
                  // TODO: bug!, we actually need to consider forked state!
                  case r2: Running[T, S]  => Running(Or(r.next, r2.next), r.state)
                    //r // all other case, continue running r for seq1
                }
            }
          case Implies(seq1, seq2) => 
            val seq1Step = step(value, Running(seq1, state))
            seq1Step match {
              case TerminatedFailed => TerminatedFailed
              case TerminatedDone(newState)   =>
                val seq2Step = step(value, Running(seq2, newState))
                seq2Step match {
                  case TerminatedFailed => TerminatedFailed
                  case TerminatedDone(newState)   => TerminatedDone(newState)
                  case r: Running[T, S]    => r
                }
              case r: Running[T, S]    => Running(Implies(r.next, seq2), r.state)
            }
        }
      case TerminatedDone(newState)   => TerminatedDone(newState)
      case TerminatedFailed => TerminatedFailed
    }
  }

  // Given this value, does this sequence start?
  private def start[T, S](value: T, seq: ScalaSeq[T, S], initState: S): Boolean = {
    seq match {
      case ap: AtmProp[T, S] => ap.pred(value, initState)
      case Fuse(seq1, seq2)       => start(value, seq1, initState)
      case Delay(n)               => n > 0
      case Repeated(seq, repeats) => start(value, seq, initState)
      case Or(seq1, seq2)         => start(value, seq1, initState) || start(value, seq2, initState)
      case Implies(seq1, seq2)    => start(value, seq1, initState)
    }
  }

  // assert result gives tuples of time stamps when sequence starts and failed
  def assert[T, S](sequence: ScalaSeq[T, S], trace: Seq[T], initState: S = new Object()): AssertResult = {
    val finalState = trace.foldLeft(AssertState[T, S](Set.empty, 0, Seq.empty)) { (state, value) =>
      val seqsInFlight: Set[(Time, SequenceStatus[T, S])] = if (start(value, sequence, initState)) {
        state.seqsInFlight.incl((state.time, Running(sequence, initState)))
      } else {
        state.seqsInFlight.incl((state.time, TerminatedFailed))
      }
      // step the sequence
      val seqsAfterStep = seqsInFlight.map{ case (startTime, seq) => (startTime, step(value, seq))}

      // keep the sequence that fails, contrary to cover state. Filter out success states
      val seqsFails = seqsAfterStep.filter { case (startTime, seqStatus) =>
        seqStatus match {
          case TerminatedFailed => true
          case TerminatedDone(_)   => false
          case Running(_, _)    => true
        }
      }
      val seqsFailedCompleted = seqsFails.filter { case (startTime, seqStatus) =>
        seqStatus match {
          case TerminatedFailed => true
          case TerminatedDone(_)   => false
          case Running(_, _)    => false
        }
      }.map{ case (startTime, seqStatus) => (startTime, state.time)}

      state.copy(
        seqsInFlight = seqsAfterStep.filter { case (startTime, seqStatus) =>
          seqStatus match { case r: Running[T, S] => true; case _ => false }
        },
        time = state.time + 1,
        failed = state.failed ++ seqsFailedCompleted
      )
    }
    AssertResult(finalState.failed, finalState.seqsInFlight.map{ case (startTime, _) => startTime }.toSeq)
  }


  def cover[T, S](sequence: ScalaSeq[T, S], trace: Seq[T], initState: S = new Object()): CoverResult = {
    val finalState = trace.foldLeft(CoverState[T, S](Set.empty, 0, Seq.empty)) { (state, value) =>
      // If a new sequence instance launches now, then add it to the sequences in flight
      val seqsInFlight: Set[(Time, SequenceStatus[T, S])] = if (start(value, sequence, initState)) {
        state.seqsInFlight.incl((state.time, Running(sequence, initState))) // record when this sequence started
      } else {
        state.seqsInFlight
      }

      // Step each sequence instance given this value
      val seqsAfterStep = seqsInFlight.map { case (startTime, seq) => (startTime, step(value, seq)) }

      // If any sequence instances have failed, remove then from the in-flight set
      val seqsNonFailures = seqsAfterStep.filter { case (startTime, seqStatus) =>
        seqStatus match {
          case TerminatedFailed =>
            false // TODO: we probably want to track failures in "assert" mode, but this is "cover" mode
          case TerminatedDone(_) => true
          case Running(_, _)  => true
        }
      }

      // If any sequence instances have completed, remove them from the in-flight set, and turn them into (startTime, endTime) pairs
      val seqsCompleted = seqsNonFailures.filter { case (startTime, seqStatus) =>
        seqStatus match {
          case TerminatedFailed => ??? // should never happen
          case TerminatedDone(_)   => true
          case Running(_, _)    => false
        }
      }.map { case (startTime, seqStatus) => (startTime, state.time) }

      state.copy(
        // only keep the sequences still in the 'Running' state
        seqsInFlight = seqsAfterStep.filter { case (startTime, seqStatus) =>
          seqStatus match { case r: Running[T, S] => true; case _ => false }
        },
        time = state.time + 1,
        completed = state.completed ++ seqsCompleted
      )
    }
    CoverResult(finalState.completed, finalState.seqsInFlight.map { case (startTime, _) => startTime }.toSeq)
  }

  // consolidated function with option to choose between cover or assert
  def check[T, S](sequence: ScalaSeq[T, S], trace: Seq[T], initState: S = new Object(), isAssert: Boolean): CheckResult = {
      val finalState = trace.foldLeft(CheckState[T, S](Set.empty, 0, Seq.empty)) { (state, value) =>
      // If a new sequence instance launches now, then add it to the sequences in flight
      val seqsInFlight: Set[(Time, SequenceStatus[T, S])] = if (start(value, sequence, initState)) {
        state.seqsInFlight.incl((state.time, Running(sequence, initState))) // record when this sequence started
      } else if (isAssert) {
        state.seqsInFlight.incl((state.time, TerminatedFailed))
      } else {
        state.seqsInFlight
      }

      // Step each sequence instance given this value
      val seqsAfterStep = seqsInFlight.map { case (startTime, seq) => (startTime, step(value, seq)) }

      // If any sequence instances have failed, remove then from the in-flight set
      val seqsTarget = seqsAfterStep.filter { case (startTime, seqStatus) =>
        if (isAssert) {
          seqStatus match {
            case TerminatedFailed => true
            case TerminatedDone(_) => false
            case Running(_, _) => true
          }
        } else {
          seqStatus match {
            case TerminatedFailed =>
              false // TODO: we probably want to track failures in "assert" mode, but this is "cover" mode
            case TerminatedDone(_) => true
            case Running(_, _)  => true
          }
        }
      }

      // If any sequence instances have completed, remove them from the in-flight set, and turn them into (startTime, endTime) pairs
      val seqsCompleted = seqsTarget.filter { case (startTime, seqStatus) =>
        if (isAssert) {
          seqStatus match {
            case TerminatedFailed => true
            case TerminatedDone(_) => false
            case Running(_, _) => false
          }
        } else {
          seqStatus match {
            case TerminatedFailed => ??? // should never happen
            case TerminatedDone(_)   => true
            case Running(_, _)    => false
          }
        }
      }.map { case (startTime, seqStatus) => (startTime, state.time) }

      state.copy(
        // only keep the sequences still in the 'Running' state
        seqsInFlight = seqsAfterStep.filter { case (startTime, seqStatus) =>
          seqStatus match { case r: Running[T, S] => true; case _ => false }
        },
        time = state.time + 1,
        result = state.result ++ seqsCompleted
      )
    }
    CheckResult(finalState.result, finalState.seqsInFlight.map { case (startTime, _) => startTime }.toSeq)
  }

  // helper function for constructFormula
  def toFormula[T, S](seqn: ScalaSeq[T, S], map: Map[String, AtmProp[T, S]]): (String, Map[String, AtmProp[T, S]]) = {
      seqn match {
        case seq: AtmProp[T, S] =>  if (map.isEmpty) { 
          ("a", Map("a" -> seq))
        } else if (map.values.exists(_ == seq)) {
          // obtain psl symbol for existing AtmProp
          (map.find(_._2 == seq).map(_._1).getOrElse(""), map)
        } else { 
          val maxString = map.keySet.max
          // generate new key instance for each new AtmProp
          val newKey: String = (maxString.charAt(0) + 1).toChar.toString()
          val newMap         = map + (newKey -> seq)
          (newKey, newMap)
        }
        case Fuse(s1, s2)    => {
          val (psl1, newMap1) = toFormula(s1, map)
          val (psl2, newMap2) = toFormula(s2, newMap1)
          s1 match {
            case Delay(n) => (s"${psl1} (${psl2})", newMap2)
            case _        => (s"${psl1} & ${psl2}", newMap2)
          }
        }
        case Delay(n)        => (s"X", map)
        case Repeated(s1, n) => (s"{${toFormula(s1, map)._1}[*${n}]}", map)
        case Or(s1, s2)      => {
          val (psl1, newMap1) = toFormula(s1, map)
          val (psl2, newMap2) = toFormula(s2, newMap1)
          (s"(${psl1}) | (${psl2})", newMap2)
        }
        case Implies(s1, s2) => {
          val (psl1, newMap1) = toFormula(s1, map)
          val (psl2, newMap2) = toFormula(s2, newMap1)
          (s"(${psl1}) |-> (${psl2})", newMap2)
        }
      }
    }

  def constructFormula[T, S](seqn: ScalaSeq[T, S], prefix: String): (String, Map[String, AtmProp[T, S]]) = {
    // seqn[Boolean, Any]: isTrue delay isFalse
    // a -> isTrue
    // b -> isFalse
    // psl: G(a X b)
    // map: {a: isTrue, b: isFalse}
    // hoa
    val (formula, map) = toFormula(seqn, Map.empty[String, AtmProp[T, S]])
    (s"${prefix}(${formula})", map)
  }

  // assert trace sequences with HOA model
  // map: mapping of hoa symbol to ScalaSeq AtmProp
  // if failed at a certain time, return to state 0 on the next trace
  def assertHOA[T, S](trace: Seq[T], hoa: HOA, map: Map[String, AtmProp[T, Any]]): AssertHOAResult = {
    val finalState = trace.foldLeft(AssertHOAState(hoa.initialState, 0, Seq.empty)) { case (state, value) =>
      val transitions: Map[Condition, Int]  = hoa.states(state.currStateId).transitions
      val nextStateMap: Map[Condition, Int] = transitions.filter { case (cond, next) =>
        evalCondition(cond, value, null, hoa, map) // null for local state (not implemented)
      }
      val nextState: Int = if (nextStateMap.isEmpty) {
        hoa.initialState
      } else {
        // obtain the next state ID; nextStateMap should only contain one valid entry at this point
        nextStateMap.values.head
      }

      state.copy(
        currStateId = nextState, 
        time = state.time + 1, 
        failed = if (nextStateMap.isEmpty) { state.failed ++ Seq(state.time)} else { state.failed}
      )
    }
    AssertHOAResult(finalState.failed.equals(Seq.empty), finalState.failed)
  }

  // TODO: state: S is reserved for local state. Currently is a placeholder.
  def evalCondition[T, S](condition: Condition, value: T, state: S, hoa: HOA, map: Map[String, AtmProp[T, S]]): Boolean = {
    condition match {
      case HOAParser.True              => true
      case HOAParser.Predicate(ap)     => map(hoa.aps(ap)).pred(value, state)
      case HOAParser.Not(pred)         => !evalCondition(pred, value, state, hoa, map)
      case HOAParser.And(conds @ _*)   => conds.map(evalCondition(_, value, state, hoa, map)).reduce(_ && _)
      case HOAParser.Or(conds @ _*)    => conds.map(evalCondition(_, value, state, hoa, map)).reduce(_ || _)
    }
  }

}