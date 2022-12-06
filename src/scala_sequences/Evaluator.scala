package scala_sequences

import sequences.backend.HOAParser.HOA

object Evaluator {
  type Time = Int
  case class CoverResult(completed: Seq[(Time, Time)], pending: Seq[Time])
  case class CoverState[T, S](seqsInFlight: Set[(Time, SequenceStatus[T, S])], time: Time, completed: Seq[(Time, Time)])
  case class AssertResult(failed: Seq[(Time, Time)], pending: Seq[Time])
  case class AssertState[T, S](seqsInFlight: Set[(Time, SequenceStatus[T, S])], time: Time, failed: Seq[(Time, Time)])

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

  def constructFormula[T, S](seqn: ScalaSeq[T, S]): (String, Map[AtmProp[T, S], String]) = {
    // seqn[Boolean, Any]: isTrue delay isFalse
    // a -> isTrue
    // b -> isFalse
    // psl: G(a X b)
    // map: {a: isTrue, b: isFalse}
    // hoa
    ???
  }

  def assertHOA[T](trace: Seq[T], hoa: HOA): AssertResult = {
    val finalState = trace.foldLeft(Seq(AssertState[T, S](Set.empty, 0, Seq.empty), hoa.initialState)) { case (Seq(state, currState), value) =>
      val nextState = transition(value, hoa.aps, hoa.states(currState).transitions)
      val seqfailed = nextstate match {
        case Some(i) => ???
        case None    => (state.time, state.time) // when the automata does not have next state / cannot proceed further, record fail time
      }
      Seq(state.copy(
        seqsInFlight = seqsInFlight // no need to keep track seqsInFlight?
        time = state.time + 1,
        failed = state.failed ++ seqfailed
      ), 
      nextState match {
        case Some(i) => i
        case None    => hoa.initialState
      })
    }
    AssertResult(finalState.failed, finalState.seqsInFlight.map{ case (startTime, _) => startTime }.toSeq))
  }

  // given a trace value, return the state id that it will transition into
  def transition(value: T, aps: Map[AP, String], transitions: Map[Condition, Int]): Option[Int] = {
    val nextState = transitions.map { case (cond, next) =>
      val result = cond(value) // TODO apply the transition condition to the current trace value
      (result, next)
    }.toSeq.filter{ case (bool, next) => bool}
    
    if (nextState.isEmpty()) {
      None
    } else {
      nextState.map{ case (bool, next) => next}
    }
  }
}
