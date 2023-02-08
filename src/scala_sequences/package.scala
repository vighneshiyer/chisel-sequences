package object scala_sequences {
  // A ScalaSeq is a temporal sequence defined on a trace of type T and carrying per-instance state of type S
  sealed trait ScalaSeq[+T, +S]

  case class AtmProp[T, S](pred: (T, S) => Boolean, update: (T, S) => S) extends ScalaSeq[T, S]
  case class Fuse[T, S](seq1: ScalaSeq[T, S], seq2: ScalaSeq[T, S]) extends ScalaSeq[T, S]
  case class Delay(n: Int) extends ScalaSeq[Nothing, Nothing] {
    require(n >= 0)
  }
  case class Repeated[T, S](seq: ScalaSeq[T, S], repeats: Int) extends ScalaSeq[T, S]
  // case class UnboundedRepeat[T](seq: ScalaSeq[T], min_repeat: Int) extends ScalaSeq[T]
  case class Or[T, S](seq1: ScalaSeq[T, S], seq2: ScalaSeq[T, S]) extends ScalaSeq[T, S]
  case class Implies[T, S](seq1: ScalaSeq[T, S], seq2: ScalaSeq[T, S]) extends ScalaSeq[T, S]

  def Concat[T, S](seq1: ScalaSeq[T, S], seq2: ScalaSeq[T, S]): ScalaSeq[T, S] = {
    Fuse(seq1, Fuse(Delay(1), seq2))
  }

  sealed trait Property[+T, S] {}

  case class PropSeq[T, S](seq: ScalaSeq[T, S], stateGen: () => S) extends Property[T, S]
}
