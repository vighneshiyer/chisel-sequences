package scala_sequences

object TestSeqs {
  val isTrue = AtmProp[Boolean, Any]((i, _) => i, (_, s) => s)
  val isFalse = AtmProp[Boolean, Any]((i, _) => !i, (_, s) => s)
  val delay = Delay(1)

  def isNumber(number: Int): AtmProp[Int, Any] = {
    AtmProp((i, _) => i == number, (_, s) => s)
  }

  type History = Int
  val recordValue = AtmProp[Int, History]((value, state) => true, (value, state) => value + 1)
  val checkValue = AtmProp[Int, History]((value, state) => value == state, (_, s) => s)
  val incrOne = Implies(recordValue, Fuse(Delay(1), checkValue))
  //val noUpdates = AtmProp[Int, History]((i, localState) => i == localState)

  // frontend sketch
  /*
  properties[Int, History] {
    // in implicit scope we have 'value' and we have 'state'
    val incrOne = (true, value+1) |=> (state == value, None)
  }

  properties[T <: Data, S <: Data] {

  }
  */
}
