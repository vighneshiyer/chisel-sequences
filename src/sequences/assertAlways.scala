package sequences

import chisel3.assert
import chisel3.Bits
import backend.{Backend, SequenceFsms}

object assertAlways {
  def apply[S <: Bits](prop: Property, desc: String = "", backend: Backend = SequenceFsms, initialState: S): Unit = {
    val fail = toAutomaton(prop, backend, initialState)
    assert(!fail, desc)
  }
}
