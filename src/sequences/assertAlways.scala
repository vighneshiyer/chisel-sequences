package sequences

import chisel3.assert
import chisel3.Data
import backend.{Backend, SequenceFsms}

object assertAlways {
  def apply[S <: Data](prop: Property[S], desc: String = "", backend: Backend = SequenceFsms, initialState: S): Unit = {
    val fail = toAutomaton(prop, backend, initialState)
    assert(!fail, desc)
  }
}
