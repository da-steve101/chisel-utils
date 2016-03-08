package chiselutils.interfaces

import Chisel._

class InterfaceableIO[Tx <: Data, Rx <: Data](genTx : Tx, genRx : Rx) extends Bundle {
  val rx = Decoupled(genRx.cloneType).flip()
  val tx = Decoupled(genTx.cloneType)
}

class Interfaceable[Tx <: Data, Rx <: Data] (txDataFormat : Tx, rxDataFormat : Rx) extends Module {
  val io = new InterfaceableIO[Tx, Rx](txDataFormat, rxDataFormat)
}
