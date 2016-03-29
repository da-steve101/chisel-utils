package chiselutils.interfaces.exanicx4

import Chisel._

class TimeSeriesInterfaceIO[ T <: Bits ]( genType : T, noReg : Int ) extends Bundle {
  val dataIn = Decoupled( Vec.fill( 8 ) { UInt( width = 8) } ).flip
  val regIn = Vec.fill( noReg ) { UInt(INPUT, 32) }
  val regOut = Vec.fill( noReg ) { UInt(OUTPUT, 32) }
  val regOutEn = Bool( OUTPUT )
  val memAddr = UInt( OUTPUT, 19 )
  val memData = UInt( INPUT, 128 )
  val error = Bool(OUTPUT)
  val dataOut = Decoupled( genType.cloneType )
}

class TimeSeriesInterface[T <: Bits ]( genType : T, noReg : Int) extends Module {
  def bwOfGenType() = genType.getWidth
  def getNoReg() = noReg
  val io = new TimeSeriesInterfaceIO( genType, noReg )
}
