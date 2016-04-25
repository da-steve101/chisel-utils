package chiselutils.xilinx

import Chisel._

class Fifo36E1Io( is36Mode : Boolean, dataWidth : Int ) extends Bundle {
  if ( !Array( 4, 9, 18, 36, 72 ).contains( dataWidth ) ) {
    ChiselError.error( "dataWidth must be 4, 9, 18, 36 or 72" )
  }
  if ( !is36Mode && dataWidth == 72 ) {
    ChiselError.error( "must be in 36 mode for dataWidth of 72" )
  }

  def getParityWidth : Int = {
    if ( dataWidth == 4 ) { 0 }
    else if ( dataWidth == 9 ) { 1 }
    else if ( dataWidth == 18 ) { 2 }
    else if ( dataWidth == 36 ) { 4 }
    else ( 8 )
  }

  val din = UInt( INPUT, dataWidth - getParityWidth )
  val dip = UInt( INPUT, getParityWidth )
  val wren = Bool( INPUT )
  val rden = Bool( INPUT )
  // These are used in async mode
  // val rstreg = Bool( INPUT )
  // val regce = Bool( INPUT )
  val dout = UInt( OUTPUT, dataWidth - getParityWidth )
  val dop = UInt( OUTPUT, getParityWidth )
  val full = Bool( OUTPUT )
  val almostFull = Bool( OUTPUT )
  val empty = Bool( OUTPUT )
  val almostEmpty = Bool( OUTPUT )
  val rdCount = UInt( OUTPUT, 13 )
  val wrCount = UInt( OUTPUT, 13 )
  val wrErr = Bool( OUTPUT )
  val rdErr = Bool( OUTPUT )

  def setNames() = {
    din.setName("DIN")
    dip.setName("DIP")
    wren.setName("WREN")
    rden.setName("RDEN")
    // rstreg.setName("RSTREG")
    // regce.setName("REGCE")
    dout.setName("DOUT")
    dop.setName("DOP")
    full.setName("FULL")
    almostFull.setName("ALMOSTFULL")
    empty.setName("EMPTY")
    almostEmpty.setName("ALMOSTEMPTY")
    rdCount.setName("RDCOUNT")
    wrCount.setName("WRCOUNT")
    wrErr.setName("WRERR")
    rdErr.setName("RDERR")
  }
}

class Fifo36E1Param( almostFull : Int, almostEmpty : Int, fwft : Boolean, doReg : Int,
  dataWidth : Int, fifoMode : String, enSyn : Boolean, srVal : Int, init : Int ) extends VerilogParameters {
  if ( !Array( 4, 9, 18, 36, 72 ).contains( dataWidth ) ) {
    ChiselError.error( "dataWidth must be 4, 9, 18, 36 or 72" )
  }
  if ( !Array( "FIFO36", "FIFO36_72", "FIFO18", "FIFO18_36" ).contains( fifoMode ) ) {
    ChiselError.error( "invalid fifoMode" )
  }
  if ( enSyn && fwft ) {
    ChiselError.error( "enSyn and fwft cannot be set simultaneously" )
  }
  if ( !enSyn && doReg != 1 ) {
    ChiselError.error( "if enSyn is false, doReg must be 1" )
  }
  if ( srVal != 0 && !( enSyn  && doReg == 1 ) ) {
    ChiselError.warning( "srVal is only supported when enSyn is true and doReg is 1" )
  }
  if ( init != 0 && !( enSyn  && doReg == 1 ) ) {
    ChiselError.warning( "srVal is only supported when enSyn is true and doReg is 1" )
  }

  val ALMOST_FULL_OFFSET = almostFull
  val ALMOST_EMPTY_OFFSET = almostEmpty
  val FIRST_WORD_FALL_THROUGH = fwft
  val DO_REG = doReg
  val DATA_WIDTH = dataWidth
  val FIFO_MODE = fifoMode
  val EN_SYN = enSyn
  val SRVAL = srVal
  val INIT = init
}

class Fifo36E1( val is36Mode : Boolean, val dataWidth : Int, val almostEmpty : Int,
  val almostFull : Int, val enqClk : Clock, val deqClk : Clock ) extends BlackBox {

  val io = new Fifo36E1Io( is36Mode, dataWidth )

  if ( almostEmpty < 6 ) {
    ChiselError.error( "almostFull must be atleast 6" )
  }
  if ( almostFull < 4 ) {
    ChiselError.error( "almostEmpty must be atleast 4" )
  }
  if ( almostEmpty > getCapacity - 6 ) {
    ChiselError.error( "almostEmpty must be at most " + ( getCapacity - 6 ) )
  }
  if ( almostFull > getCapacity - 8 ) {
    ChiselError.error( "almostFull must be at most " + ( getCapacity - 8 ) )
  }
  if ( enqClk.getReset != deqClk.getReset ) {
    ChiselError.error( "enq and deq clocks must have the same reset for xilinx async fifo" )
  }

  deqClk.getReset.setName("RST")

  val fwft = true
  val doReg = 1
  val enSyn = false
  val srVal = 0
  val init = 0

  // use an async fifo for simulation for now ...
  val simFifo = Module( new AsyncFifo( UInt( width = dataWidth ),
    getCapacity, enqClk, deqClk ) )
  simFifo.io.enq.bits := io.din ## io.dip
  simFifo.io.enq.valid := io.wren
  io.full := !simFifo.io.enq.ready
  io.empty := !simFifo.io.deq.valid
  simFifo.io.deq.ready := io.rden
  io.almostFull := ( simFifo.io.count >= UInt(almostFull) )
  io.almostEmpty := ( simFifo.io.count <= UInt(almostEmpty) )
  val rdCount = Reg( init = UInt( 0, 13 ), clock = deqClk )
  val wrCount = Reg( init = UInt( 0, 13 ), clock = enqClk )
  when ( io.wren ) {
    wrCount := wrCount + UInt( 1 )
  }
  when ( io.rden ) {
    rdCount := rdCount + UInt( 1 )
  }
  io.rdCount := rdCount
  io.wrCount := wrCount
  io.dout := simFifo.io.deq.bits( dataWidth - 1, io.dop.getWidth() )
  io.dop := simFifo.io.deq.bits( io.dop.getWidth() - 1, 0 )

  val verParams = new Fifo36E1Param( almostFull, almostEmpty, fwft, doReg,
    dataWidth, { if ( is36Mode ) "FIFO36" else "FIFO18" },
    enSyn, srVal, init )

  setVerilogParameters( verParams )

  def getCapacity = {
    if ( is36Mode ) {
      if ( dataWidth == 4 ) { 8193 }
      else if ( dataWidth == 9 ) { 4097 }
      else if ( dataWidth == 18 ) { 2049 }
      else if ( dataWidth == 36 ) { 1025 }
      else 513 
    } else {
      if ( dataWidth == 4 ) { 4097 }
      else if ( dataWidth == 9 ) { 2049 }
      else if ( dataWidth == 18 ) { 1025 }
      else 513
    }
  }
}
