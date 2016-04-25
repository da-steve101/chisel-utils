package chiselutils.xilinx

import Chisel._

/** This module uses xilinx fifo blocks to implement an asyncFifo
  */
class AsyncFifoXilinx[ T <: Data ] ( genType : T, entries : Int, enqClk : Clock, deqClk : Clock ) extends Module {

  // cascade fifos to build arbitrary size? for now just limit to 500
  // make almost full the ready bits
  if ( entries > 500 ) {
    ChiselError.error( "More than 500 entries in async fifo not supported" )
  }
  if ( entries < 6 ) {
    ChiselError.error( "Less than 6 entries in async fifo not supported" )
  }

  val io = new QueueIO( genType, entries )

  val din = UInt( width = genType.getWidth() ).fromBits(io.enq.bits.flatten.map( x => x._2 ).reduceLeft(_ ## _))
  val dout = UInt( width = genType.getWidth() )
  dout := UInt( 0 ) // default assign
  io.deq.bits := genType.fromBits( dout )

  val noPart = math.ceil( genType.getWidth()/36.0 ).toInt
  val no18Fifo = noPart % 2
  val no36Fifo = ( noPart - no18Fifo )/2

  val fifo72Bit = Array.fill( no36Fifo ) { Module( new Fifo36E1( true, 72, 10, entries, enqClk, deqClk ) ) }
  val fifo36Bit = Array.fill( no18Fifo ) { Module( new Fifo36E1( false, 36, 10, entries, enqClk, deqClk ) ) }

  for ( fifoIdx <- 0 until no36Fifo ) {
    fifo72Bit(fifoIdx).io.din := din( math.min( ( fifoIdx + 1 )*72 - 1, genType.getWidth() - 1 ), ( fifoIdx + 1 )*72 - 64 )
    fifo72Bit(fifoIdx).io.dip := din( math.min( ( fifoIdx + 1 )*72 - 65, genType.getWidth() - 1 ), fifoIdx*72 )
    fifo72Bit(fifoIdx).io.wren := io.enq.valid
    fifo72Bit(fifoIdx).io.rden := io.deq.ready
  }

  if ( no18Fifo != 0 ) {
    fifo36Bit(0).io.din := din( genType.getWidth() - 1, genType.getWidth() - 32 )
    fifo36Bit(0).io.dip := din( genType.getWidth() - 33, genType.getWidth() - 36 )
    fifo36Bit(0).io.wren := io.enq.valid
    fifo36Bit(0).io.rden := io.deq.ready
    io.enq.ready := fifo72Bit.map( x => !x.io.almostFull ).reduce(_ && _) && !fifo36Bit(0).io.almostFull
    io.deq.valid := fifo72Bit.map( x => !x.io.empty ).reduce(_ && _) && !fifo36Bit(0).io.empty
  } else {
    io.enq.ready := fifo72Bit.map( x => !x.io.almostFull ).reduce(_ && _)
    io.deq.valid := fifo72Bit.map( x => !x.io.empty ).reduce(_ && _)
  }

  val dataCombined = fifo72Bit.map( x => ( x.io.dout ## x.io.dop ) ).reverse.reduceLeft( _ ## _ )
  val with18Fifo = { if ( no18Fifo != 0 ) {
    val fifo36Part = fifo36Bit(0).io.dout ## fifo36Bit(0).io.dop
    fifo36Part( 35, 35 - ( genType.getWidth() % 36 ) ) ## dataCombined
  } else
    dataCombined( genType.getWidth() - 1, 0 )
  }
  dout := with18Fifo

}
