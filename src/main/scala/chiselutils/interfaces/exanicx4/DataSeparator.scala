package chiselutils.interfaces.exanicx4

import Chisel._

class DataSeparator ( val bytesOut : Int ) extends Module {

  val genType = Vec.fill( 8 ) { UInt( width = 8 ) }

  val io = new Bundle {
    val enq = Decoupled( genType ).flip
    val deq = Decoupled( genType )
  }

  val lastByte = (bytesOut/8).toInt
  val padSize = ( bytesOut % 8 )
  val bufferCnt = RegInit( UInt(0, log2Up(lastByte) + 1 ) )
  val regCnt = RegInit( UInt(0, 4 ) )
  io.enq.ready := io.deq.ready
  val regBuf = Vec.fill( 8 ) { RegInit( UInt( 0, 8 ) ) }
  val outMod = Vec.fill( 8 ) { UInt( width = 8 ) }
  val bitsIdx = ( 0 until 8 ).map( x => UInt( width = 3 ) )

  for ( idx <- 0 until 8 ) {
    bitsIdx(idx) := ( UInt( idx, 3 ) + regCnt )
    outMod( idx ) := io.enq.bits( bitsIdx(idx) )
    when ( UInt(idx) < regCnt ) {
      outMod( idx ) := regBuf( bitsIdx(idx) )
    }
    when ( io.enq.valid && io.enq.ready ) {
      regBuf(idx) := io.enq.bits( idx )
    }
  }

  when ( io.enq.valid && io.enq.ready ) {
    bufferCnt := bufferCnt + UInt( 1 )
  }

  when ( bufferCnt === UInt( lastByte ) && io.enq.valid && io.deq.ready ) {
    // pad the data
    io.enq.ready := ( regCnt < UInt( padSize ) )
    bufferCnt := UInt( 0 )
    regCnt := regCnt - UInt( padSize )
    when ( regCnt < UInt( padSize ) ) {
      regCnt := regCnt + UInt( 8 - padSize )
    }
  }
  val deqVld = Reg( Bool() )
  val deqBits = Reg( outMod )
  io.deq.bits := deqBits
  io.deq.valid := deqVld
  when ( io.deq.ready ) {
    deqVld := io.enq.valid
    deqBits := outMod
  }
}
