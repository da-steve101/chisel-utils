
package chiselutils.utils

import Chisel._

object MemShiftRegister {

  def apply[ T <: Data ]( in : T, n : Int, en : Bool = Bool(true) ) : T = {
    val memSR = Module( new MemShiftRegister( in, n ) )
    memSR.io.en := en
    memSR.io.in := in
    memSR.io.out
  }

}

class MemShiftRegister[ T <: Data ]( genType : T, n : Int ) extends Module {
  val io = new Bundle {
    val in = genType.cloneType.asInput
    val en = Bool( INPUT )
    val out = genType.cloneType.asOutput
  }

  if ( n <= 2 ) {
    val reg1 = Reg( genType )
    val reg2 = Reg( genType )
    // use this as en is implemented differently in a shift register
    // in ShiftRegister en is just on input
    when ( io.en ) {
      reg1 := io.in
      reg2 := reg1
    }
    io.out := {
      if ( n == 2 )
        reg2
      else if ( n == 1 )
        reg1
      else
        io.in
    }
  } else {
    val myMem = Mem( n - 1, genType )

    // put a register at the front and back
    val regTop = Reg( genType )
    val regBot = Reg( genType )
    val cntr = Counter( io.en, n - 1 )
    val incrAddr = cntr._1 + UInt(1)
    val readAddr = UInt( width = incrAddr.getWidth )

    readAddr := incrAddr
    when ( cntr._1 === UInt( n - 2 ) ) {
      readAddr := UInt( 0 )
    }

    when ( io.en ) {
      myMem.write( cntr._1, regTop )
      regTop := io.in
      regBot := myMem( readAddr )
    }
    io.out := regBot
  }
}
