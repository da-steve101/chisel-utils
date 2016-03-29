package chiselutils.interfaces.exanicx4

import Chisel._

/** This module joins incomplete frames together back to back
  * based on the len of each frame
  * len = 0 => 8
  */
class DataCombiner extends Module {
  val io = new Bundle {
    val dataIn = Vec.fill(8) { UInt(INPUT, 8) }
    val vld = Bool(INPUT)
    val len = UInt(INPUT, 3)
    val dataOut = Vec.fill(8) { UInt(OUTPUT, 8) }
    val vldOut = Bool(OUTPUT)
  }

  // only need to store 7 as 8 should pass through
  val store = Reg( Vec.fill(7) { UInt( width = 8 ) } )
  val count = RegInit( UInt(0, 3) )

  // total amount of bytes available
  val total = UInt( width = 4 )
  total := UInt(0, 1) ## count
  when ( io.vld ) {
    total := ( UInt(0, 1) ## count ) + io.len
  }

  when( io.vld ) {
    // need to add 1 as len is from 0
    count := total
    when ( io.vldOut ) {
      count := count + io.len
    }
    for ( idx <- 0 until 7 ) {
      when ( ( UInt(idx, 3) >= count ) && ( UInt(idx, 3) <= count + io.len ) ) {
        store(idx) := io.dataIn( UInt( idx, 3 ) - count )
      }
      when ( io.vldOut ) {
        store(idx) := io.dataIn( UInt( 8, 4 ) - count + UInt(idx, 3) )
      }
    }
  }

  io.vldOut := ( total > UInt( 7, 4 ) || ( io.vld && io.len === UInt( 0, 3 ) ) )

  // dataout = dataIn(8 - count, 0) ## store(count - 1, 0)
  for ( idx <- 0 until 8 ) {
    if ( idx < 7 )
      io.dataOut(idx) := store(idx)
    else
      io.dataOut(idx) := UInt(0, 8)
    when ( UInt(idx, 3) >= count ) {
      io.dataOut(idx) := io.dataIn( UInt(idx, 4) - count )
    }
  }

  printf("total = %d\n", total)
  printf("count = %d\n", count)
  printf("len = %d\n", io.len)
}
