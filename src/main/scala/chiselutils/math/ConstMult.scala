
package chiselutils.math

import Chisel._
import chiselutils.algorithms.RPAG
import chiselutils.algorithms.PmcmILP

class ConstMult( bitWidth : Int, fracWidth : Int, multVar : List[BigInt], useIlp : Boolean = true ) extends Module {
  val io = new Bundle {
    val in = Fixed( INPUT, bitWidth, fracWidth )
    val out = Vec( multVar.size, Fixed( OUTPUT, bitWidth, fracWidth ) )
  }

  /* Preprocessing:
   * 1) If zero remove
   * 2) Make all positive
   * 3) Shift all numbers so that lsb is 1
   * 4) Remove duplicates
   */
  val zeroFilt = multVar.zipWithIndex.filter(_._1 != BigInt(0) )
  val negIdxs = zeroFilt.map( x => ( x._1.abs, x._2, x._1 < 0 ) )
  val shiftIdxs = negIdxs.map( x => { ( x._1 >> x._1.lowestSetBit, x._1.lowestSetBit, x._2 ) })
  val dupIdxs = shiftIdxs.groupBy( _._1 )
  val t = dupIdxs.map( _._1 ).toList
  val addMapping = {
    if ( useIlp )
      PmcmILP.solveILP( t )
    else
      RPAG( t )
  }
  val xIn = SInt( width = bitWidth ).fromBits( io.in )
  val wOut = RPAG.implementAdder( xIn, addMapping, t )

  def latency : Int = {
    if ( addMapping.size > 1 )
      addMapping.size + 1
    else {
      if ( addMapping(0).filter( x => ( x._1 == x._2 && x._2 == x._3 )).size ==
        addMapping(0).size )
        1
      else
        2
    }
  }

  // default to 0
  for ( z <- (0 until multVar.size) )
    io.out(z) := Fixed(0, bitWidth, fracWidth )
  for ( i <- ( 0 until t.size ) ) {
    val wo = wOut(i)
    for ( d <- dupIdxs( t(i) ) ) {
      val woMod = { if ( negIdxs.find( _._2 == d._3).get._3 ) -wo else wo } << UInt(d._2) // undo shift right
      // after multiply in fixed need to shift right
      val woLo = fracWidth
      val woHigh = woLo + bitWidth - 1
      val woModWidth = woMod.getWidth()
      val woTrim = {
        if ( woModWidth > woHigh )
          woMod( woHigh, woLo )
        else {
          val sext = ( 0 until woHigh - woModWidth + 1).map( x => woMod(woModWidth - 1) ).reduce( _ ## _ )
          sext ## woMod( woModWidth - 1, woLo )
        }
      }
      val fv = Fixed( width = bitWidth, fracWidth = fracWidth ).fromBits(woTrim)
      io.out(d._3) := fv
    }
  }
}
