
package chiselutils.math

import Chisel._
import chiselutils.algorithms.RPAG

class ConstMult( genType : Fixed, multVar : List[BigInt] ) extends Module {
  val io = new Bundle {
    val in = genType.cloneType.asInput
    val out = Vec( multVar.size, genType.cloneType ).asOutput
  }

  /* Preprocessing:
   * 1) Shift all numbers so that lsb is 1, if zero remove
   * 2) Remove all duplicates
   */
  val zeroFilt = multVar.zipWithIndex.filter(_._1 != BigInt(0) )
  val negIdxs = zeroFilt.map( x => ( x._1.abs, x._2, x._1 < 0 ) )
  val shiftIdxs = negIdxs.map( x => { ( x._1 >> x._1.lowestSetBit, x._1.lowestSetBit, x._2 ) })
  val dupIdxs = shiftIdxs.groupBy( _._1 )
  val t = dupIdxs.map( _._1 ).toList
  val addMapping = RPAG( t )
  val xIn = SInt( width = genType.getWidth() ).fromBits( io.in )
  val wOut = RPAG.implementAdder( xIn, t.max.toInt*4, addMapping, t )

  // default to 0
  for ( z <- (0 until multVar.size) )
    io.out(z) := Fixed(0, genType.getWidth(), genType.getFractionalWidth() )
  for ( i <- ( 0 until t.size ) ) {
    val wo = wOut(i)
    for ( d <- dupIdxs( t(i) ) ) {
      val woMod = { if ( negIdxs.find( _._2 == d._3).get._3 ) -wo else wo } << UInt(d._2) // undo shift right
      // after multiply in fixed need to shift right
      val woLo = genType.getFractionalWidth()
      val woHigh = woLo + genType.getWidth() - 1
      val woModWidth = woMod.getWidth()
      val woTrim = {
        if ( woModWidth > woHigh )
          woMod( woHigh, woLo )
        else {
          // TODO: find a nicer way to do sign extend
          val sext = Mux( woMod(woModWidth - 1), SInt( -1, woHigh - woModWidth + 1 ), SInt( 0, woHigh - woModWidth + 1 ) )
          sext ## woMod( woModWidth - 1, woLo )
        }
      }
      val fv = Fixed( width = genType.getWidth(), fracWidth = genType.getFractionalWidth() ).fromBits(woTrim)
      io.out(d._3) := fv
    }
  }
}
