
package chiselutils.math

import Chisel._
import chiselutils.algorithms.RPAG

class ConstMult( genType : Fixed, multVar : List[BigInt] ) extends Module {
  val io = new Bundle {
    val in = genType.cloneType.asInput
    val out = Vec( multVar.size, genType.cloneType ).asOutput
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
          val sext = ( 0 until woHigh - woModWidth + 1).map( x => woMod(woModWidth - 1) ).reduce( _ ## _ )
          sext ## woMod( woModWidth - 1, woLo )
        }
      }
      val fv = genType.cloneType.fromBits(woTrim)
      io.out(d._3) := fv
    }
  }
}
