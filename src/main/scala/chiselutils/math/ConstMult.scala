
package chiselutils.math

import Chisel._
import chiselutils.algorithms.RPAG

class ConstMultFixed( genType : Fixed, multVar : List[BigInt] ) extends Module {
  val io = new Bundle {
    val in = genType.cloneType.asInput
    val out = Vec( multVar.size, genType.cloneType ).asOutput
  }

  /* Preprocessing:
   * 1) Shift all numbers so that lsb is 1, if zero remove
   * 2) Remove all duplicates
   */
  val zeroIdxs = multVar.zipWithIndex.filter( _._1 == BigInt(0) ).map( _._2 )
  val zeroFilt = multVar.zipWithIndex.filter(_._1 != BigInt(0) )
  val shiftIdxs = zeroFilt.map( x => { ( x._1 >> x._1.lowestSetBit, x._1.lowestSetBit, x._2 ) })
  val dupIdxs = shiftIdxs.groupBy( _._1 )
  val t = dupIdxs.map( _._1 ).toList
  val addMapping = RPAG( t )
  val wOut = RPAG.implementAdder( UInt( width = genType.getWidth).fromBits( io.in ), t.max.toInt*4, addMapping, t )

  for ( z <- zeroIdxs )
    io.out(z) := Fixed(0, genType.getWidth(), genType.getFractionalWidth() )
  for ( i <- ( 0 until t.size ) ) {
    for ( d <- dupIdxs( t(i) ) )
      io.out(d._3) := Fixed( width = genType.getWidth(), fracWidth = genType.getFractionalWidth() ).fromBits(wOut(i)) << UInt(d._2)
  }
}
