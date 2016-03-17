package chiselutils.utils

import Chisel._

/** This object allows the assignment of a dynamic section of a vector to another
  * Allowing vecOut(5,1) := vecIn(6, 2), where the indexs can be dynamically specified
  */
object DynamicVecAssign {
  def apply[T <: Data]( vecOut : Vec[T], hiOut : UInt, loOut : UInt, vecIn : Vec[T],  hiIn : UInt, loIn : UInt ) : Unit = {
    val vecOutLen = vecOut.length
    val vecInLen = vecIn.length
    val vecOutBW = log2Up(vecOutLen)
    val vecInBW = log2Up(vecInLen)
    val maxWidth = if ( vecInBW > vecOutBW ) vecInBW else vecOutBW
    if ( vecOutLen == 0 || vecInLen == 0 ) {
      ChiselError.error("The vectors cannot have a width of 0")
    }
    for ( i <- 0 until vecOutLen ) {
      val inIdx = loIn + UInt(i, maxWidth) - loOut
      when ( hiOut >= UInt(i, vecOutBW) && loOut <= UInt(i, vecOutBW) ) {
        vecOut(UInt(i, vecOutBW)) := vecIn(inIdx(vecInBW - 1, 0))
      }
    }
  }
}
