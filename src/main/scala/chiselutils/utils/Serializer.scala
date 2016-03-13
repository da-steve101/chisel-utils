/** This file provides blocks for convienient vector manipulations
  */

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
    println("vecInSize = " + vecInLen + ", vecOutSize = " + vecOutLen)
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

/** This block provides a conversion from a vector of one width to another
  * The interface has a bits/valid designed to pull input from a fifo
  * The output is bits/valid
  */
class Serializer[T <: Data]( genType : T, widthIn : Int, widthOut : Int) extends Module {
  if ( widthIn < 1 || widthOut < 1 ) {
    ChiselError.error("Widths of input and output vectors must be greater than 0")
  }

  val io = new Bundle {
    val dataIn = Decoupled(Vec.fill(widthIn) { genType.cloneType }).flip
    val dataOut = Valid(Vec.fill(widthOut) { genType.cloneType })
  }
  val inBW = log2Up(widthIn)
  val outBW = log2Up(widthOut)
  println("outBW = " + outBW)
  println("inBW = " + inBW)

  // compute LCM then attach to vec of combined

  // first trivial case is widthIn == widthOut
  if ( widthIn == widthOut ) {
    io.dataOut.valid := io.dataIn.valid
    io.dataOut.bits := io.dataIn.bits
    io.dataIn.ready := Bool(true)
  }
  // second case is if widthIn < widthOut
  if ( widthIn < widthOut ) {

    // Special case, if widthIn is a multiple of widthOut don't need as much wiring
    if ( widthOut % widthIn == 0 ) {
      // as is a multiple we don't need to store the last widthIn numbers, hence reg is widthOut - widthIn in size
      val tmpReg = Reg( Vec.fill( widthOut - widthIn ) { genType.cloneType } )
      val stages = widthOut/widthIn
      val stagesBW = log2Up(stages)
      val stageNo = RegInit( UInt(0, width = stagesBW ) )
      for ( i <- 0 until ( stages - 1 ) ) {
        when ( stageNo === UInt(i, stagesBW) ) {
          for ( j <- 0 until widthIn ) {
            tmpReg(i*widthIn + j) := io.dataIn.bits(j)
          }
        }
      }
      for ( i <- 0 until ( widthOut - widthIn ) ) {
        io.dataOut.bits(i) := tmpReg(i)
      }
      for ( i <- 0 until widthIn ) {
        io.dataOut.bits(i + widthOut - widthIn) := io.dataIn.bits(i)
      }
      io.dataOut.valid := Bool( false )
      when ( io.dataIn.valid ) {
        stageNo := stageNo + UInt( 1, stagesBW )
        when ( stageNo === UInt( stages - 1, stagesBW ) ) {
          io.dataOut.valid := Bool( true )
          stageNo := UInt( 0, stagesBW )
        }
      }
      io.dataIn.ready := Bool(true)
    } else {
      // In the general case, we need to be able to wire each value to anywhere in the output
      // There will also be left over Input to be wrapped around
      val outPos = RegInit(UInt(0, width = outBW)) // the position we are upto in the output reg
      val outPosNext = outPos + UInt(widthIn, outBW + 1) // the next position after this clock cycle
      val excess = outPos - UInt( widthOut - widthIn, outBW ) // When the last of the output is filled in, how many of the input are left
      val remaining = UInt( widthOut, outBW + 1) - outPos // the number of spots remaining
      val filled = ( UInt( widthOut - widthIn, outBW ) <= outPos ) && io.dataIn.valid // Indicates if the output was just filled

      // at least 1 value can come directly from the input
      val tmpReg = Reg( Vec.fill( widthOut - 1 ) { genType.cloneType } )
      // a vec representing the mix from the input and register
      val tmpOut = Vec.fill( widthIn ) { genType.cloneType }

      // assign tmpOut from tmpReg and input
      for ( i <- 0 until widthIn ) {
        when ( excess > UInt(i, inBW) ) {
          tmpOut(i) := tmpReg( UInt( widthOut - widthIn, outBW ) + UInt( i, outBW ) )
        } .otherwise {
          tmpOut(i) := io.dataIn.bits( UInt(i, inBW) - excess )
        }
      }

      // assign the output
      for ( i <- 0 until ( widthOut - widthIn ) ) {
        io.dataOut.bits(i) := tmpReg(i)
      }
      for ( i <- 0 until widthIn ) {
        io.dataOut.bits( i + widthOut - widthIn ) := tmpOut(i)
      }
      io.dataOut.valid := filled
      io.dataIn.ready := Bool(true)

      // update the output position
      when ( io.dataIn.valid ) {
        outPos := outPosNext
        when ( filled ) {
          outPos := excess
        }
      }

      val hiOut = UInt( width = outBW )
      val loOut = UInt( width = outBW )
      val hiIn = UInt( width = inBW )
      val loIn = UInt( width = inBW )

      when ( io.dataIn.valid && !( filled && ( excess === UInt( 0, outBW ) ) )) {
        DynamicVecAssign( tmpReg, hiOut, loOut, io.dataIn.bits, hiIn, loIn )
      }

      hiOut := outPosNext - UInt(1, outBW)
      loOut := outPos
      hiIn := UInt( widthIn - 1, inBW )
      loIn := UInt( 0, inBW )

      when ( filled ) {
        // excess > 0 for vec assign to be used
        hiOut := excess - UInt(1, outBW)
        loOut := UInt(0, outBW)
        loIn := remaining
      }
    }
  }
  // final case if widthIn > widthOut
  if ( widthIn > widthOut ) {

    // Special case, if widthOut is a multiple of widthIn don't need as much wiring
    if ( widthIn % widthOut == 0 ) {
      val stages = widthIn/widthOut
      val stagesBW = log2Up(stages)
      val stageNo = RegInit( UInt(0, stagesBW ) )
      // default io.dataOut.bits
      for ( j <- 0 until widthOut ) {
        io.dataOut.bits(j) := io.dataIn.bits(j)
      }
      for ( i <- 0 until stages ) {
        when ( stageNo === UInt( i, stagesBW ) ) {
          for ( j <- 0 until widthOut ) {
            io.dataOut.bits(j) := io.dataIn.bits(i*widthOut + j)
          }
        }
      }
      io.dataIn.ready := Bool(false)
      io.dataOut.valid := io.dataIn.valid
      when ( io.dataIn.valid ) {
        stageNo := stageNo + UInt(1, stagesBW)
        when ( stageNo === UInt(stages - 1, stagesBW) ) {
          stageNo := UInt(0, stagesBW)
          io.dataIn.ready := Bool(true)
        }
      }
    } else {
      // Store the last widthOut - 1 values of width In
      val tmpReg = Reg( Vec.fill( widthOut - 1 ) { genType.cloneType } )
      val inPos = RegInit( UInt( 0, inBW ) )
      val remaining = UInt( widthOut, inBW + 1 ) - inPos
      val inPosNext = inPos + UInt( widthOut, inBW + 1 )
      val leftOver = RegInit( Bool(false) )
      val used = { ( !leftOver && ( UInt( widthIn - widthOut, inBW + 1) < inPosNext ) && io.dataIn.valid ) ||
        leftOver && ( UInt( widthIn - widthOut, inBW + 1 ) < inPos ) && io.dataIn.valid }
      when ( io.dataIn.valid ) {
        printf("inPos = %x, inPosNext = %x, used = %x, leftOver = %x, remaining = %x\n", inPos, inPosNext, used, leftOver, remaining)
        for ( i <- 0 until (widthOut - 1) ) {
          tmpReg(i) := io.dataIn.bits( widthIn - widthOut + 1 + i )
          printf("tmpReg(" + i + ") = %d\n", tmpReg(i))
          printf("tmpReg(" + i + ").next = %d\n", io.dataIn.bits( widthIn - widthOut + 1 + i ))
        }
        inPos := inPosNext
      }
      val tmpOut = Vec.fill( widthOut ) { genType.cloneType }
      for ( i <- 0 until widthOut ) {
        val tmpRegIdx = (inPos - UInt(1, inBW)) + UInt(i, outBW) 
        when ( UInt(i, outBW ) < remaining ) {
          tmpOut(i) := tmpReg( tmpRegIdx( outBW - 1, 0 ) )
          printf("tmpOut(" + i + ") = tmpReg(%d) = %d\n", tmpRegIdx( outBW - 1, 0 ), tmpOut(i))
        } .otherwise {
          tmpOut(i) := io.dataIn.bits( UInt( i, inBW ) - remaining )
          printf("tmpOut(" + i + ") = io.dataIn.bits(%d) = %d\n", UInt( i, inBW ) - remaining, io.dataIn.bits( UInt( i, inBW ) - remaining ))
        }
      }
      when ( leftOver && io.dataIn.valid) {
        leftOver := Bool(false)
        inPos := inPos
      }
      when ( used ) {
        when( inPosNext =/= UInt( widthIn, inBW + 1 ) ) {
          leftOver := Bool(true)
          inPos := inPosNext - UInt( widthIn - widthOut, inBW + 1 )
          when ( leftOver ) {
            inPos := inPosNext - UInt( widthIn, inBW + 1 )
          }
        } .otherwise {
          inPos := UInt( 0, inBW )
        }
      }
      io.dataIn.ready := used
      io.dataOut.valid := io.dataIn.valid
      // dynVec with default value
      val dynVecOut = Vec.fill( widthOut ) { genType.cloneType }
      for ( i <- 0 until widthOut ) {
        dynVecOut( i ) := tmpOut( i )
      }
      DynamicVecAssign( dynVecOut, UInt( widthOut - 1, outBW ), UInt( 0, outBW ), io.dataIn.bits, inPosNext - UInt( 1, inBW ), inPos )
      io.dataOut.bits := dynVecOut
      when ( leftOver ) {
        io.dataOut.bits := tmpOut
      }
    }
  }
}
