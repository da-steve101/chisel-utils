
package chiselutils.math

import collection.mutable.ArrayBuffer
import Chisel._

/** A convolutional adder, takes in 1 pixel in each cycle
  * filterSize is the size of filter for image eg) (7, 7, 64)
  * imgSize is the size of the input image eg) ( 128, 128 )
  */
class ConvAdder( filterSize : (Int, Int, Int), imgSize : (Int, Int) ) extends Module {

  val io = new Bundle {
    val validIn = Bool( INPUT )
    val multIn = Vec( filterSize._1 * filterSize._2, Vec( filterSize._3, Fixed( INPUT, 16, 8 ) ) )
    val validOut = Bool( OUTPUT )
    val convOut = Vec( filterSize._3, Fixed( OUTPUT, 16, 8 ) )
  }

  val rowSumLat = filterSize._2 + 1 + ( (filterSize._2 - 2) / 2 )
  val colSumLat = imgSize._2 * ( filterSize._1 / 2 ) + 2

  def latency : Int = rowSumLat + colSumLat

  val rowAdder = ArrayBuffer[ List[ List[ Fixed ] ] ]()
  rowAdder += ( 0 until filterSize._1 ).map( rIdx => {
    io.multIn( rIdx*filterSize._2 ).toList.map( RegNext(_) )
  }).toList

  val muxConds = ArrayBuffer[ Bool ]()

  for ( idx <- 1 until filterSize._2 ) {
    val inputSelected = ( 0 until filterSize._1 ).map( rIdx => {
      io.multIn( rIdx*filterSize._2 + idx ).toList
    })

    val inputDelayed = {
      if ( idx == 1 )
        inputSelected
      else
        inputSelected.map( x => x.map( ShiftRegister( _, idx - 1 ) ) )
    }

    val prevInputs = rowAdder.last

    val adder = ( inputDelayed zip prevInputs ).map( x => {
      ( x._1 zip x._2 ).map( ab => RegNext( ab._1 + ab._2 ) )
    })

    val regDelay = {
      if ( idx <= filterSize._2 / 2 )
        inputDelayed.map( x => x.map( RegNext(_) ) )
      else
        prevInputs.map( x => x.map( RegNext(_) ) )
    }

    muxConds += Bool()
    rowAdder += ( adder zip regDelay ).map( x => {
      ( x._1 zip x._2 ).map( ab => {
        RegNext( Mux( muxConds.last, ab._2, ab._1) ) // If true then col not needed
      })
    }).toList
  }

  val noRowMuxs = muxConds.size

  val colAdder = ArrayBuffer[ List[ Fixed ] ]()
  colAdder += rowAdder.last( 0 ).map( ShiftRegister( _, 2 ) )

  for ( cIdx <- 1 until filterSize._1 ) {
    val inputSelected = rowAdder.last( cIdx )
    val srIn = colAdder.last.map( ShiftRegister( _, imgSize._2 - 2 ) )

    val inputDelayed = {
      if ( cIdx <= filterSize._1 / 2 )
        inputSelected.map( RegNext(_) )
      else
        srIn.map( RegNext( _ ) )
    }
    val adder = ( srIn zip inputSelected ).map( ab => {
      RegNext( ab._1 + ab._2 )
    })

    muxConds += Bool()
    val muxOut = ( adder zip inputDelayed ).map( ab => {
      RegNext( Mux( muxConds.last, ab._2, ab._1 ) )
    })

    colAdder += muxOut
  }

  val validSR = List( io.validIn ) ++ List.fill( latency ) { RegInit( Bool(false) ) }
  for ( vIdx <- 1 until validSR.size )
    validSR( vIdx ) := validSR( vIdx - 1 )

  for ( mIdx <- 0 until noRowMuxs ) {
    val initialVal = {
      if ( mIdx < filterSize._2/2 )
        ( 2*imgSize._2 - mIdx - 1 ) % imgSize._2
      else {
        val cutoff = (filterSize._2 / 2)
        ( 2*imgSize._2 - cutoff - 2*( mIdx - cutoff + 1) ) % imgSize._2
      }
    }

    val cntr = RegInit( UInt( initialVal, log2Up( imgSize._2 ) ) )
    when ( io.validIn ) {
      cntr := cntr + UInt( 1 )
    }
    when ( cntr === UInt( imgSize._2 - 1 ) ) {
      cntr := UInt( 0 )
    }

    // if right of filter
    if ( mIdx >= filterSize._2 / 2 )
      muxConds( mIdx ) := { cntr >= UInt( imgSize._2 - 1 - ( mIdx - (filterSize._2 / 2 ) ) ) }
    else
      muxConds( mIdx ) := { cntr <= UInt( 0 ) }
  }

  for ( cIdx <- 0 until muxConds.size - noRowMuxs ) {
    val mIdx = cIdx + noRowMuxs

    val initialVal = ( 2*imgSize._2 - rowSumLat - 1 ) % imgSize._2
    val initialRow = ( imgSize._1 - 1 - ( rowSumLat / imgSize._2 ) - {
      if ( cIdx < filterSize._1 / 2 )
        0
      else
        cIdx - ( filterSize._1 / 2 ) + 1
    } ) % imgSize._1

    val cntr = RegInit( UInt( initialVal, log2Up( imgSize._2 ) ))
    val rowCntr = RegInit( UInt( initialRow, log2Up( imgSize._1 ) ))
    when ( io.validIn ) {
      cntr := cntr + UInt( 1 )
    }
    when ( cntr === UInt( imgSize._2 - 1 ) ) {
      cntr := UInt( 0 )
      rowCntr := rowCntr + UInt( 1 )
      when ( rowCntr === UInt( imgSize._1 - 1 ) ) {
        rowCntr := UInt( 0 )
      }
    }

    // if below filter
    if ( cIdx >= filterSize._1 / 2 )
      muxConds( mIdx ) := { rowCntr >= UInt( imgSize._1 - 1 - ( cIdx - (filterSize._1 / 2 ) ) ) }
    else
      muxConds( mIdx ) := { rowCntr <= UInt( 0 ) }
  }

  io.convOut := Vec( colAdder.last )
  io.validOut := validSR.last
}
