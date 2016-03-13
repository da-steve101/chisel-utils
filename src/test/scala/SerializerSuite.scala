import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

import Chisel._
import chiselutils.utils._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

class SerializerSuite extends TestSuite {
/*
  @Test def testDynVec {
    class UserMod( val vecInSize : Int, val vecOutSize : Int) extends Module {
      val io = new Bundle {
        val vecIn = Vec.fill( vecInSize ) { UInt(INPUT, 4) }
        val hiIn = UInt(INPUT, log2Up(vecInSize))
        val loIn = UInt(INPUT, log2Up(vecInSize))
        val hiOut = UInt(INPUT, log2Up(vecOutSize))
        val loOut = UInt(INPUT, log2Up(vecOutSize))
        val vecOut = Vec.fill( vecOutSize ) { UInt(OUTPUT, 4) }
      }
      (0 until vecOutSize).foreach( i => io.vecOut(i) := UInt(0, 4) )
      DynamicVecAssign(io.vecOut, io.hiOut, io.loOut, io.vecIn, io.hiIn, io.loIn)
    }

    class UserTests(c : UserMod) extends Tester(c) {
      val myRand = new Random
      val vecLen = myRand.nextInt( scala.math.min( c.vecInSize, c.vecOutSize ) ) + 1
      val hiIn = myRand.nextInt( c.vecInSize + 1 - vecLen ) + vecLen - 1
      val loIn = hiIn + 1 - vecLen
      val hiOut = myRand.nextInt( c.vecOutSize + 1 - vecLen ) + vecLen - 1
      val loOut = hiOut + 1 - vecLen
      val vecIn = ArrayBuffer.fill( c.vecInSize ) { myRand.nextInt(16) }
      println("inSize = " + c.vecInSize + ", outSize = " + c.vecOutSize + ", hiIn = " + hiIn + ", hiOut =  " + hiOut + ", vecLen = " + vecLen)
      vecIn.zipWithIndex.map( x => poke(c.io.vecIn(x._2), x._1) )
      poke(c.io.hiIn, hiIn)
      poke(c.io.loIn, loIn)
      poke(c.io.hiOut, hiOut)
      poke(c.io.loOut, loOut)
      (0 until vecLen).foreach( x => expect(c.io.vecOut(x + loOut), vecIn(x + loIn ) ) )
    }

    for ( vecOutSize <- 1 until 20 ) {
      for ( vecInSize <- 1 until 20 ) {
        chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => Module(
          new UserMod( vecInSize, vecOutSize )) ) { c => new UserTests(c) }
      }
    }
  }
 */
  @Test def testSerailizer {
    class UserMod( val vecInSize : Int, val vecOutSize : Int) extends Module {
      val io = new Bundle {
        val dataIn = Decoupled( Vec.fill( vecInSize ) { UInt(INPUT, 4) } ).flip
        val dataOut = Valid( Vec.fill( vecOutSize ) { UInt(OUTPUT, 4) } )
      }
      val genType = UInt( width = 4 )
      val serMod = Module(new Serializer(genType, vecInSize, vecOutSize))
      io <> serMod.io
    }

    class UserTests(c : UserMod, cycles : Int) extends Tester(c) {
      val myRand = new Random
      val inputData = ArrayBuffer.fill( cycles ) { ArrayBuffer.fill( c.vecInSize ) { myRand.nextInt(16) } }
      var count = 0;
      var outCount = 0;
      while ( count < cycles ) {
        poke( c.io.dataIn.valid, true)
        (0 until c.vecInSize).foreach( i => poke( c.io.dataIn.bits(i), inputData(count)(i) ) )
        val outputValid = peek( c.io.dataOut.valid )
        if ( outputValid == 1) {
          for ( i <- 0 until c.vecOutSize ) {
            val outCyc = (outCount - (outCount % c.vecInSize)) / c.vecInSize
            expect( c.io.dataOut.bits(i), inputData( outCyc )( outCount - (outCyc * c.vecInSize )) )
            outCount = outCount + 1
          }
        }
        val ready = peek(c.io.dataIn.ready)
        if ( ready == 1 ) {
          count = count + 1
        }
        step(1)
      }
    }

    for ( vecOutSize <- 2 until 20 ) {
      for ( vecInSize <- 2 until 20 ) {
        chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => Module(
          new UserMod( vecInSize, vecOutSize )) ) { c => new UserTests(c, scala.math.max( vecOutSize, vecInSize )*5 ) }
      }
    }
  }
}
