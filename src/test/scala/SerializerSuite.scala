import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

import Chisel._
import chiselutils.utils._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

class SerializerSuite extends TestSuite {

  @Test def testSerailizer {
    class UserMod( val vecInSize : Int, val vecOutSize : Int) extends Module {
      val io = new Bundle {
        val dataIn = Decoupled( Vec.fill( vecInSize ) { UInt(INPUT, 4) } ).flip
        val flush = Bool(INPUT)
        val dataOut = Valid( Vec.fill( vecOutSize ) { UInt(OUTPUT, 4) } )
        val flushed = Bool(OUTPUT)
      }
      val genType = UInt( width = 4 )
      val serMod = Module(new Serializer(genType, vecInSize, vecOutSize))
      io <> serMod.io
    }

    class UserTests(c : UserMod, cycles : Int) extends Tester(c) {
      val myRand = new Random
      val inputData = ArrayBuffer.fill( cycles ) { ArrayBuffer.fill( c.vecInSize ) { myRand.nextInt(16) } }
      var count = 0;
      var countOld = count
      var lastVld = false
      var outCount = 0;
      var flushOutCount = count*c.vecInSize
      while ( count < cycles ) {
        val inputVld = if ( count != countOld || !lastVld) (myRand.nextInt(5) != 0) else lastVld
        lastVld = inputVld
        val flush = (myRand.nextInt(15) == 0)
        if ( flush ) {
          flushOutCount = { if (inputVld) (count + 1)*c.vecInSize else count*c.vecInSize }
          println("flushOutCount = " + flushOutCount)
        }
        poke( c.io.flush, flush )
        poke( c.io.dataIn.valid, inputVld )
        (0 until c.vecInSize).foreach( i => poke( c.io.dataIn.bits(i), inputData(count)(i) ) )
        val outputValid = peek( c.io.dataOut.valid )
        val flushed = ( peek( c.io.flushed ) == BigInt(1) )
        if ( outputValid == 1) {
          for ( i <- 0 until c.vecOutSize ) {
            println("outCount = " + outCount)
            val outCyc = (outCount - (outCount % c.vecInSize)) / c.vecInSize
            if ( ( flushed && ( outCount < flushOutCount ) ) || !flushed ) {
              expect( c.io.dataOut.bits(i), inputData( outCyc )( outCount - (outCyc * c.vecInSize )) )
              outCount = outCount + 1
            } else
              peek( c.io.dataOut.bits(i) )
          }
        }
        val ready = peek(c.io.dataIn.ready)
        countOld = count
        if ( ready == 1 && inputVld ) {
          count = count + 1
        }
        step(1)
      }
    }

    for ( vecOutSize <- 1 until 20 ) {
      for ( vecInSize <- 1 until 20 ) {
        chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => Module(
          new UserMod( vecInSize, vecOutSize )) ) { c => new UserTests(c, scala.math.max( vecOutSize, vecInSize )*5 ) }
      }
    }
  }
}
