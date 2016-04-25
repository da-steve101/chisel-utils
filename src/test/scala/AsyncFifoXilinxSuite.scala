import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

import Chisel._
import chiselutils.xilinx._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class AsyncFifoXilinxSuite extends TestSuite {

  @Test def testFifo() {

    class UserTop extends Module {
      val io = new QueueIO( Vec.fill( 16 ) { UInt( width = 8 ) }, 16 )
      val enqClk = Clock()
      val deqClk = Clock()
      val testFifo = Module( new AsyncFifoXilinx( Vec.fill( 16 ) { UInt( width = 8 ) },
        16, enqClk, deqClk ) )
      testFifo.io <> io
    }

    class AsyncQueueTests( c : UserTop ) extends Tester( c ) {

      val cycles = 20
      val myRand = new Random
      val values = ArrayBuffer.fill(cycles) { Array.fill( 16 ) { BigInt(myRand.nextInt( 1 << 8 )) } }
      var deqCount = 0

      poke( c.io.enq.bits, values(0) )
      poke( c.io.enq.valid, false )
      poke( c.io.deq.ready, false )

      step(1) // need this here as two clocks

      for ( cyc <- 0 until cycles ) {

        poke( c.io.enq.bits, values(cyc) )
        poke( c.io.enq.valid, true )
        val deqR = ( myRand.nextInt(2) == 0 )
        poke( c.io.deq.ready, deqR )

        step(1)

        val deqVal = ( peek( c.io.deq.valid ) == 1 )
        if( deqVal && deqR ) {
          values(deqCount).zipWithIndex.map( x => {
            expect( c.io.deq.bits(x._2), x._1 )
          } )
          deqCount = deqCount + 1
        }

      }
    }
    launchCppTester( ( c : UserTop ) => new AsyncQueueTests( c ) )
  }

}
