import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import Chisel._
import chiselutils.interfaces.exanicx4._

class DataSeparatorSuite extends TestSuite {

  @Test def dataSep {

    class DataSepTests( c : DataSeparator ) extends Tester( c ) {
      val cycles = 150
      var idx = 0
      var outIdx = 0
      var pkts = 0
      val myRand = new Random
      poke( c.reset, true )
      step(3)
      poke( c.reset, false )
      for ( cyc <- 0 until cycles ) {
        val vld = ( myRand.nextInt( 5 ) != 0 )
        val deq = ( myRand.nextInt( 5 ) != 0 )
        if ( vld )
          poke( c.io.enq.bits, ( idx until idx + 8).map( BigInt(_) ).toArray )
        else
          poke( c.io.enq.bits, ( idx until idx + 8).map( x => BigInt(0) ).toArray )
        poke( c.io.enq.valid, vld )
        poke( c.io.deq.ready, deq )
        step(1)
        val rdy = ( peek( c.io.enq.ready ) == BigInt(1) )
        if ( vld && rdy ) {
          idx = idx + 8
        }

        val outVld = ( peek( c.io.deq.valid ) == BigInt(1) )
        if ( outVld && deq ) {
          for ( tmpIdx <- (0 until 8) ) {
            if ( (pkts + 1)*c.bytesOut > ( tmpIdx + outIdx ) ) {
              expect( c.io.deq.bits( tmpIdx ),  BigInt( tmpIdx + outIdx ) )
            } else {
              peek( c.io.deq.bits( tmpIdx ) )
            }
          }
          if ( outIdx + 8 > (pkts + 1)*c.bytesOut ) {
            pkts = pkts + 1
            outIdx = pkts*c.bytesOut
          } else
            outIdx = outIdx + 8
        }
      }
    }
    for ( noBO <- 100 until 101 ) {
      chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
        Module( new DataSeparator( noBO ) ) }) { c => new DataSepTests( c ) }
    }
  }

}
