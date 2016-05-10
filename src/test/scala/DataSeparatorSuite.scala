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
      val cycles = 40
      var idx = 0
      var outIdx = 0
      var pkts = 0
      val myRand = new Random
      for ( cyc <- 0 until cycles ) {
        poke( c.io.enq.bits, ( idx until idx + 8).map( BigInt(_) ).toArray.reverse )
        val vld = ( myRand.nextInt( 5 ) != 0 )
        poke( c.io.enq.valid, vld )
        poke( c.io.deq.ready, true )
        val rdy = ( peek( c.io.enq.ready ) == BigInt(1) )
        if ( vld && rdy ) {
          idx = idx + 8
        }

        val outVld = ( peek( c.io.deq.valid ) == BigInt(1) )
        if ( outVld ) {
          for ( tmpIdx <- (0 until 8).reverse ) {
            if ( (pkts + 1)*c.bytesOut > ( 7 - tmpIdx + outIdx ) ) {
              expect( c.io.deq.bits( tmpIdx ),  BigInt( 7 - tmpIdx + outIdx ) )
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
        step(1)
      }
    }
    for ( noBO <- Array( 100 ) ) {
      chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
        Module( new DataSeparator( noBO ) ) }) { c => new DataSepTests( c ) }
    }
  }

}
