import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import Chisel._
import chiselutils.interfaces.exanicx4._

class DataCombinerSuite extends TestSuite {

  @Test def dataCombinerTest {

    class DataCombinerTests( c : DataCombiner ) extends Tester( c ) {
      val cycles = 40
      val myRand = new Random
      val expectedOut = new ArrayBuffer[Int]()
      var idx = 0

      for ( cyc <- 0 until cycles ) {
        val len = myRand.nextInt(9)
        val inputData = Array.fill(len) { myRand.nextInt(1 << 8) }
        val padData =  inputData ++ Array.fill( 8 - len ){ 0 }
        val vld = ( len != 0 )

        expectedOut.appendAll(inputData)

        poke( c.io.dataIn, padData.map( x => BigInt(x) ) )
        poke( c.io.vld, vld )
        poke( c.io.len, len )

        val vldOut = ( expectedOut.length - 8*idx >= 8 )
        expect( c.io.vldOut, vldOut )
        if ( vldOut ) {
          expectedOut.drop(8*idx).take(8).toArray.zipWithIndex.map( x => {
            expect( c.io.dataOut(x._2), x._1 )
          })
          idx = idx + 1
        }
        step(1)
      }
    }
    chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
      Module( new DataCombiner ) }) { c => new DataCombinerTests( c ) }
  }

}

