import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import Chisel._
import chiselutils.utils.MemShiftRegister

class MemShiftRegisterSuite extends TestSuite {

  class UserMod( val n : Int ) extends Module {
    val io = new Bundle {
      val in = Fixed( INPUT, 16, 8 )
      val en = Bool( INPUT )
      val out = Fixed( OUTPUT, 16, 8 )
    }

    val memSr = MemShiftRegister( io.in, n, io.en )
    io.out := memSr
  }

  class UserModTests( c : UserMod ) extends Tester( c ) {
    val inputs = ArrayBuffer[BigInt]()
    val myRand = new Random
    val cycles = 3*( c.n + 10 )
    var outIdx = -c.n
    for ( cyc <- 0 until cycles ) {
      val en = { myRand.nextInt(10) != 0 }
      val in = myRand.nextInt( 1 << 16 )
      poke( c.io.in, in )
      poke( c.io.en, en )
      if ( en ) {
        inputs += BigInt( in )
        if ( outIdx >= 0 )
          expect( c.io.out, inputs(outIdx) )
        outIdx += 1
      }
      step(1)
    }
  }

  @Test def memSrTest {
    for ( n <- List(1, 2, 3, 5, 25, 73) ) {
      println( "Test sr with " + n )
      chiselMainTest( Array("--genHarness", "--compile", "--test", "--backend", "c"),
        () => Module( new UserMod( n ) ) ) { c => new UserModTests( c ) }
    }
  }

}
