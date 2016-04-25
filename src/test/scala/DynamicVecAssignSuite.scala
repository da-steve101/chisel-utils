import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

import Chisel._
import chiselutils.utils._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

class DynamicVecAssignSuite extends TestSuite {

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
        chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend",
          "c", "--targetDir", dir.getPath.toString()), () => Module(
          new UserMod( vecInSize, vecOutSize )) ) { c => new UserTests(c) }
      }
    }
  }

}
