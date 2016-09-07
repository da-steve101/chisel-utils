import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import Chisel._
import chiselutils.math.ConstMult

class ConstMultSuite extends TestSuite {
  val myRand = new Random
  val bw = 16
  val fw = 8
  val inputMax = 1 << 10
  val inputShift = 1 << 9
  val cyc = 15

  def testNums( numsIn : List[BigInt] ) = {
    class ConstTester( c : ConstMult ) extends Tester( c ) {
      val xIns = List.fill( cyc ) { myRand.nextInt( inputMax ) - inputShift }
      val expOuts = xIns.map( x => numsIn.map( n => (n*x) >> fw ) )
      for ( i <- ( 0 until cyc + c.latency ) ) {
        if ( i < cyc )
          poke( c.io.in, BigInt(xIns(i)) )
        if ( i >= c.latency ) {
          for ( n <- expOuts(i - c.latency).zipWithIndex )
            expect( c.io.out(n._2), n._1  )
        }
        step(1)
      }
    }

    chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
      Module( new ConstMult( bw, fw, numsIn.toList ) ) }) { c => new ConstTester( c ) }
  }

  @Test def zeroTest {
    val numsIn = List( 2, 0, 5, 22, 13, 0, 7, 53 ).map( BigInt(_) )
    testNums( numsIn )
  }

  @Test def dupTest {
    val numsIn = List( 2, 4, 5, 22, 13, 5, 7, 13, 53 ).map( BigInt(_) )
    testNums( numsIn )
  }

  @Test def negTest {
    val numsIn = List( 4, -4, 5, -22, 13, -7, 53 ).map( BigInt(_) )
    testNums( numsIn )
  }

  @Test def constMultTest {
    val numsGen = List.fill( 10 ) { myRand.nextInt(100) }
    val numsIn = numsGen.map( BigInt(_) )
    testNums( numsIn )
  }

  @Test def constMultLargeTest {
    val numsGen = List.fill( 3000 ) { myRand.nextInt(inputMax) - inputShift }
    println( "val numsGen = " + numsGen )
    val numsIn = numsGen.map( BigInt(_) )
    testNums( numsIn )
  }

  @Test def csdRepresentationTest {
    val numsGenList = List(List( 443, 321, 211, 253 ), List(-445, -42, -375, -113, 84, -107, -387, -232, 155, -279))
    for ( numsGen <- numsGenList ) {
      println( "val numsGen = " + numsGen )
      val numsIn = numsGen.map( BigInt(_) )
      testNums( numsIn )
    }
  }

  @Test def smallNumsTest {
    val numsGen = List(-1, -2, -3, -4, -5)
    val numsIn = numsGen.map( BigInt(_) )
    testNums( numsIn )
  }

}
