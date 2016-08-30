import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import chiselutils.algorithms.RPAG
import chiselutils.exceptions.NoResultException
import Chisel._

class RpagSuite extends TestSuite {

  class UserMod( numsIn : List[BigInt], cMax : Int, bw : Int ) extends Module {
    val io = new Bundle {
      val xIn = UInt( INPUT, bw )
      val vecOut = Vec( numsIn.size, UInt( OUTPUT, bw ))
    }
    val addMapping = RPAG( numsIn )
    println( addMapping )
    val adderOut = RPAG.implementAdder( io.xIn, cMax, addMapping, numsIn )
    println( adderOut )
    io.vecOut := adderOut
  }

  @Test def csdTest {
    val numIn = ( 0 until 5000 ).map( x => BigInt(x) ).toList
    val numOut = numIn.map( x => RPAG.fromCsd( RPAG.toCsd( x ) ) ).toList
    (numIn zip numOut ).map( x => assertEquals( x._1, x._2 ) )
  }

  @Test def bestSingleTest {
    val numsIn = Set(  3, 9, 11 ).map( x => BigInt(x) )
    val bS = RPAG.bestSingle( numsIn, Set[BigInt](), 2, 16, 16 )
    val bS1 = RPAG.bestSingle( Set( BigInt(11) ), Set[BigInt](bS._1), 2, 16, 16 )
    println( bS )
    println( bS1 )
    assertEquals( bS._1 , BigInt(3) )
    assertEquals( bS1._1 , BigInt(1) )
  }

  @Test def bestPairTest {
    val numsIn = Set( 53, 89, 111 ).map( x => BigInt(x) )
    val expVal = intercept[NoResultException] { RPAG.bestSingle( numsIn, Set[BigInt](), 2, 16, 128 ) }
    assertEquals( expVal.getMessage, "No single candidates found" )
    val bp = RPAG.bestPair( numsIn, Set[BigInt](), 2, 16, 128 )
    println( bp )
    assertEquals( bp._1, BigInt(5) )
    assertEquals( bp._2, BigInt(3) )
    val bs = RPAG.bestSingle( numsIn, Set[BigInt](bp._1), 2, 16, 128 )
    println( bs )
    assertEquals( bs._1, BigInt(3) )
  }

  @Test def adderStructureTest {
    val numsIn = List(  3, 9, 11 ).map( x => BigInt(x) )
    val addMapping = RPAG( numsIn )
    println( addMapping )
    assertEquals( 1, 1 )
  }

  @Test def functionalSingleTest {
    val numsIn = List(  3, 9, 11 ).map( x => BigInt(x) )

    class UserModTests( c : UserMod ) extends Tester( c ) {
      poke( c.io.xIn, 5 )
      expect( c.io.vecOut(0), 15 )
      expect( c.io.vecOut(1), 45 )
      expect( c.io.vecOut(2), 55 )
    }

    chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
      Module( new UserMod( numsIn, 16, 8 ) ) }) { c => new UserModTests( c ) }

  }

  @Test def functionalPairTest {
    val numsIn = List( 53, 89, 111 ).map( x => BigInt(x) )

    class UserModTests( c : UserMod ) extends Tester( c ) {
      poke( c.io.xIn, 5 )
      expect( c.io.vecOut(0), 265 )
      expect( c.io.vecOut(1), 445 )
      expect( c.io.vecOut(2), 555 )
    }

    chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
      Module( new UserMod( numsIn, 128, 16 ) ) }) { c => new UserModTests( c ) }
  }

  @Test def randomLargeScaleTest {
    val myRand = new Random
    val numsGen = List.fill( 10 ) { myRand.nextInt(100) }
    val numsIn = numsGen.map( BigInt(_) ).map( x => (x >> x.lowestSetBit) ).distinct

    class UserModTests( c : UserMod ) extends Tester( c ) {
      println( "Using " + numsIn )
      for ( cyc <- ( 0 until 10 ) ) {
        val x = myRand.nextInt(100)
        poke( c.io.xIn, x )
        for ( n <- numsIn.zipWithIndex )
          expect( c.io.vecOut(n._2), x*n._1 )
      }
    }

    chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
      Module( new UserMod( numsIn, 128, 16 ) ) }) { c => new UserModTests( c ) }

  }
}
