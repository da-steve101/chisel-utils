import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import Chisel._
import chiselutils.math.SumScheduler

class SumSchedulerSuite extends TestSuite {

  def getConvSums( imgSize : Int, filterSize : Int ) : List[Set[(Int, Int)]] = {
    // no padding, 1 filter
    // stream pixel by pixel
    // only odd filter
    // need to flip
    // input is 3 by 3 => positions 0 to 8
    // columns are x, rows are y
    val positions = ( 0 until filterSize*filterSize )
    val outSums = ( 0 until imgSize ).map( x => {
      ( 0 until imgSize ).map( y => {
        positions.map( p => {
          val px = ( p % filterSize ) - ( filterSize / 2 )
          val py = ( p / filterSize ) - ( filterSize / 2 )
          val xpx = x + px
          val ypy = y + py
          ( xpx, ypy, p )
        }).filter { case ( xpx, ypy, p ) => {
          ( xpx >= 0 && xpx < imgSize && ypy >= 0 && ypy < imgSize )
        }}.map { case ( xpx, ypy, p ) => {
          ( ypy*imgSize + xpx, p )
        }}.toSet
      })
    }).reduce( _ ++ _ ).toList
    outSums
  }
/*
  @Test def testIntersectGain() {
    val setA = ( 0 until 5 ).toSet[Int]
    val setB = setA.map( x => x + 1 )
    assertEquals( 2, SumScheduler.intersectGain( setA, setB ), 0 )
  }

  @Test def testSumGain() {
    val outSums = getConvSums( 5, 3 )
    val sumToTry = List( (12, 13, 9, 5), (11, 8, 9, 9), ( 0, 24, -5, 24 ), ( 2, 3, 6, 5 ) )
    for ( s <- sumToTry ) {
      val sumA = outSums(s._1).groupBy( _._1 ).map( x => { ( x._1, x._2.map( _._2 ) ) })
      val sumB = outSums(s._2).groupBy( _._1 ).map( x => { ( x._1, x._2.map( _._2 ) ) })
      val sumGain = SumScheduler.sumGain( sumA, sumB )
      assertEquals( s._3, sumGain._1, 0 )
      assertEquals( s._4, sumGain._2)
    }
  }

  @Test def testGainMatSwap() {
    val outSums = getConvSums( 5, 3 )
    val gainMat = SumScheduler.getSumGainComb( outSums )
    val posA = 8
    val posB = 16
    val gainSwapped = SumScheduler.gainMatSwapPos( gainMat, posA, posB )
    val permutedList : List[Int] = {
      (0 until posA).toList ++ List( posB ) ++
      (posA + 1 until posB).toList ++ List( posA ) ++
      ( (posB + 1) until outSums.size ).toList
    }
    val gainMat2 = SumScheduler.getSumGainComb( permutedList.map( x => outSums(x) ) )
    assertEquals( gainSwapped, gainMat2 )
  }

  @Test def testMatrixMove() {
    val outSums = getConvSums( 5, 3 )
    val gainMat = SumScheduler.getSumGainComb( outSums )
    gainMat.foreach( g => println(g) )
    gainMat.foreach( x => println( x.reduce( _ + _ )/x.size ) )
  }
 */
  @Test def testMatrixSort() {
    val outSums = getConvSums( 5, 3 )
    val gainMat = SumScheduler.getSumGainComb( outSums )
    gainMat.foreach( g => println(g) )
    val sorted = SumScheduler.gainMatSort( gainMat )
    sorted.foreach( g => println(g) )
  }



  /*
   chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
   Module( new ConstMult( bw, fw, numsIn.toList ) ) }) { c => new ConstTester( c ) }
   */
}
