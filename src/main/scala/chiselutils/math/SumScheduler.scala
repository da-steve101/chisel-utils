
package chiselutils.math

import scala.collection.mutable.ArrayBuffer
import Chisel._

object SumScheduler {

  def intersectGain( setA : Set[Int], setB : Set[Int] ) : Float = {
    val intersectSize = setA.intersect(setB).size
    val totalSize = setA.size + setB.size - intersectSize
    val leftOver = ( setA.size - intersectSize ) + ( setB.size - intersectSize )
    val res = ( totalSize - 2*leftOver ).toFloat
    res
  }

  /** A map of cycles to each set of positions
    * O(n + m) where n, m are number of entries in map
    */
  def sumGain( sumA : Map[Int, Set[Int]], sumB : Map[Int, Set[Int]] ) : ( Float, Int ) = {
    // set to worst possible case where nothing matches
    var maxGain = {
      ( - sumA.map( sa => sa._2.size ).reduce( _ + _ ) -
        sumB.map( sb => sb._2.size ).reduce( _ + _ )).toFloat
    }
    var maxOff = 0
    val aIsBigger = sumA.keys.size > sumB.keys.size
    val bigMap = { if ( aIsBigger ) sumA else sumB }
    val smallMap = { if ( aIsBigger ) sumB else sumA }
    // hold small map still and slide the big map over
    val lowerLimit = -smallMap.keys.max + bigMap.keys.min
    val upperLimit = bigMap.keys.max - smallMap.keys.min + 1
    val smK = smallMap.keys.toList
    for ( kShift <- ( lowerLimit until upperLimit ) ) {
      val filtKey = smK.filter( x => bigMap.contains(x + kShift) )
      val gainList = filtKey.map( k => {
        val setA = smallMap.getOrElse( k, Set[Int]() )
        val setB = bigMap.getOrElse( k + kShift , Set[Int]() )
        intersectGain( setA, setB )
      })
      val gainInt = { if ( gainList.size == 0 ) 0 else gainList.reduce( _ + _ ) }
      val gain = gainInt - ( smallMap.size + bigMap.size - 2*filtKey.size )
      if ( gain > maxGain ) {
        maxGain = gain
        maxOff = kShift
      }
    }
    if ( aIsBigger )
      maxOff = -maxOff
    ( maxGain, maxOff )
  }

  /** break cp coords into pairwise interection gain */
  def getSumGainComb( sumStructure : List[Set[(Int, Int)]] ) : List[List[Float]] = {
    val sumAry = sumStructure.map( s => {
      s.groupBy( _._1 ).map( x => { ( x._1, x._2.map( _._2 ) ) })
    })
    ( 1 until sumAry.size ).map( i => {
      ( 0 until i ).map( j => {
        sumGain( sumAry(i), sumAry(j) )._1
      }).toList
    }).toList
  }

  /** find best combination of sums, returns gain and the set of sum idxs
    */
  def findBestComb( sumStructure : List[Set[(Int, Int)]] ) : ( Float, Set[Int] ) = {
    val sumPairs = getSumGainComb( sumStructure )
    println( "sumPairs = " + sumPairs )
    val sumSubsets = ( 0 until sumStructure.size ).toSet[Int].subsets.filter( _.size > 1 )
    val combGains = sumSubsets.map( s => {
      val sGain = s.map( x => {
        val g = s.filter( _ > x).map( y => {
          sumPairs(y - 1)(x)
        })
        if ( g.size == 0 )
          0
        else
          g.reduce( _ + _ )
      }).reduce( _ + _ )
      ( sGain, s )
    })
    combGains.maxBy( _._1 )
  }

  /** find the common sections and remove them
    * return the modified sums and the removed section for each sum
    */
  def removeCommon( selSums : List[Set[(Int, Int)]] ) : List[(Set[(Int, Int)], Set[(Int, Int)])] = {
    // start from everything in first sum
    var commonSum = selSums(0)
    val offsets = ArrayBuffer[Int](0)
    // loop through sums and remove everything which isn't position matching with cycle shift
    for ( i <- ( 1 until selSums.size ) ) {
      val gain = sumGain( commonSum.groupBy( _._1 ).map( x => { ( x._1, x._2.map( _._2 ) ) } ),
        selSums(i).groupBy( _._1 ).map( x => { ( x._1,  x._2.map(  _._2 ) ) } ) )
      offsets += gain._2
      commonSum = selSums(i).map( x => { ( x._1 - gain._2, x._2 ) } ).intersect( commonSum )
    }
      (selSums zip offsets).map( x => {
        ( x._1.diff( commonSum ), commonSum.map( c => { ( c._1 - x._2, c._2 ) }) )
      }).toList
  }

  /** Implement the common sets
    * Try to compress so that outputs are as close to full throughput as possible? Could result in unnecessary muxing ...
    * Find common sets again? Until can't interleave?
    * Try find interleave over positions?
    */
  /*
  def implementCommonSets( cs : List[Set[(Int, Int)]] ) : List[(Int, Int)] = {

  }
   */

  def gainMatSwapPos( gainMat : List[List[Float]], posA : Int, posB : Int ) : List[List[Float]] = {
    if ( posA == posB || posA < 0 || posB < 0 || posA - 1  >= gainMat.size || posB - 1 >= gainMat.size )
      return gainMat
    val maxPos = math.max( posA, posB )
    val minPos = math.min( posA, posB )
    val maxPosRow = gainMat( maxPos - 1 ).drop( minPos )
    val minPosCol = ( minPos + 1 until maxPos ).map( x => gainMat(x - 1)(minPos) ).toList
    gainMat.zipWithIndex.map( s => {
      if ( s._2 < minPos - 1 )
        s._1
      else if ( s._2 == minPos - 1 )
        gainMat( maxPos - 1 ).dropRight( maxPos - minPos )
      else if ( s._2 == maxPos - 1 )
        gainMat( minPos - 1 ) ++ List(maxPosRow( 0 )) ++ minPosCol
      else if ( s._2 > maxPos - 1 ) {
        s._1.take( minPos ) ++ List(s._1( maxPos )) ++
        s._1.slice( minPos + 1, maxPos ) ++ List(s._1(minPos)) ++
        s._1.takeRight( s._1.size - maxPos - 1 )
      } else
        s._1.take( minPos ) ++ List(maxPosRow( maxPosRow.size - s._2 + minPos - 1 )) ++ s._1.drop( minPos + 1 )
    })
  }

  def permuteGainMat( gainMat : List[List[Float]], indexPermute : List[Int] ) : List[List[Float]] = {
    val newMat = List.fill( gainMat.size ){ ArrayBuffer[Float]() }
    for ( io <- idxOrder.zipWithIndex ) {
      for ( rIdx <- ( io._2 + 1 until newMat.size ) ) {
        val idx = idxOrder( rIdx )
        val coords = {
          if ( idx < io._1 )
            ( io._1, idx )
          else
            ( idx, io._1 )
        }
        newMat(rIdx - 1) += gainMat(coords._1)(coords._2)
      }
    }
  }

  def getEntryWithIndexMap( coords : (Int, Int), idxMap : Map[Int, Int], gainMat : List[List[Float]] ) : Float = {
    val coordA = idxMap.get( coords._1 )
    val coordB = idxMap.get( coords._2 )
    if ( coordA < coordB ) {}
    0.0
  }

  def gainMatSort( gainMat : List[List[Float]] ) : List[List[Float]] = {
    val idxMap = collection.mutable.Map[Int, Int]() ++ {
      ( 0 until gainMat.size + 1 ).zip( 0 until gainMat.size + 1 ).toList
    }
    for ( idx <- ( 0 until 1/*gainMat.size*/ ) ) {
      // sort by column idx
      val idxOrder = (0 until idx).toList ++ tmpMat.zipWithIndex.filter( g => g._2 >= idx ).map( g => {
        ( g._1(idx), g._2 ) }).sortBy( _._1 ).map( _._2 ).toList
      tmpMat = permuteGainMat( tmpMat, idxOrder )
    }
    tmpMat
  }
}

/** This class implements a sum scheduler
  * It takes in when the numbers arrive in ( cycle, position ) coordinates
  * Assume that after the last cycle the first immediately repeats
  * Follows ASAP scheduling where as soon as it is possible to output, it must output the result
  * The schedule must also output in the order that is given in the sumStructure list
  * sumStructure dimensions are sum order, input positions, (cycle, position)
  * will create the correct size vec of outputs to keep up
  * essentially routing from io.in to sums, but know destination beforehand
  * should allow repeated adds? eg ( 1, 0 ), ( 1, 0 ) to get 2*x ... not allowed for now ...
  */
class SumScheduler( bw : Int, fw : Int, sumStructure : List[Set[(Int, Int)]], outSize : Int ) extends Module {

  val noPos = sumStructure.reduce( _ ++ _ ).map( _._2 ).max
  val noCycles = sumStructure.reduce( _ ++ _ ).map( _._1 ).max

  val io = new Bundle {
    val in = Vec( noPos, Fixed( INPUT, bw, fw ) )
    val out = Vec( outSize, Fixed( OUTPUT, bw, fw ) )
  }

  /** For each sum provided output ( cycle, position ) */
  def getOutStructure() : List[(Int, Int)] = {
    // TODO: pass structure out
    List[(Int, Int)]()
  }


  /* TODO:
   * try to simplify, find sums with repeated positions and cycle offsets ( can overlap and put in sr for eg )
   * Then find position combinations that are used commonly and factor out
   */

  /* Possible algorithm:
   * Start from sum and try to find highest gain
   * Highest gain is largest number in common of subsets of sums = |intersectAll(sumSubsets)|
   * intersection is defined as same positions with cycle offset
   * After found best subset, implement the common components
   * Can compute common and subtract if better
   * Remove common and turn into another input?
   * Intersection between all pairs of sums to get gain, find best grouping
   */

  // TODO: warning if outSize is too small

}

