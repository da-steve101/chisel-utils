
package chiselutils.math

import Chisel._

class ConstMultFixed( genType : Fixed, multVar : List[BigInt] ) extends Module {
  val io = new Bundle {
    val in = genType.cloneType.asInput
    val out = Vec( multVar.size, genType.cloneType ).asOutput
  }

  /* Preprocessing:
   * 1) Shift all numbers so that lsb is 1, if zero remove
   * 2) Remove all duplicates
   */
  val zeroIdxs = multVar.zipWithIndex.filter( _._1 == BigInt(0) ).map( _._2 )
  val zeroFilt = multVar.zipWithIndex.filter(_._1 != BigInt(0) )
  val shiftIdxs = zeroFilt.map( x => { ( x._1 >> x._1.lowestSetBit, x._1.lowestSetBit, x._2 ) })
  val dupIdxs = shiftIdxs.groupBy( _._1 )
  val t = dupIdxs.map( _._1 )
  val wOut = Vec( t.size, genType.cloneType )

  for ( z <- zeroIdxs )
    io.out(z) := Fixed(0, genType.bitWidth, genType.fracWidth )
  for ( i <- ( 0 until t.size ) ) {
    for ( d <- dupIdxs(t[i]) )
      io.out(d._3) := wOut(i) << d._2
  }

  /** This is an implementation of:
    * M. Kumm: Pipelined Adder Graph Optimization for High
    * Speed Multiple Constant Multiplication
    */
  def rpag( t : List[BigInt] ) {

    /** Get adder depth. Should be positive only */
    def getAdMin( x : BigInt ) : Int = {
      var Sx = 0
      var carry = false
      var carryToOne = false
      for ( i <- 0 until ( x.bitLength + 1 ) ) {
        if ( x.testBit(i) ) {
          if ( !carry )
            Sx += 1
          else
            carryToOne = true
          carry = true
        } else {
          if ( carryToOne )
            Sx += 1
          carryToOne = false
          carry = false
        }
      }
      log2Ceil( Sx )
    }

    def getGain( p : BigInt, W : List[BigInt], P : List[BigInt] ) {
      val cA = 1
      val cR = 1

    }

    /** best single predecessor implementation */
    def bestSingle( W : List[BigInt], P : List[BigInt],  s : Int) {
      val wIdx = W.zipWithIndex
      // topology a) move to higher layer
      val pA = wIdx.map( w => ( getAdMin(w._1), w._2 ) ).filter( w => w._1 < s )
      // topology b) check if divisible by
      val pB = wIdx.map( w => {
        val possibleK = List.fill(2)( (0 until w._1.bitLength).toList ).reduce(_ ++ _).zip(
          List.fill( w._1.bitLength ) { 1 } ++ List.fill( w._1.bitLength ) { -1 }).map( k =>
          math.pow(2, k._1) + k._2 ) // list of 2^k +/- 1 from k <- 0 until bitLength
        possibleK.zipWithIndex.map( kVal => ( w._1.doubleVal / kVal._1, kVal._1, kVal._2 ) ).filter(
          x => int(x._1) == x._1 && x._2 != 1 ).map( possibleK(_._3) )
      })
      // topology c) check if can add single to be combined with whats already there
      val pC = wIdx.map( w => {
        val bl = w._1.bitLength
        // does l2 need to be iterated over? don't think so ... same as r
        P.map( p => { for ( l1 <- (0 until bl); sg <- List( -1, 1 ) ) {
          yeild( (w._1 << l1) + BigInt(sg)*p, l1, sg, p )
        }}.filter( getAdMin(_._1) < s ) )
      })

      // evaluate the topologies and find highest gain

    }

    val S = t.map( getAdMin(_) ).reduce( (x,y) => math.max( x, y ) )
    println( "S = " + S )
    var Xs = t
    for ( s <- (2 until S).reverse ) {
      var W = Xs
      val P = new ArrayBuffer()
      while ( W.length > 0 ) {

      }
      Xs = P.toList
    }
  }
}
