package chiselutils.algorithms

import Chisel._
import scala.collection.mutable.ArrayBuffer
import com.typesafe.scalalogging._
import chiselutils.exceptions.NoResultException

/** This is an implementation of:
  * M. Kumm: Pipelined Adder Graph Optimization for High
  * Speed Multiple Constant Multiplication
  */
object RPAG extends LazyLogging {

  /** Convert a BigInt to Csd bit set format, must be positive
    */
  def toCsd( x : BigInt ) : List[(Int, Boolean)] = {
    val csdRep = ArrayBuffer[(Int, Boolean)]()
    var carry = false
    var carryToOne = false
    for ( i <- 0 until ( x.bitLength + 1 ) ) {
      if ( x.testBit(i) ) {
        if ( !carry )
          csdRep += ( ( i, true ) )
        else
          carryToOne = true
        carry = true
      } else {
        if ( carryToOne ) {
          csdRep(csdRep.size - 1) = ( (csdRep.last._1, false) )
          csdRep += ( (i, true) )
        }
        carryToOne = false
        carry = false
      }
    }
    csdRep.toList
  }

  /** Convert csd bit set format to a BigInt
    */
  def fromCsd( csd : List[(Int, Boolean)] ) : BigInt = {
    var x = BigInt(0)
    for ( c <- csd )
      x += { if ( c._2 ) { BigInt(1) << c._1 } else { BigInt(-1) << c._1 } }
    x
  }

  /** Get adder depth. Should be positive only */
  def getAdMin( x : BigInt ) : Int = {
    log2Ceil( toCsd(x).size )
  }

  /** Get adder depth. Should be positive only */
  def getAdMin( x : List[(Int, Boolean)] ) : Int = {
    log2Ceil( x.size )
  }

  def log2Ceil( x : Double ) : Int = {
    math.ceil(math.log(x)/math.log(2)).toInt
  }

  def log2Ceil( x : BigInt ) : Int = {
    log2Ceil( x.doubleValue )
  }


  /** The cost function for an adder */
  def costA( x : BigInt, u : BigInt, v : BigInt, bw : Int, cMax : Int) : Double = { 1.0 }

  /** The cost function for a register */
  def costR( x : BigInt, bw : Int) : Double = { 1.0 }

  /** Get all possible values using combinations of r1 and r2
    */
  def succ( r1 : BigInt, r2 : BigInt, cMax : Int, excludeR : Boolean, excludeOne : Boolean) : List[BigInt] = {
    val rMax = r1.max(r2)
    val rMin = r1.min(r2)
    if ( rMax > cMax ) {
      logger.warn( "cMax should be bigger than r1,r2" )
      return List[BigInt]()
    }

    val kMax = log2Ceil((cMax.doubleValue - rMax.doubleValue)/rMin.doubleValue )
    val sS = ArrayBuffer[BigInt]()
    sS += fundamental(rMax + rMin)
    sS += fundamental(rMax - rMin)
    for ( k <- ( 1 until kMax + 1 ) ) {
      val kto2 = BigInt(1) << k
      sS += fundamental(rMax * kto2 + rMin)
      sS += fundamental(rMax * kto2 - rMin)
      sS += fundamental(rMax + rMin * kto2)
      sS += fundamental(( rMax - rMin * kto2 ).abs)
    }
    if ( excludeR )
      sS -= ( BigInt(0), rMax, rMin )
    if ( excludeOne )
      sS -= ( BigInt(0), BigInt(1) )
    sS.toList
  }

  def insertRightShift( x : SInt, rawVal : BigInt, outVal : BigInt ) : SInt = {
    if ( rawVal == outVal )
      x
    else {
      assert( rawVal >> rawVal.lowestSetBit == outVal, "rawVal and outVal should just be shifts of each other" )
      x >> UInt( rawVal.lowestSetBit )
    }
  }

  def getAdd( a : SInt, b : SInt, aVal : BigInt, bVal : BigInt, cMax : Int, outVal : BigInt ) : SInt = {
    if ( aVal == outVal )
      return a
    if ( bVal == outVal )
      return b
    val sc = succ( aVal, bVal, cMax, false, false )
    val validIdxs = sc.zipWithIndex.filter( x => { x._1 == outVal } ).map( _._2 ).toList
    if ( validIdxs.size == 0 )
      NoResultException( "No add could be found as a combination of these numbers" )
    val idx = validIdxs(0)
    val isSub = ( ( idx % 2 ) == 1 )
    val kIdx = (( idx - 2)/4) + 1
    val powOfK = UInt(kIdx)
    val aIsMax = ( aVal.max(bVal) == aVal )
    val combIdx = ( ( idx - 2 ) % 4 )
    // Adder logic
    val rMaxVal = { if ( aIsMax ) aVal else bVal }
    val rMinVal = { if ( aIsMax ) bVal else aVal }
    val rMax = { if ( aIsMax ) a else b }
    val rMin = { if ( aIsMax ) b else a }
    // when adding need to do signed bit extend
    val res = {
      if ( idx == 0 ) {
        logger.debug( "doing " + rMaxVal + " + " + rMinVal )
        insertRightShift( rMax + rMin, rMaxVal + rMinVal, outVal )
      } else if ( idx == 1 ) {
        logger.debug( "doing " + rMaxVal + " - " + rMinVal )
        insertRightShift( rMax - rMin, rMaxVal - rMinVal, outVal )
      } else {
        if ( combIdx == 0 ) {
          logger.debug( "doing " + rMaxVal + " << " + kIdx + " + " + rMinVal )
          insertRightShift( ( rMax << powOfK ) + rMin, (rMaxVal << kIdx) + rMinVal, outVal )
        } else if ( combIdx == 1 ) {
          logger.debug( "doing " + rMaxVal + " << " + kIdx + " - " + rMinVal )
          insertRightShift( (rMax << powOfK) - rMin, (rMaxVal << kIdx) - rMinVal, outVal )
        } else if ( combIdx == 2 ) {
          logger.debug( "doing " + rMaxVal + " + " + rMinVal + " << " + kIdx )
          insertRightShift( rMax + (rMin << powOfK ), rMaxVal + (rMinVal << kIdx), outVal )
        } else {
          if ( rMaxVal > ( rMinVal << kIdx ) ) {
            logger.debug( "doing " + rMaxVal + " - " + rMinVal + " << " + kIdx )
            insertRightShift( rMax - ( rMin << powOfK ), rMaxVal - ( rMinVal << kIdx), outVal )
          } else {
            logger.debug( "doing " + rMinVal + " << " + kIdx + " - " + rMaxVal )
            insertRightShift( (rMin << powOfK ) - rMax, ( rMinVal << kIdx ) - rMaxVal, outVal )
          }
        }
      }
    }
    res
  }

  /** Get all combinations of set P */
  def succSet( P : Set[BigInt], cMax : Int, excludeR : Boolean, excludeOne : Boolean ) : Set[(BigInt, BigInt, BigInt)] = {
    val sSAll = P.subsets.filter( s => s.size < 3 && s.size > 0 ).map( _.toList ).map( x => {
      if ( x.size == 2 )
        succ( x(0), x(1), cMax, excludeR, excludeOne ).map( s => { ( s, x(0), x(1) ) } )
      else
        succ( x(0), x(0), cMax, excludeR, excludeOne ).map( s => { ( s, x(0), x(0) ) } )
    }).reduce( _ ++ _ )
    sSAll.filter( num => {
      ( !excludeR || !P.contains(num._1) ) && ( !excludeOne || !( BigInt(1) == num._1) ) && ( BigInt(0) != num._1 )
    }).groupBy( _._1 ).map( x => x._2(0) ).toSet[(BigInt, BigInt, BigInt)]
  }

  /** Get the best single number to add to P */
  def bestSingle( W : Set[BigInt], P : Set[BigInt],  s : Int, bw : Int, cMax : Int ) : ( BigInt, Double ) = {
    val gainMap = scala.collection.mutable.Map[ BigInt, Double ]()

    for ( w <- W ) {
      // topology a) move to higher layer (just reg it)
      logger.debug("w = " + w)
      if ( getAdMin(w) < s ) {
        logger.debug( "Topology A: moving " + w + " to higher layer" )
        gainMap( w ) = (1/costR(w, bw)) + gainMap.getOrElse( w, 0.0 )
      }

      // topology b) check if divisible by
      for ( k <- ( 1 until log2Up(w + 1) ) ) {
        val div1 = (math.pow(2.0, k) + 1).toInt
        val div2 = (math.pow(2.0, k) - 1).toInt
        val quot : BigInt = {
          if ( w % div1 == 0 )
            BigInt((w.doubleValue/div1).toInt)
          else if ( w % div2 == 0 )
            BigInt((w.doubleValue/div2).toInt)
          else
            BigInt(0)
        }
        if ( quot > 0 && getAdMin( quot ) < s ) {
          logger.debug( "Topology B: found divisible: " + quot )
          gainMap(quot) = (1/costA( w, quot, quot, bw, cMax)) + gainMap.getOrElse( quot, 0.0 )
        }
      }

      // topology c) check if we add one more then can be combined with whats already there
      val chkAry = collection.mutable.Set[BigInt]()
      for ( p <- P ) {
        val sS = succ( w, p, cMax, true, false )
        for ( sc <- sS ) {
          if ( getAdMin( sc )  < s ) {
            if ( !chkAry.contains( sc ) ) {
              logger.debug( "Topology C: found combined " + sc )
              gainMap( sc ) = (1/costA( w, sc, p, bw, cMax)) + gainMap.getOrElse( sc, 0.0 )
            }
            chkAry += sc
          }
        }
      }
    }

    // evaluate the topologies and find lowest number with highest gain
    if ( gainMap.size < 1 )
      NoResultException("No single candidates found")

    logger.debug("gainMap = " + gainMap)
    val gainMax = gainMap.maxBy( _._2 )
    logger.debug("gainMax = " + gainMax)
    val gainList = gainMap.toList.filter( _._2 == gainMax._2 ).sortBy( _._1 )
    logger.debug("gainList = " + gainList)

    val gainChoice = gainList(0)  // perhaps randomally select?
    logger.debug("gainChoice = " + gainChoice)

    assert( !P.contains( gainChoice._1 ), "Selected a number again in single?" )

    gainChoice
  }

  /** get all canonical signed digit combinations using tail recursion
    * returns each combination in bit indexed csd format */
  def getMsdComb( xCsd : List[(Int, Boolean)], startIdx : Int = 1 ) : Set[List[(Int, Boolean)]] = {
    val validComb = collection.mutable.Set[List[(Int, Boolean)]]()
    validComb += xCsd
    val sIdx = { if ( startIdx < 1 ) 1 else startIdx }
    for ( idx <- ( sIdx until xCsd.size ) ) {
      if ( ( xCsd(idx - 1)._1 == xCsd(idx)._1 - 2 ) && ( xCsd(idx)._2 != xCsd(idx - 1)._2 ) ) {
        val newCsd = {
          if ( xCsd(idx)._2 )  {
            // replace 1,0,-1  with 0,1,1
            xCsd.slice( 0, idx - 1 ) ++ List(
              ( xCsd( idx - 1 )._1, true ), ( xCsd(idx)._1 - 1, true )
            ) ++ xCsd.slice( idx + 1, xCsd.size )
          } else {
            // replace -1,0,1 with 0,-1,-1
            xCsd.slice( 0, idx - 1 ) ++ List(
              ( xCsd( idx - 1 )._1, false ), ( xCsd(idx)._1 - 1, false )
            ) ++ xCsd.slice( idx + 1, xCsd.size )
          }
        }
        validComb ++= getMsdComb( newCsd, idx - 1 )
      }
    }
    Set[List[(Int, Boolean)]]() ++ validComb
  }

  def fundamental( xCsd : List[(Int, Boolean)] ) : List[(Int, Boolean)] = {
    xCsd.map( x => ( x._1 - xCsd(0)._1, x._2 ) ).toList
  }

  def fundamental( x : BigInt ) : BigInt = {
    x >> x.lowestSetBit
  }

  def abs( xCsd : List[(Int, Boolean)] ) : List[(Int, Boolean)] = {
    if ( !xCsd.last._2 )
      xCsd.map( x => ( x._1, !x._2 ) ).toList
    else
      xCsd
  }

  /** For a given number find all unique combinations of fundamental additions
    */
  def bitCombinations( xCsd : List[(Int, Boolean)], maxAd : Int ) : Set[(List[(Int, Boolean)],List[(Int, Boolean)])] = {
    val xSets = xCsd.toSet[(Int,Boolean)].subsets
    val bothSets = xSets.map( x => { ( x, xCsd.filter( !x.contains(_) ).toSet[(Int,Boolean)]  )} )
    val pairs = bothSets.filter( x => !(x._1.isEmpty || x._2.isEmpty ) ).map( xy => {
      ( xy._1.toList.sortBy(_._1), xy._2.toList.sortBy(_._1) ) })
    val pairVld = pairs.filter( xy => getAdMin(xy._1) <= maxAd && getAdMin(xy._2) <= maxAd )
    pairVld.map( xy => {
      val abs1 = fundamental(abs(xy._1))
      val abs2 = fundamental(abs(xy._2))
      // order so not repeated
      if ( abs1.last._1 > abs2.last._1 )
        ( abs1, abs2 )
      else
        ( abs2, abs1 )
    }).toSet[(List[(Int, Boolean)],List[(Int, Boolean)])] // Filter out 1's? maybe not just turn pair to single later
  }

  /** For each number find all CSD representations for potential numbers
    * Then find all possible combinations of each representation
    */
  def findMsdPotentials( W : Set[BigInt], maxAd : Int ) : Set[(List[(Int, Boolean)],List[(Int, Boolean)])] = {
    val wFiltered = W.filter( !List(0, 1).contains(_) ).map( toCsd(_) )
    val pairSet = collection.mutable.Set[(List[(Int, Boolean)],List[(Int, Boolean)])]()
    for ( w <- wFiltered ) {
      val msdComb = getMsdComb( w )
      for ( msd <- msdComb )
        pairSet ++= bitCombinations( msd, maxAd )
    }
    Set[(List[(Int, Boolean)],List[(Int, Boolean)])]() ++ pairSet
  }

  def bestPair( W : Set[BigInt], P : Set[BigInt], s : Int, bw : Int, cMax : Int ) : ( BigInt, BigInt, Double ) = {

    var bestPair = ( BigInt(0), BigInt(0) )
    var gainMax = 0.0

    val allPairs = findMsdPotentials( W, s )

    for ( ap <- allPairs ) {
      val ap1 = fromCsd( ap._1 )
      val ap2 = fromCsd( ap._2 )
      val xi = { if ( ap1 > ap2 ) ap1 else ap2 }
      val xj = { if ( ap1 > ap2 ) ap2 else ap1 }
      if ( ( xi < 2*cMax ) && ( xj < 2*cMax ) ) {
        // find all W which can be removed if this pair is added
        val sc = succ( xi, xj, cMax*2, true, false)
        val combinationOfPair = W.toSet[BigInt].filter( x => sc.contains(x) )
        val gain = combinationOfPair.map( x => { 1/costA(x, xi, xj, bw, cMax ) }).reduce( _ + _ )/2
        if ( gain >= gainMax ) {
          if ( xi < bestPair._1 || gain > gainMax ) { // find smallest with equal gain. maybe choose random?
            logger.debug( "Updating pair to (" + xi + ", " + xj + ") with gain " + gain )
            gainMax = gain
            bestPair = ( xi, xj )
          }
        }
      }
    }
    ( bestPair._1, bestPair._2, gainMax )
  }

  /** Adder layer format: ( outVal, ( aVal, aIdx ), ( bVal, bIdx ) )
    */
  def implementAdderLayer( xIn : List[SInt], adderLyr : Set[(BigInt, (BigInt, Int), (BigInt, Int))], cMax : Int ) : List[(SInt, BigInt)] = {
    logger.debug( "adderLyr = " + adderLyr )
    logger.debug( "xIn = " + xIn )
    adderLyr.map( x => {
      val outVal = x._1
      val aVal = x._2._1
      val aIdx = x._2._2
      val bVal = x._3._1
      val bIdx = x._3._2
      logger.debug( "x = " + x )
      val xOut = getAdd( xIn(aIdx), xIn(bIdx), aVal, bVal, cMax, outVal)
      ( xOut, outVal )
    }).toList
  }

  def implementAdder( xIn : SInt, cMax : Int, adderStructure : List[Set[(BigInt, BigInt, BigInt)]], t : List[BigInt] ) : Vec[SInt] = {
    val firstNumbers = adderStructure(0).map( x => Set( x._2, x._3 ) ).reduce( _ ++ _ )

    var currLyr = firstNumbers.map( fn => {
      val csd : List[(Int, Boolean)] = toCsd(fn)
      // should have most len of 2
      // c(0)._1 == 0
      // c(1)._2 == true
      logger.debug( fn + " as csd = " + csd )
      val res = {
        if ( csd.size == 2 ) {
          val xShift = xIn << UInt( csd(1)._1 )
          if ( csd(0)._2 )
            ( xShift + xIn, fn )
          else
            ( xShift - xIn, fn )
        } else // if size 1 then can only be 1
          ( xIn + SInt(0), BigInt(1) )
      }
      res
    }).toList

    // check small number case
    if ( adderStructure.size == 1 && adderStructure(0).filter( x => ( x._1 == x._2 && x._2 == x._3 ) ).size == adderStructure(0).size ) {
      return Vec( t.map( x => currLyr.find( _._2 == x ).get._1 ) )
    }

    for ( as <- adderStructure ) {
      currLyr = implementAdderLayer(
        currLyr.map( _._1 ),
        as.map( x => { // get this layer
          ( x._1,                                          // The out value
            ( x._2, currLyr.map( _._2 ).indexOf( x._2 ) ), // ( aVal, aIdx )
            ( x._3, currLyr.map( _._2 ).indexOf( x._3 ) )  // ( bVal, bIdx )
          )
        }), 
        cMax )
    }
    Vec( t.map( x => currLyr.find( _._2 == x ).get._1 ) )
  }

  /** Run the RPAG algorithm
    * return the structure of the adder
    */
  def apply( t : List[BigInt] ) : List[Set[(BigInt, BigInt, BigInt)]] = {

    val bitsT = log2Ceil(t.reduce( (x,y) => x.max(y) )) + 1
    val cMax = math.pow(2, bitsT ).toInt
    val bw = 16 + bitsT

    val S = t.map( getAdMin(_) ).reduce( (x,y) => math.max( x, y ) )
    
    logger.info( "Using " + S + " layers" )
    val X = ArrayBuffer(collection.mutable.Set(t :_*)) // fill with initial numbers
    val adderConst = ArrayBuffer[Set[(BigInt, BigInt, BigInt)]]( )
    for ( s <- (2 until S + 1).reverse ) {
      val W = X.last // working set is all in current stage
      val P = collection.mutable.Set[BigInt]()
      val layerSet = collection.mutable.Set[(BigInt, BigInt, BigInt)]()
      while ( W.size > 0 ) {
        logger.info( "P = " + P )
        logger.info( "W = " + W )
        val bsRes = try {
          bestSingle( Set[BigInt]() ++ W, Set[BigInt]() ++ P, s, bw, cMax )
        } catch {
          // No single candidates found
          case e: NoResultException => ( BigInt(0), 0.0 )
          case e: Throwable => throw e
        }
        val numAdded = {
          if ( bsRes._1 != 0 ) {
            // add single case
            logger.debug( "Single: " + bsRes )
            bsRes._1
          } else {
            // add pair case
            val bpRes = bestPair( Set[BigInt]() ++ W, Set[BigInt]() ++ P, s, bw, cMax )
            logger.debug( "Pair: " + bpRes )
            // just add one as next single case will catch it
            if ( bpRes._2 == BigInt(1) )
              bpRes._1
            else
              bpRes._2
          }
        }
        P += numAdded
        val sS = succSet( Set[BigInt]() ++ P, cMax * 2, false, false )
        val toRemove = W.filter( x => sS.map( _._1 ).contains(x) )
        logger.info( "toRemove = " + toRemove )
        W --= toRemove
        // find r = a + b etc ...
        val depsMap = toRemove.map( r => sS.find( _._1 == r ).get ).toSet[(BigInt, BigInt, BigInt)]
        layerSet ++= depsMap
      }
      X += P
      adderConst += Set[(BigInt, BigInt, BigInt)]() ++ layerSet
    }
    if ( adderConst.size == 0 ) {
      // all small nums can be computed immediately
      adderConst += t.map( x => ( x, x, x ) ).toSet[(BigInt, BigInt, BigInt)]
    }
    adderConst.toList.reverse
  }
}
