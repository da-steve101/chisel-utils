
package chiselutils.math

import scala.collection.mutable.ArrayBuffer
import collection.immutable.HashSet
import optimus.optimization._
import optimus.optimization.MPIntVar
import optimus.algebra.Expression
import optimus.algebra.Constraint
import optimus.algebra.One
import optimus.algebra.Zero
import Chisel._
import util.Random

/** Algorithm:
  * Until all citys are complete, select a random city
  *   Until all paths for city are complete, grow the graph by:
  *     1)  Choose the left most road
  *     2)  Use signposts map to find what convoys are on the road
  *     3)  Calculate the number till split, if > 0 then add a straight section and start at 1)
  *     4)  Otherwise, propose a split, either a mux or add
  *     5)  See if either side of the split can be merged into existing roads, if yes merge it
  *     6)  If can't merge then implement the split growing the graph a little
  *
  * To see if a split can be merged:
  * Signposts show who is at each road section
  * Use to see if there are any matches
  * A match is where non-empty in the newly created split match with either empty or identical convoys in the split
  * If it matches with an empty then need to look into the possible data structure to fill in the valid signposts
  */

/** Format of a Road
  * ( cyc1, pos1, cyc2, pos2, type, neg ) = ( Int, Int, Int, Int, Int, Boolean )
  * ( cyc1, pos1 ) is where left side of road connects to
  * ( cyc2, pos2 ) is where right side of road connects to
  * type = 0 is a signpost splitting convoys (mux)
  * type < 0 is type 0 with a shift of << ( -type ) on the left road
  * type = 1 is a signpost splitting people in convoys (add)
  * type > 1 is type 1 with a shift of << (type - 1) on the left road
  * neg is boolean, if to make input 1 of operation negative
  */

/** Format of people
  * ( trip time, beach, shift, neg ) = ( Int, Int, Int, Boolean )
  * ( trip time, beach ) is how long a person wants to take to a particular beach
  * shift is how much left shift is needed
  * neg is if is negative
  * empty set is don't care
  */

/** Convoy = Set[People]
  * City = List[Convoy]
  */

case class NoSolution( msg : String ) extends Exception

object SumScheduler {

  /** Check if one convoy is a time shifted subset of another masterSet
    * masterSet is the main convoy
    * sub is another convoy
    * return ( idx of shift, if sub is a subset of masterSet )
    */
  def isSubsetCycShift( masterSet : Set[Person],
    sub : Set[Person] ) : (Int, Boolean) = {
    val subMin = sub.minBy( _.roadTripTime ).roadTripTime
    // loop through masterSet cycles
    for ( msCyc <- masterSet.map( _.roadTripTime - subMin ) ) {
      if ( sub.map( x => { Person( x.roadTripTime + msCyc, x.beachId, x.shift, x.neg ) }).subsetOf(masterSet) )
        return ( msCyc, true )
    }
    return ( -1, false )
  }

  /** Get unique convoys
    * Note: Cannot cycle shift as output is locked and changing it would result in incorrect results
    */
  def getUniqueConvoys( city : List[Set[Person]] ) : List[Set[Person]] = {
    val uniqueConvoys = collection.mutable.HashSet[Set[Person]]()
    for ( convoy <- city )
      uniqueConvoys += convoy
    uniqueConvoys.toList.filterNot( _.isEmpty )
  }

  def getCommonSection( city : List[Set[Person]] ) : Set[Person] = {
    city.filterNot( _.isEmpty ).reduce( (x,y) => x.intersect(y) )
  }

  def splitCommonSection( city : List[Set[Person]] ) : (List[Set[Person]], List[Set[Person]]) = {
    val common = getCommonSection( city )
    if ( common.isEmpty || city.map( s => s == common ).reduce( _ || _ ) )
      return ( List.fill( city.size ) { Set[Person]() }, city )
    ( city.map( c => if ( c.isEmpty ) c else common ), city.map( c => c -- common ) )
  }

  /* Need to rethink how to do add and mux splits, need to look further ahead.
   * Consider 3 ( or more ) next to each other
   */

  def corrDistance( a : Person, b : Person,
    convoys : List[HashSet[Person]] ) : Double = {
    val aList = convoys.map( c => c.contains( a ) )
    val bList = convoys.map( c => c.contains( b ) )
    // compare the two, jaccard coeff
    val noTrue = ( aList zip bList ).count( x => x._1 && x._2 )
    // val noDiff = ( aList zip bList ).count( x => x._1 != x._2 )
    val total = convoys.size
    // math.max( noTrue, noDiff ).toDouble / total
    noTrue.toDouble / total
  }

  /** Call recursively until a solution that meets min path is found
    */
  def clusterDFS( leftSide : HashSet[Person], rightSide : HashSet[Person],
    loopList : List[Person], convoyHash : List[HashSet[Person]] ) : (
    HashSet[Person], HashSet[Person], Boolean ) = {

    val convoySplits = convoyHash.map( c => {
      ( leftSide.filter( c.contains( _ ) ).toSet,
        rightSide.filter( c.contains( _ ) ).toSet )
    })
    val lMin = cityMinRoad( convoySplits.map( _._1 ) )
    val rMin = cityMinRoad( convoySplits.map( _._2 ) )
    if ( math.min( lMin, rMin ) < 1 )
      return ( HashSet[Person](), HashSet[Person](), false )

    if ( loopList.size == 0 ) {
      val splitInvalid = (convoyHash zip convoySplits).map( x =>
        !x._1.isEmpty && ( x._2._1.isEmpty || x._2._2.isEmpty )
      ).reduce( _ || _ )
      if ( splitInvalid )
        return ( HashSet[Person](), HashSet[Person](), false )
      return ( leftSide, rightSide, true )
    }

    val p = loopList.head
    val selConvoys = convoyHash.filter( c => c.contains( p ) )
    val lDist = leftSide.map( l => corrDistance( l, p, selConvoys ) )
    val rDist = rightSide.map( r => corrDistance( r, p, selConvoys ) )
    val leftFirst = ( lDist.sum / lDist.size > rDist.sum / rDist.size )
    val res = clusterDFS( leftSide ++ { if ( leftFirst ) HashSet(p) else HashSet[Person]() },
      rightSide ++ { if ( !leftFirst ) HashSet(p) else HashSet[Person]() },
      loopList.drop(1), convoyHash )
    if ( res._3 )
      return res
    clusterDFS( leftSide ++ { if ( !leftFirst ) HashSet(p) else HashSet[Person]() },
      rightSide ++ { if ( leftFirst ) HashSet(p) else HashSet[Person]() },
      loopList.drop(1), convoyHash )
  }

  /** Also cluster on the add??
    * Partitioning for greatest amount of uniformity on both sides
    * Also need to consider timings :/
    * Take entire set of all possible ppl
    * Partition ppl on each side
    * Cannot put all of a sum on one side
    * Therefore group ppl by how soon needed?
    * Need to balance with how many can be on one side?
    * Could have alot of non overlapping
    * Find the split of ppl that has the minimum number of different?
    * If more than original number perhaps use mux?
    * represent as matrix of possible ppl and convoys,
    * rearrange cols so that can split at col and have both with fewest unique rows in each
    */
  def clusterAddSplit( convoys : List[Set[Person]] ) : (HashSet[Person], HashSet[Person]) = {
    val myRand = new Random
    val pplSet = convoys.reduce( _ ++ _ ).toList.sortBy( _.roadTripTime )
    val convoyHash = convoys.map( HashSet[Person]() ++ _ )
    val left = pplSet.head
    val right = pplSet.drop( 1 ).map( p => ( p, corrDistance( left, p, convoyHash ) ) ).minBy( _._2 )._1
    val leftSide = HashSet( left )
    val rightSide = HashSet( right )
    val loopSet = pplSet.filter( p => p != left && p != right )
    // do dfs to find something that has a feasable path
    val res = clusterDFS( leftSide, rightSide, loopSet, convoyHash )
    if ( !res._3 ) {
      println( "Add Split not possible for " + convoys )
      throw new NoSolution( "Cannot generate add split" )
    }
    println( "clusterSplit L = " + res._1 + ", clusterSplit R = " + res._2 )
    ( res._1, res._2 )
  }

  /** Check if the convoys have a common subsection
    * Generate a split left and right
    * If put all on one side will ignore an default to mux
    */
  def generateAddSplit( cityToSplit : City ) : (
    HashSet[Person], HashSet[Person] ) = {
    val uniqueConvoys = cityToSplit.getUnique()

    // TODO: try other splitting methods

    clusterAddSplit( uniqueConvoys )
  }

  def clusterDistance( b : HashSet[Person], cluster : List[HashSet[Person]] ) : Float = {
    cluster.map( c => c.diff(b).size + b.diff(c).size ).reduce( _ + _ )/cluster.size.toFloat
  }

  /** Look at trying to assign a set to a cluster for mux
    */
  def assignLeft( b : HashSet[Person], clusterL : List[HashSet[Person]],
    clusterR : List[HashSet[Person]] ) : Boolean = {
    val dL = clusterDistance( b, clusterL )
    val dR = clusterDistance( b, clusterR )
    dL < dR
  }

  def clusterMuxDFS( clusterL : List[HashSet[Person]],
    clusterR : List[HashSet[Person]],
    unassigned : List[HashSet[Person]] ) : (List[Boolean], Boolean) = {

    val lMin = cityMinRoad( clusterL )
    val rMin = cityMinRoad( clusterR )
    if ( math.min( lMin, rMin ) < 1 )
      return ( List[Boolean](), false )
    if ( unassigned.size == 0 )
      return ( List[Boolean](), true )

    val newConvoy = unassigned.head
    val leftFirst = assignLeft( newConvoy, clusterL, clusterR )
    val res = clusterMuxDFS( clusterL ++ {
      if ( leftFirst ) List( newConvoy ) else List[HashSet[Person]]()
    }, clusterR ++ {
      if ( !leftFirst ) List( newConvoy ) else List[HashSet[Person]]()
    }, unassigned.drop(1) )
    if ( res._2 )
      return ( List( !leftFirst ) ++ res._1, res._2 )
    val resR = clusterMuxDFS( clusterL ++ {
      if ( !leftFirst ) List( newConvoy ) else List[HashSet[Person]]()
    }, clusterR ++ {
      if ( leftFirst ) List( newConvoy ) else List[HashSet[Person]]()
    }, unassigned.drop(1) )
    ( List( leftFirst ) ++ resR._1, resR._2 )
  }

  /**
    */
  def clusterSplit( convoys : List[Set[Person]] ) : List[Boolean] = {
    val myRand = new Random
    val convoysHash = convoys.map( HashSet[Person]() ++ _ )
    for ( lIdx <- Random.shuffle( ( 0 until convoysHash.size ).toList ) ) {
      val clusterL = List( convoysHash( lIdx ) )
      val rIdx = convoysHash.zipWithIndex.map( c => ( c._2, clusterDistance( c._1, clusterL.toList )) ).maxBy( _._2 )._1
      val clusterR = List( convoysHash( rIdx ) )

      val idxs = Random.shuffle( ( 0 until convoys.size ).filter( x => x != lIdx && x != rIdx ).toList )
      val unassigned = idxs.map( convoysHash( _ ) )

      val res = clusterMuxDFS( clusterL, clusterR, unassigned )
      if ( res._2 ) {
        val reordered = (res._1 zip idxs).toList ++ List( ( false, lIdx ) ) ++ List( ( true, rIdx ) )
        return reordered.sortBy( _._2 ).map( _._1 )
      }
    }
    throw new NoSolution( "Mux couldn't find a solution" )
    List[Boolean]()
  }

  /** Generate a mux split on everything that has the next most needed thing in common
    * Forbidden to generate a mux all on one side
    */
  def generateMuxSplit( cityToSplit : City ) : List[Boolean] = {
    val uniqueConvoys = cityToSplit.getUnique()
    val muxSplit = clusterSplit( uniqueConvoys )
    cityToSplit.getIdxs().map( idx => {
      if ( idx == -1 ) // empty
        false
      else
        muxSplit( idx )
    })
  }

  /** Convert a city to cycle position (cp) coordinates
    */
  def cityToCp( city : List[Set[Person]] ) : List[Set[(Int, Int, Int, Boolean)]] = {
    val latency = city.zipWithIndex.map( convoy => convoy._1.maxBy( _.roadTripTime ).roadTripTime - convoy._2 ).max
    city.zipWithIndex.map( convoy => convoy._1.map( p => {
      ( latency + convoy._2 - p.roadTripTime, p.beachId, p.shift, p.neg )
    }))
  }

  /** Convert a list of sums in cp for an output to a city
    */
  def cpToCity( cp : List[Set[(Int, Int, Int, Boolean)]], outCyc : List[Int] ) : List[Set[Person]] = {
    (cp zip outCyc).map( c => c._1.map( x => {
      Person( c._2 - x._1, x._2, x._3, x._4 )
    }))
  }

  /** Calculate the number of outputs after binary reduce noIn inputs over noCycles
    */
  def getBinReduce( noIn : Int, noCycles : Int ) : Int = {
    var currIn = noIn
    for ( i <- 0 until noCycles ) {
      val oddBit = currIn & 1
      currIn = ( currIn >> 1 ) + oddBit
    }
    currIn
  }

  /** Calculate the amount of resources in bin reduce after noCycles
    * return ( resources, noOut )
    */
  def getBinReduceRes( noIn : Int, noCycles : Int ) : ( Int, Int ) = {
    var currResource = 0
    var currIn = noIn
    for ( i <- 0 until noCycles ) {
      val oddBit = currIn & 1
      currIn = ( currIn >> 1 ) + oddBit
      currResource += currIn
    }
    ( currResource, currIn )
  }

  /** Calculate the resources used for an input pattern
    * returns ( noResource, noCycle )
    */
  def cumulativeResource( noIns : List[(Int, Int)] ) : ( Int, Int ) = {
    val addZones = ( noIns.dropRight(1) zip noIns.drop(1) )
    var currResource = 0
    var currOut = 0
    var currCyc = noIns.head._2
    for ( aIdx <- 0 until addZones.size ) {
      val az = addZones( aIdx )
      val noIn = currOut + az._1._1
      val noCyc = az._2._2 - az._1._2
      val ar = getBinReduceRes( noIn, noCyc )
      currCyc += noCyc
      currResource += ar._1
      currOut = ar._2
    }

    // last iteration
    val az = noIns.last
    val noIn = currOut + az._1
    val noCyc = log2Ceil( noIn )
    val ar = getBinReduceRes( noIn, noCyc )

    ( currResource + ar._1, currCyc + noCyc )
  }

  def getMaxTrip( convoy : Set[Person] ) : Int = {
    convoy.maxBy( _.roadTripTime ).roadTripTime
  }

  def getMinTrip( convoy : Set[Person] ) : Int = {
    convoy.minBy( _.roadTripTime ).roadTripTime
  }

  def getMaxTrip( city : List[Set[Person]] ) : Int = {
    city.filterNot( _.isEmpty ).map( getMaxTrip( _ ) ).max
  }

  def getMinTrip( city : List[Set[Person]] ) : Int = {
    city.filterNot( _.isEmpty ).map( getMinTrip( _ ) ).min
  }

  def subCycles( city : List[Set[Person]], cycToSub : Int ) :
      List[Set[Person]] = {
    city.map( convoy => convoy.map( p => { Person( p.roadTripTime - cycToSub, p.beachId, p.shift, p.neg ) } ))
  }

  def negConvoy( convoy : Set[Person], neg : Boolean ) : Set[Person] = {
    if ( !neg )
      return convoy
    convoy.map( p => { Person( p.roadTripTime, p.beachId, p.shift, !p.neg ) } )
  }

  def subShift( convoy : Set[Person], shift : Int ) : Set[Person] = {
    convoy.map( p => { Person( p.roadTripTime, p.beachId, p.shift - shift, p.neg ) } )
  }

  def delayUp( currRd : Road ) : Road = {
    currRd.setDelay()
    val lRoad = new Road( currRd.noConvoy, currRd.latency )
    lRoad.addConvoysIn( currRd.passLeftUp( currRd.getCity() ) )
    lRoad
  }

  def muxUp( currRd : Road, muxSwitch : List[Boolean] ) : ( Road, Road ) = {
    currRd.setMux()
    val muxMod = muxSwitch.drop(1) ++ muxSwitch.take(1)
    for ( i <- 0 until muxSwitch.size )
      currRd.setMuxSwitch( i, muxMod(i) )
    val lRoad = new Road( currRd.noConvoy, currRd.latency )
    val rRoad = new Road( currRd.noConvoy, currRd.latency )
    println( "muxing up " + currRd.getCity() )
    println( "using muxSwitch = " + muxSwitch )
    println( "left = " + currRd.passLeftUp( currRd.getCity() ) )
    println( "right = " + currRd.passRightUp( currRd.getCity() ) )
    lRoad.addConvoysIn( currRd.passLeftUp( currRd.getCity() ) )
    rRoad.addConvoysIn( currRd.passRightUp( currRd.getCity() ) )
    ( lRoad, rRoad )
  }

  def addUp( currRd : Road, lPpl : HashSet[Person], rPpl : HashSet[Person] ) : ( Road, Road ) = {
    currRd.setAdd()

    // set currRd add split
    for ( l <- lPpl )
      currRd.setAddLeft( l )
    for ( r <- rPpl )
      currRd.setAddRight( r )

    val lRoad = new Road( currRd.noConvoy, currRd.latency )
    val rRoad = new Road( currRd.noConvoy, currRd.latency )
    lRoad.addConvoysIn( currRd.passLeftUp( currRd.getCity() ) )
    rRoad.addConvoysIn( currRd.passRightUp( currRd.getCity() ) )
    ( lRoad, rRoad )
  }

  /** Get the min number of road stages needed to disperse a convoy
    */
  def convoyMinRoad( convoy : Set[Person] ) : Int = {
    val convoyExits = convoy.groupBy( _.roadTripTime ) // partition convoy into when ppl are supposed to arrive
    val convoyKeys = convoyExits.keys.toList.sortBy( x => x ).reverse // get a list of destination times

    // extra cycles needed for adder tree if multiple on same cycle
    val convoyCycs = convoyExits.map( x => { ( x._1, x._2.size ) } )

    // calculate cumulative amount of road stages needed
    var currCyc = 0
    var noStages = 0
    for( kIdx <- 0 until convoyKeys.size ) {
      val key = convoyKeys(kIdx)
      val noIn = currCyc + convoyCycs( key )
      val noCyc = {
        if ( kIdx != convoyKeys.size - 1)
          key - convoyKeys( kIdx + 1 )
        else
          log2Ceil( noIn )
      }
      currCyc = getBinReduce( noIn, noCyc )
      noStages += noCyc
    }
    noStages
  }

  /** Calculate the max unshared roads this convoy needs
    * This can be used to provide a bound beyond which do not generate paths
    */
  def upperBound( convoy : Set[Person] ) : Int = {
    val convoyMax = convoy.map( _.roadTripTime ).max
    val convoyMap = convoy.groupBy( _.roadTripTime )
    val convoyCyc = convoyMap.map( x => ( x._2.size, convoyMax - x._1 ) )

    val cr = cumulativeResource( convoyCyc.toList.sortBy( _._2 ) )
    val idleCycles = {
      if ( convoyMax > cr._2 )
        convoyMax - cr._2
      else
        0
    }
    cr._1 + idleCycles
  }

  def muxRemaining( muxRequire : List[Int] ) : Int = {
    val muxList = ArrayBuffer[Int]() ++ muxRequire.sorted
    while ( muxList.size > 1 ) {
      val max = muxList.remove( muxList.size - 1 )
      val max2 = muxList.remove( muxList.size - 1 )
      val newVal = math.min( max, max2 ) - 1
      // insert into sorted list with binary insert
      var lowerBound = 0
      var upperBound = muxList.size - 1
      var idx = -1
      while ( idx == -1 ) {
        if ( upperBound <= lowerBound + 1 )
          idx = lowerBound
        else {
          val newIdx = ( upperBound + lowerBound ) >> 1
          if ( newVal == muxList( newIdx ) )
            idx = newIdx
          else if ( newVal < muxList( newIdx ) )
            upperBound = newIdx
          else
            lowerBound = newIdx
        }
      }
      muxList.insert( idx, newVal )
    }
    // return the remaining amount of delays before having to mux all
    muxList.head
  }

  /** Get the max number of road stages before having to split
    */
  def cityMinRoad( city : List[Set[Person]] ) : Int = {

    // method 1: do the adds then mux all of them
    val muxRequired = getUniqueConvoys( city ).map( convoy => getMaxTrip( convoy ) - convoyMinRoad( convoy ) )
    // println( "muxRequired = " + muxRequired )
    val method1Res = muxRemaining( muxRequired )

    if ( muxRequired.size == 1 )
      return method1Res

    // consider case where some number of ppl in all convoys exits early and goes to the same place
    // method 2: try find subsets to remove
    val commonSplit = splitCommonSection( city )
    if ( commonSplit._1.map( _.isEmpty ).reduce( _ && _ ) )
      return method1Res

    val convoysL = cityMinRoad( subCycles( commonSplit._1, 1 ) )
    val convoysR = cityMinRoad( subCycles( commonSplit._2, 1 ) )
    val method2Res = math.min( convoysL, convoysR ) // get the longest path
    return math.max( method1Res,  method2Res )
  }

  /** Build roads for all cities
    * returns the roads leading out of each city
    */
  def buildRoads( cities : List[List[Set[Person]]] ) : List[Road] = {

    // store roads out of cities
    val roadOut = ArrayBuffer[Road]()
    var idleCyc = Int.MaxValue
    var noRoads = 0

    // todo: calc latency better
    val latency = cities.map( convoys => convoys.filterNot( _.isEmpty ).zipWithIndex.map( c => {
      c._1.maxBy( _.roadTripTime ).roadTripTime - c._2
    }).max ).max

    // TODO shuffle cities order
    for ( city <- cities ) {
      // create road out of city
      val newRoad = new Road( city.size, latency )
      noRoads += 1
      newRoad.addConvoysIn( city )
      var cityIdle = 0
      var stop = false

      // loop until road completed
      while( !newRoad.completed() ) {
        val nextRd = newRoad.nextIncomplete().get

        println( "rds = " + newRoad )

        // choose what nextRd should be
        val minRd = cityMinRoad( nextRd.getCity().getUnique() )
        println( "nextRd.getCity() = " + nextRd.getCity() )
        if ( nextRd != newRoad )
          assert( minRd >= 0, "Bug: Cannot generate hardware, Internal miscalculation of cycles, minRd = " + minRd )
        else
          assert( minRd >= 0, "Cannot generate hardware, possibly not enough cycles available for user input, minRd = " + minRd )

        println( "minRd = " + minRd )
        if ( minRd > 0 ) {

          var prevRd = nextRd
          for ( i <- 0 until minRd ) {
            // TODO: deal with shifts and neg but for now ignore
            prevRd.setShift( 0 )
            prevRd.setNeg( false )

            val lRoad = delayUp( prevRd )
            prevRd.setLeft( lRoad )
            prevRd = lRoad
          }
          if ( !stop )
            cityIdle += 1
          noRoads += minRd
        } else {
          stop = true
          // try to do an add split first
          try {
            val addSplit = generateAddSplit( nextRd.getCity() )
            // if any adds are empty, do mux instead

            // TODO: deal with shifts and neg but for now ignore
            nextRd.setShift( 0 )
            nextRd.setNeg( false )

            println( "add split = " + addSplit )

            // implement the add
            val rds = addUp( nextRd, addSplit._1, addSplit._2 )
            nextRd.setLeft( rds._1 )
            nextRd.setRight( rds._2 )
            noRoads += 2
          } catch {
            case x : NoSolution => {
              val muxSwitch = generateMuxSplit( nextRd.getCity() )
              println( "mux split = " + muxSwitch )

              // TODO: deal with shifts and neg but for now ignore
              nextRd.setShift( 0 )
              nextRd.setNeg( false )

              // create the l and r roads
              val rds = muxUp( nextRd, muxSwitch )
              nextRd.setLeft( rds._1 )
              nextRd.setRight( rds._2 )
              noRoads += 2
            }
          }
        }
      }
      if ( cityIdle < idleCyc )
        idleCyc = cityIdle
      roadOut += newRoad
    }
    println( "idleCyc = " + idleCyc )
    println( "noRoads = " + noRoads )
    return roadOut.toList
  }

  def implementStructure( cities : List[Road], inputs : List[Fixed], validIn : Bool ) : List[Fixed] = {
    cities.map( _.implementRoad( inputs, validIn ) )
  }

  def writeRdToDot( cityRoads : List[Road], filename : String = "rd.dot" ) = {
    val writer = new java.io.PrintWriter(new java.io.File( filename ))
    writer.write( "digraph G {\n" )
    for ( rd <- cityRoads )
      writer.write( rd.toDot() )
    writer.write( "}\n" )
    writer.close()
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
class SumScheduler( bw : Int, fw : Int, sumStructure : List[List[Set[(Int, Int, Int, Boolean)]]], outCyc : List[List[Int]] ) extends Module {

  val noPos = sumStructure.map( x => x.map( _.maxBy( _._2 )._2 ).max ).max + 1

  val io = new Bundle {
    val in = Vec( noPos, Fixed( INPUT, bw, fw ) )
    val out = Vec( outCyc.size, Fixed( OUTPUT, bw, fw ) )
  }

  val cities = ( sumStructure zip outCyc ).map( x => SumScheduler.cpToCity( x._1, x._2 ) )

  println( "SumScheduler: cities = " + cities )

  val cityRoads = SumScheduler.buildRoads( cities )
  SumScheduler.writeRdToDot( cityRoads )

  val out = SumScheduler.implementStructure( cityRoads, io.in.toList, Bool(true) )
  io.out := Vec( out )
}

