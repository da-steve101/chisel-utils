
package chiselutils.math

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import optimus.optimization._
import optimus.optimization.MPIntVar
import optimus.algebra.Expression
import optimus.algebra.Constraint
import optimus.algebra.One
import optimus.algebra.Zero
import Chisel._

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
    val uniqueConvoys = HashSet[Set[Person]]()
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

  /** Check if the convoys have a common subsection
    * Generate a split left and right
    * If put all on one side will ignore an default to mux
    */
  def generateAddSplit( convoys : List[Set[Person]] ) : (
    List[Set[Person]], List[Set[Person]] ) = {
    val commonSection = getCommonSection( convoys )
    println( "commonSection = " + commonSection )
    // inspect the common section. Filter out anything not in the 1 cycle reach chain
    val cycles = commonSection.map( _.roadTripTime ).toList.sortBy( x => x )
    println( "cycles = " + cycles )
    if ( cycles.size == 0 )
      return ( List.fill( convoys.size ) { Set[Person]() }, convoys )
    val cycStart = cycles(0)
    var cycStop = cycStart
    for ( i <- 0 until cycles.size ) {
      if ( cycles(i) <= cycStop + 1 )
        cycStop = cycles(i)
    }
    // Fix me: This is an arbitrary rule with no strong reasoning ( other than too early to do this )
    if ( cycStart > 2 )
      return ( List.fill( convoys.size ) { Set[Person]() }, convoys )
    // Look at adds the split, if common section is entire set then reduce the split by 1 cause can put all on one side
    if ( convoys.map( s => s == commonSection ).reduce( _ || _ ) )
      cycStop -= 1
    println( "cycStop = " + cycStop )
    println( "cycStart = " + cycStart )
    val splitOff = commonSection.filter( _.roadTripTime <= cycStop ).toSet
    val lConvoy = convoys.map( c => if ( c.isEmpty ) Set[Person]() else splitOff ).toList
    val rConvoy = convoys.map( c => { c -- splitOff.toSet } ).toList
    ( lConvoy, rConvoy )
  }

  /** Generate a mux split on everything that has the next most needed thing in common
    * Forbidden to generate a mux all on one side
    */
  def generateMuxSplit( convoys : List[Set[Person]] ) : List[Boolean] = {
    val uniqueConvoys = getUniqueConvoys( convoys )
    val minRemain = getMinTrip( uniqueConvoys )
    val withMin = uniqueConvoys.map( convoy => convoy.filter( _.roadTripTime == minRemain ).toList ).reduce( _ ++ _ )
    val bestPerson = withMin.groupBy( x => x ).map( x => { ( x._1, x._2.size ) }).maxBy( _._2 )._1
    // ensure not all on one side - only happen if one set is best person
    if ( convoys.map( s => s == Set( bestPerson ) ).reduce( _ || _ ) )
      return convoys.map( s => s != Set( bestPerson ) )
    convoys.map( !_.contains(bestPerson) )
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

  def moveConvoys( convoys : List[Set[Person]] ) : List[Set[Person]] = {
    val rotated = convoys.drop(1) ++ List( convoys.head )
    subCycles( rotated, 1 )
  }

  def antiMovePossible( possible : ( Set[Person], List[Int] ), noConvoys : Int ) :
      ( Set[Person], List[Int] ) = {
    ( possible._1.map( p => Person( p.roadTripTime + 1, p.beachId, p.shift, p.neg ) ),
      possible._2.map( i => ( (i + 1) % noConvoys ) ) )
  }

  def separateConvoy( road : Road,
    lConvoy : List[Set[Person]],
    rConvoy : List[Set[Person]],
    currConvoy : Set[Person] ) : ( Set[Person], Set[Person] ) = {
    for ( l <- lConvoy ) {
      val lCalc = negConvoy( subShift( l, -road.getShift() ), road.getNeg() )
      val lMod = lCalc.map( p => Person( p.roadTripTime + 1, p.beachId, p.shift, p.neg ) )
      for ( r <- rConvoy ) {
        val rMod = r.map( p => Person( p.roadTripTime + 1, p.beachId, p.shift, p.neg ) )
        if ( road.isDelay() ) {
          if ( lMod == currConvoy )
            return ( l, Set[Person]() )
        } else if ( road.isAdd() ) {
          if ( lMod ++ rMod == currConvoy )
            return ( l, rMod )
        } else {
          if ( lMod == currConvoy )
            return ( lMod, Set[Person]() )
          if ( rMod == currConvoy )
            return ( Set[Person](), rMod )
        }
      }
    }
    return ( Set[Person](), Set[Person]() )
  }

  def delayUp( currRd : Road ) : Road = {
    currRd.setDelay()
    val convoys = currRd.passLeftUp( currRd.convoyIn() )
    val lRoad = new Road( convoys.size )
    lRoad.addConvoysIn( convoys )
    lRoad
  }

  def muxUp( currRd : Road, muxSwitch : List[Boolean] ) : ( Road, Road ) = {
    currRd.setMux()
    val muxMod = muxSwitch.takeRight(1) ++ muxSwitch.dropRight(1)
    for ( i <- 0 until muxSwitch.size )
      currRd.setMuxSwitch( i, muxMod(i) )
    val lConvoy = currRd.convoyIn().zip( muxSwitch ).map( x => if ( x._2 ) Set[Person]() else x._1 )
    val rConvoy = currRd.convoyIn().zip( muxSwitch ).map( x => if ( x._2 ) x._1 else Set[Person]() )
    println( "mux -> {" + lConvoy + "}, {" + rConvoy + "}" )
    val lRoad = new Road( lConvoy.size )
    val rRoad = new Road( rConvoy.size )
    lRoad.addConvoysIn( currRd.passLeftUp( lConvoy ) )
    rRoad.addConvoysIn( currRd.passRightUp( rConvoy ) )
    ( lRoad, rRoad )
  }

  def addUp( currRd : Road, lConvoy : List[Set[Person]], rConvoy : List[Set[Person]] ) : ( Road, Road ) = {
    currRd.setAdd()

    // verify that add is good
    val convoys = currRd.convoyIn()
    for ( i <- 0 until convoys.size )
      assert( convoys(i) == lConvoy(i) ++ rConvoy(i), "Add must be correctly split" )

    val lRoad = new Road( lConvoy.size )
    val rRoad = new Road( rConvoy.size )
    lRoad.addConvoysIn( currRd.passLeftUp( lConvoy ) )
    rRoad.addConvoysIn( currRd.passRightUp( rConvoy ) )
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

    println( "city = " + city )

    // method 1: do the adds then mux all of them
    val muxRequired = getUniqueConvoys( city ).map( convoy => getMaxTrip( convoy ) - convoyMinRoad( convoy ) )
    println( "muxRequired = " + muxRequired )
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

    // TODO shuffle cities order
    for ( city <- cities ) {
      // create road out of city
      val newRoad = new Road( city.size )
      newRoad.addConvoysIn( city )
      var cityIdle = 0
      var stop = false

      // loop until road completed
      while( !newRoad.completed() ) {
        val nextRd = newRoad.nextIncomplete().get

        // choose what nextRd should be
        val minRd = cityMinRoad( nextRd.convoyIn() )
        if ( nextRd != newRoad )
          assert( minRd >= 0, "Bug: Cannot generate hardware, Internal miscalculation of cycles, minRd = " + minRd )
        else
          assert( minRd >= 0, "Cannot generate hardware, possibly not enough cycles available for user input, minRd = " + minRd )

        // println( "convoyIn = " + nextRd.convoyIn() )
        println( "minRd = " + minRd )
        println( "current roads = " + newRoad )
        if ( minRd > 0 ) {
          // TODO: deal with shifts and neg but for now ignore
          nextRd.setShift( 0 )
          nextRd.setNeg( false )

          val lRoad = delayUp( nextRd )
          nextRd.setLeft( lRoad )
          if ( !stop )
            cityIdle += 1
        } else {
          stop = true
          // try to do an add split first
          val addSplit = generateAddSplit( nextRd.convoyIn() )
          println( "addSplit = " + addSplit )
          val addInvalid = ( 0 until nextRd.convoyIn().size ).map( idx => {
            !nextRd.convoyIn()( idx ).isEmpty && ( addSplit._1(idx).isEmpty || addSplit._2(idx).isEmpty )
          }).reduce( _ || _ )
          // if any adds are empty, do mux instead
          if ( addInvalid ) {

            val muxSwitch = generateMuxSplit( nextRd.convoyIn() )

            println( "mux split = " + muxSwitch )

            // TODO: deal with shifts and neg but for now ignore
            nextRd.setShift( 0 )
            nextRd.setNeg( false )

            // create the l and r roads
            val rds = muxUp( nextRd, muxSwitch )
            nextRd.setLeft( rds._1 )
            nextRd.setRight( rds._2 )
          } else {

            // TODO: deal with shifts and neg but for now ignore
            nextRd.setShift( 0 )
            nextRd.setNeg( false )

            println( "add split = " + addSplit )

            // implement the add
            val rds = addUp( nextRd, addSplit._1, addSplit._2 )
            nextRd.setLeft( rds._1 )
            nextRd.setRight( rds._2 )
          }
        }

      }
      if ( cityIdle < idleCyc )
        idleCyc = cityIdle
      roadOut += newRoad
    }
    println( "idleCyc = " + idleCyc )
    return roadOut.toList
  }

  def implementStructure( cities : List[Road], inputs : List[Fixed], validIn : Bool ) : List[Fixed] = {
    cities.map( _.implementRoad( inputs, validIn ) )
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



}

