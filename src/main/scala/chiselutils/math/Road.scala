package chiselutils.math

import collection.mutable.ArrayBuffer
import Chisel._

/** A Road in the city beaches problem
  */
class Road( val noConvoy : Int, val latency : Int ) {

  // the convoy that goes into the road
  // the convoy can change following merges
  private val cityForRd = new City( noConvoy )
  private val muxSwitch = ArrayBuffer.fill( noConvoy ) { false }
  private val addSwitch = ( collection.mutable.HashSet[Person](), collection.mutable.HashSet[Person]() )

  // a method to perform merges
  def addConvoysIn( convoys : List[Set[Person]] ) = {
    val unqConvoys = convoys.toSet.toList
    val cIdxs = convoys.map( unqConvoys.indexOf( _ ) )

    val res = cityForRd.addConvoysIn( unqConvoys, cIdxs )
    complete |= res._1
    beachId = res._2
  }

  def addConvoysIn( newCity : City ) = {
    val res = cityForRd.addConvoysIn( newCity )
    complete |= res._1
    beachId = res._2
  }

  private var beachId = -1
  private var lRoad = None : Option[Road]
  private var rRoad = None : Option[Road]
  private var parentRds = ArrayBuffer[Road]()
  private var shift = 0
  private var neg = false
  private var complete = false
  private var roadType = -1

  def setLeft( l : Road ) {
    lRoad = Some(l)
    l.setParent( this )
  }
  def setRight( r : Road ) {
    rRoad = Some(r)
    r.setParent( this )
  }
  protected def setParent( p : Road ) { parentRds += p }
  def getLeft() = lRoad
  def getRight() = rRoad
  def getParents() = parentRds.toList

  def getShift() = shift
  def setShift( s : Int ) = { shift = s }
  def getNeg() = neg
  def setNeg( n : Boolean ) = { neg = n }

  def getMuxSwitch( i : Int ) = { muxSwitch(i) }
  def setMuxSwitch( i : Int, lr : Boolean ) = { muxSwitch(i) = lr }

  def getAddLeft() = addSwitch._1.toList
  def getAddRight() = addSwitch._2.toList
  def setAddLeft( p : Person ) = { addSwitch._1 += p }
  def setAddRight( p : Person ) = { addSwitch._2 += p }

  def getCity() = { cityForRd }

  def isMux() = { roadType == 0 }
  def setMux() = { roadType = 0 }
  def isAdd() = { roadType == 1 }
  def setAdd() = { roadType = 1 }
  def isDelay() = { roadType == 2 }
  def setDelay() = { roadType = 2 }

  def completed() : Boolean = {
    if ( complete )
      return true
    if ( isDelay() && lRoad.isDefined )
      complete = lRoad.get.completed()
    else if ( lRoad.isDefined && rRoad.isDefined )
      complete = lRoad.get.completed() && rRoad.get.completed()
    return complete
  }

  private def idxAbove( idx : Int ) = { ( idx - 1 + noConvoy ) % noConvoy }
  private def idxBelow( idx : Int ) = { ( idx + 1 ) % noConvoy }

  def passLeftDown( p : Person ) : Person = {
    p.moveTowardsCity( getShift(), getNeg() )
  }
  def passRightDown( p : Person ) : Person = {
    p.moveTowardsCity( 0, false )
  }
  def passLeftUp( p : Person ) : Person = {
    p.moveTowardsBeach( getShift(), getNeg() )
  }
  def passRightUp( p : Person ) : Person = {
    p.moveTowardsBeach( 0, false )
  }

  def passLeftDown( convoy : Set[Person] ) : Set[Person] = {
    convoy.map( passLeftDown( _ ) )
  }
  def passRightDown( convoy : Set[Person] ) : Set[Person] = {
    convoy.map( passRightDown( _ ) )
  }
  def passLeftUp( convoy : Set[Person] ) : Set[Person] = {
    if ( isAdd() )
      return convoy.filter( addSwitch._1.contains( _ ) ).map( passLeftUp( _ ) )
    convoy.map( passLeftUp( _ ) )
  }
  def passRightUp( convoy : Set[Person] ) : Set[Person] = {
    if ( isAdd() )
      return convoy.filter( addSwitch._2.contains( _ ) ).map( passRightUp( _ ) )
    convoy.map( passRightUp( _ ) )
  }

  /** Need to rotate the convoy around to enforce the fact that it should repeat
    * To explain, consider the following example of all ppl to same beach, so just type delays
    * convoy1 = List[ (3), (), (3), (), (3) ]
    * convoy2 = List[ (2), (), (2), (), (2) ]
    * crucial thing to remember is all outputs at same time
    * so if input is ( 0, 1, 2, 3, 4, 0, 1, 2, 3, 4 )
    * convoy1 out is ( x, x, x, 0, x, 2, x, 4, 0, x, 2, x, 4 )
    * convoy2 out is ( x, x, x, 1, x, 3, x, 0, 1, x, 3, x, 0 )
    * can merge after one reg but can only do so if rotated
    * convoy1Mod = List[ (), (2), (), (2), (2) ] which can now be merged
    */
  def passLeftDown( convoys : List[Set[Person]] ) : List[Set[Person]] = {
    val shiftedConvoy = convoys.map( passLeftDown( _ ) )
    shiftedConvoy.takeRight(1) ++ shiftedConvoy.dropRight(1)
  }
  def passRightDown( convoys : List[Set[Person]] ) : List[Set[Person]] = {
    val shiftedConvoy = convoys.map( passRightDown( _ ) )
    shiftedConvoy.takeRight(1) ++ shiftedConvoy.dropRight(1)
  }
  def passLeftUp( convoys : List[Set[Person]] ) : List[Set[Person]] = {
    val shiftedConvoy = convoys.map( passLeftUp( _ ) )
    shiftedConvoy.drop(1) ++ shiftedConvoy.take(1)
  }
  def passRightUp( convoys : List[Set[Person]] ) : List[Set[Person]] = {
    val shiftedConvoy = convoys.map( passRightUp( _ ) )
    shiftedConvoy.drop(1) ++ shiftedConvoy.take(1)
  }

  def passLeftDown( oldCity : City ) : City = {
    val uniqueShifted = oldCity.getUnique().map( passLeftDown( _ ) )
    val origIdxs = oldCity.getIdxs()
    val shiftedIdxs = origIdxs.takeRight(1) ++ origIdxs.dropRight(1)
    val newCity = new City( noConvoy )
    newCity.addConvoysIn( uniqueShifted, shiftedIdxs )
    newCity
  }
  def passRightDown( oldCity : City ) : City = {
    val uniqueShifted = oldCity.getUnique().map( passRightDown( _ ) )
    val origIdxs = oldCity.getIdxs()
    val shiftedIdxs = origIdxs.takeRight(1) ++ origIdxs.dropRight(1)
    val newCity = new City( noConvoy )
    newCity.addConvoysIn( uniqueShifted, shiftedIdxs )
    newCity
  }
  def passLeftUp( oldCity : City ) : City = {
    val uniqueShifted = oldCity.getUnique().map( passLeftUp( _ ) )
    // verify is unique in case of add
    val origIdxs = oldCity.getIdxs()
    val rotatedIdxs = origIdxs.drop(1) ++ origIdxs.take(1)
    val muxedIdxs = {
      if ( isMux() )
        (muxSwitch zip rotatedIdxs).map( x => if ( x._1 ) -1 else x._2 )
      else
        rotatedIdxs
    }
    val idxUsed = muxedIdxs.distinct
    val newUnique = uniqueShifted.zipWithIndex.filter( x => idxUsed.contains(x._2) ).map( _._1 ).distinct
    val idxMapping = uniqueShifted.map( newUnique.indexOf( _ ) )

    val newCity = new City( noConvoy )
    newCity.addConvoysIn( newUnique, muxedIdxs.toList.map( newCity.mapIdx( idxMapping, _ ) ) )
    newCity
  }
  def passRightUp( oldCity : City ) : City = {
    val uniqueShifted = oldCity.getUnique().map( passRightUp( _ ) )
    val origIdxs = oldCity.getIdxs()
    val rotatedIdxs = origIdxs.drop(1) ++ origIdxs.take(1)
    val muxedIdxs = {
      if ( isMux() )
        (muxSwitch zip rotatedIdxs).map( x => if ( x._1 ) x._2 else -1 )
      else
        rotatedIdxs
    }
    val idxUsed = muxedIdxs.distinct
    val newUnique = uniqueShifted.zipWithIndex.filter( x => idxUsed.contains(x._2) ).map( _._1 ).distinct
    val idxMapping = uniqueShifted.map( newUnique.indexOf( _ ) )

    val newCity = new City( noConvoy )
    newCity.addConvoysIn( newUnique, muxedIdxs.toList.map( newCity.mapIdx( idxMapping, _ ) ) )
    newCity
  }


  def nextIncomplete() : Option[Road] = {
    if ( completed() )
      return None
    if ( !lRoad.isDefined )
      return Some( this )
    if ( !lRoad.get.completed() )
      return lRoad.get.nextIncomplete()
    assert( !isDelay(), "Should not have left path complete and be delay" )
    if ( !rRoad.isDefined )
      return Some( this )
    if ( !rRoad.get.completed() )
      return rRoad.get.nextIncomplete()
    None
  }

  private var thisOp : Option[Fixed] = None
  private var cond : Option[Bool] = None

  def implementRoad( beaches : List[Fixed], validIn : Bool ) : Fixed = {
    assert( completed(), "Road must be completed to be implemented" )
    if ( thisOp.isDefined )
      return thisOp.get
    if ( beachId >= 0 ) {
      thisOp = Some(beaches( beachId ))
    } else if ( isDelay() )
      thisOp = Some(RegNext( lRoad.get.implementRoad( beaches, validIn ) ))
    else if ( isMux() ) {
      val latShift = latency % muxSwitch.size
      val muxRotated = muxSwitch.takeRight( latShift ) ++ muxSwitch.dropRight( latShift )
      println( "Mux switch for Road@" + hashCode + " = " + muxSwitch )
      println( "Mux rotated with " + latShift + " for Road@" + hashCode + " = " + muxRotated )
      val ctr = Counter( validIn, noConvoy )
      val switchVec = Vec( muxRotated.map( Bool(_) ) )
      cond = Some(switchVec( ctr._1 ))
      thisOp = Some(RegNext( Mux( cond.get, rRoad.get.implementRoad( beaches, validIn ), lRoad.get.implementRoad( beaches, validIn ) )))
    } else // Add
      thisOp = Some(RegNext( lRoad.get.implementRoad( beaches, validIn ) + rRoad.get.implementRoad( beaches, validIn ) ))
    return thisOp.get
  }

  def printfCond( c : Module ) : Unit = {
    if ( isMux() ) {
      c.printf( "cond@" + hashCode + " = %x\n", cond.get )
      c.printf( "out@" + hashCode + " = %x\n", thisOp.get )
    }
    if ( lRoad.isDefined )
      lRoad.get.printfCond( c )
    if ( rRoad.isDefined )
      rRoad.get.printfCond( c )
  }

  override def toString() : String = {
    "Road@" + hashCode + "( " + roadType + ", " + shift + ", " + neg + ", " + beachId + " ) with " + getCity() + " -> ( " + {
      if ( lRoad.isDefined ) "Road@" + lRoad.get.hashCode else "" } + ", " + {
      if ( rRoad.isDefined ) "Road@" + rRoad.get.hashCode else "" } + " )\n" + {
      if ( lRoad.isDefined ) lRoad.get else "" } + {
      if ( rRoad.isDefined ) rRoad.get else "" }
  }

  def toDot( shorten : Boolean = true) : String = {
    val nodeProp = {
      if ( isMux() )
        "[shape=box]"
      else if ( isAdd() )
        "[shape=circle]"
      else if ( isDelay() )
        "[shape=ellipse]"
      else
        "[shape=plaintext, label=" + beachId + "]"
    }
    val thisNode = "Road" + hashCode + " " + nodeProp + ";\n"
    var currRd = this
    var prevRd = currRd
    var count = 0
    while( currRd.isDelay() && shorten ) {
      count += 1
      prevRd = currRd
      currRd = currRd.getLeft().get
    }
    val dec = {
      if ( isDelay() && shorten )
        "Road" + hashCode + " " + " [label=Delay_" + count +"_Road" + hashCode + "];\n"
      else
        "Road" + hashCode + " " + nodeProp + ";\n"
    }
    dec + {
      if ( prevRd.getLeft().isDefined )
        "Road" + hashCode + " -> " + "Road" + prevRd.getLeft().get.hashCode + " [color=green];\n" + prevRd.getLeft().get.toDot()
      else
        ""
    } + {
      if ( prevRd.getRight().isDefined )
        "Road" + hashCode + " -> " + "Road" + prevRd.getRight().get.hashCode + " [color=red];\n" + prevRd.getRight().get.toDot()
      else
        ""
    }
  }
}
