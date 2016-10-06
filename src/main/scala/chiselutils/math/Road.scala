package chiselutils.math

import collection.mutable.ArrayBuffer
import Chisel._

/** A Road in the city beaches problem
  */
class Road( noConvoy : Int ) {

  // the convoy that goes into the road
  // the convoy can change following merges
  private val convoyLayout = ArrayBuffer.fill( noConvoy ) { Set[Person]() }
  private val muxSwitch = ArrayBuffer.fill( noConvoy ) { false }

  // a method to perform merges
  def addConvoysIn( convoys : List[Set[Person]] ) = {
    assert( convoys.size == noConvoy, "convoys in should be the same size" )
    for ( i <- 0 until noConvoy ) {
      if ( convoyLayout( i ) != convoys( i ) ) {
        assert( convoyLayout(i).isEmpty, "Cannot merge over a non empty slot" )
        convoyLayout(i) = convoys(i)
      }
    }
    // look at convoy and check if can set isBeach (ie complete)
    val convoyCheck = convoyLayout.filterNot( _.isEmpty ).map( convoy => {
      val beachInfo = convoy.map( p => {
        if ( p.atBeach() )
          p.beachId
        else
          -p.beachId - 1
      })
      ( beachInfo.max, beachInfo.min )
    })

    assert( convoyCheck.size > 0, "Should never generate a useless road" )

    // if any at beach then all must be at same beach
    if ( convoyCheck.maxBy( _._1 )._1 >= 0 ) {
      val maxC = convoyCheck.maxBy( _._1 )
      val minC = convoyCheck.minBy( _._2 )
      assert( maxC._1 == maxC._2 && maxC == minC,
        "If any person is at a beach, they all must be at exactly the same beach" )
      complete = true
      beachId = maxC._1
    }
  }

  private var beachId = -1
  private var lRoad = None : Option[Road]
  private var rRoad = None : Option[Road]
  private var shift = 0
  private var neg = false
  private var complete = false
  private var roadType = -1

  def setLeft( l : Road ) { lRoad = Some(l) }
  def setRight( r : Road ) { rRoad = Some(r) }
  def getLeft() = lRoad
  def getRight() = rRoad

  def getShift() = shift
  def setShift( s : Int ) = { shift = s }
  def getNeg() = neg
  def setNeg( n : Boolean ) = { neg = n }

  def getMuxSwitch( i : Int ) = { muxSwitch(i) }
  def setMuxSwitch( i : Int, lr : Boolean ) = { muxSwitch(i) = lr }

  def convoyIn() = { convoyLayout.toList }

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
    return convoy.map( passLeftDown( _ ) )
  }
  def passRightDown( convoy : Set[Person] ) : Set[Person] = {
    return convoy.map( passRightDown( _ ) )
  }
  def passLeftUp( convoy : Set[Person] ) : Set[Person] = {
    return convoy.map( passLeftUp( _ ) )
  }
  def passRightUp( convoy : Set[Person] ) : Set[Person] = {
    return convoy.map( passRightUp( _ ) )
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
      println( "Mux switch for Road@" + hashCode + " = " + muxSwitch )
      val ctr = Counter( validIn, noConvoy )
      val switchVec = Vec( muxSwitch.map( Bool(_) ) )
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
    "Road@" + hashCode + "( " + roadType + ", " + shift + ", " + neg + ", " + beachId + " ) -> ( " + {
      if ( lRoad.isDefined ) "Road@" + lRoad.get.hashCode else "" } + ", " + {
      if ( rRoad.isDefined ) "Road@" + rRoad.get.hashCode else "" } + " )\n" + {
      if ( lRoad.isDefined ) lRoad.get else "" } + {
      if ( rRoad.isDefined ) rRoad.get else "" }
  }

}
