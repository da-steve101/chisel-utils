package chiselutils.math

import collection.mutable.ArrayBuffer

class City( noConvoy : Int ) {

  private val uniqueConvoys = ArrayBuffer[Set[Person]]()
  private val convoyIdxs = ArrayBuffer.fill( noConvoy ) { 0 }

  private def getIdxMapping( uCIn : List[Set[Person]] ) : List[Int] = {
    val ucIdxs = ArrayBuffer[Int]() ++ uCIn.map( uniqueConvoys.indexOf( _ ) )
    var newIdx = uniqueConvoys.size
    for ( i <- 0 until uCIn.size ) {
      if ( ucIdxs(i) == -1 ) {
        ucIdxs(i) = newIdx
        newIdx += 1
      }
    }
    // add new convoy combinations
    uniqueConvoys ++= ( uCIn zip ucIdxs ).filter( _._2 >= uniqueConvoys.size ).map( _._1 )

    ucIdxs.toList
  }

  def mapIdx( idxMapping : List[Int], idx : Int ) : Int = {
    if ( idx == -1 )
      -1
    else
      idxMapping( idx )
  }

  private def verifyBeach() : ( Boolean, Int ) = {
    // look at convoy and check if can set isBeach (ie complete)
    val convoyCheck = uniqueConvoys.filterNot( _.isEmpty ).map( convoy => {
      val beachInfo = convoy.map( p => {
        if ( p.atBeach() )
          p.beachId
        else
          -p.beachId - 1
      })
      ( beachInfo.max, beachInfo.min )
    })

    // if any at beach then all must be at same beach
    if ( convoyCheck.maxBy( _._1 )._1 >= 0 ) {
      val maxC = convoyCheck.maxBy( _._1 )
      val minC = convoyCheck.minBy( _._2 )
      assert( maxC._1 == maxC._2 && maxC == minC,
        "If any person is at a beach, they all must be at exactly the same beach" )
      return ( true, maxC._1 )
    }
    ( false, -1 )
  }

  def addConvoysIn( uCIn : List[Set[Person]], cIdxs : List[Int]  ) : ( Boolean, Int ) = {
    assert( cIdxs.size == noConvoy, "convoys in should be the same size" )
    // check unique convoys has all the same
    val ucIdxs = getIdxMapping( uCIn )

    assert( uniqueConvoys.size > 0, "Should never generate a useless road" )

    for ( i <- 0 until noConvoy ) {
      if ( convoyIdxs( i ) != mapIdx( ucIdxs, cIdxs( i ) ) )
        setIdx( i, mapIdx( ucIdxs, cIdxs( i ) ) )
    }

    verifyBeach()
  }

  def getUnique() = uniqueConvoys.toList
  def getIdxs() = convoyIdxs.toList
  def getIdx( i : Int ) = convoyIdxs(i)
  def setIdx( i : Int, x : Int ) = {
    assert( convoyIdxs( i ) == 0, "Cannot set a non empty slot" )
    convoyIdxs( i ) = x
  }

  def addConvoysIn( newCity : City ) = {
    val idxMapping = getIdxMapping( newCity.getUnique() )

    for ( i <- 0 until noConvoy ) {
      if ( convoyIdxs( i ) != mapIdx( idxMapping, newCity.getIdx(i) ) )
        setIdx( i, mapIdx( idxMapping, newCity.getIdx( i ) ) )
    }

    verifyBeach()
  }

  override def toString() : String = {
    "City( " + getUnique() + ", " + getIdxs() + ")"
  }
}
