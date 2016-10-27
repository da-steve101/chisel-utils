package chiselutils.math

object Person {
  def apply( roadTripTime : Int, beachId : Int, shift : Int, neg : Boolean ) = {
    new Person( roadTripTime, beachId, shift, neg )
  }
}

class Person( val roadTripTime : Int, val beachId : Int, val shift : Int, val neg : Boolean ) {

  def moveTowardsBeach( roadShift : Int, roadNeg : Boolean ) : Person = {
    assert( shift >= roadShift, "Cannot shift negative amounts" )
    new Person( roadTripTime - 1, beachId, shift - roadShift, { if ( roadNeg ) !neg else neg } )
  }

  def moveTowardsCity( roadShift : Int, roadNeg : Boolean ) : Person = {
    new Person( roadTripTime + 1, beachId, shift + roadShift, { if ( roadNeg ) !neg else neg } )
  }

  def atBeach() = { roadTripTime == 0 }

  def toTuple() = { ( roadTripTime, beachId, shift, neg ) }

  override def equals(o: Any) = o match {
    case that: Person => that.hashCode == hashCode
    case _ => false
  }

  override lazy val hashCode : Int = toString().hashCode

  override def toString() : String = {
    "Person( " + roadTripTime + ", " + beachId + ", " + shift + ", " + neg + " )"
  }
}
