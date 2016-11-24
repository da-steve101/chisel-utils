/** This file contains a Node in the sum scheduler set problem
  * An initial solution is created, then transformations are performed using
  * simulated annealing
  */
package chiselutils.algorithms

import collection.mutable.ArrayBuffer
import java.util.concurrent.atomic.AtomicBoolean

object Node {

  /** Look up an index with a mapping
    */
  private def mapIdx( mapping : List[Int], idx : Int ) = {
    if ( idx == -1 )
      idx
    else
      mapping( idx )
  }

  /** Create a node cp coords
    */
  def apply( nodeVals : List[Set[Vector[Int]]] ) : Node = {
    val uk = nodeVals.distinct.filter( _.size != 0 )
    val ck = nodeVals.map( uk.indexOf( _ ) )
    apply( uk, ck )
  }

  /** Create a node from compressed cp coords
    */
  def apply( uk : List[Set[Vector[Int]]], ck : List[Int] ) : Node = {
    val dim = uk.head.iterator.next.size
    val nodeSize = ck.size
    // ensure uk is sorted
    val ukSorted = uk.zipWithIndex.sortBy( _._1.hashCode )
    val mapping = ukSorted.map( _._2 ).zipWithIndex.sortBy( _._1 ).map( _._2 )
    val ukOut = ukSorted.map( _._1 )
    val ckOut = ck.map( cki => mapIdx( mapping, cki ) )
    val n = new Node( dim, nodeSize, ukOut, ckOut )
    n
  }

  /** Check if a node satisfies constraint A
    */
  def satisfiesConstraintA( n : Node ) : Boolean = {
    /* Constraint A states that for all cki in ck,
     * cki = ( incr( c_(l,i-1%n) U c_(r,i-1%n) )
     *       AND |c_(l,i-1%n)| != 0 AND |c_(r,i-1%n)| != 0 ) OR
     *       |cki| = 0
     */
    if ( !n.getL().isDefined || !n.getR().isDefined )
      return false

    // look for violation of constraint using find
    val violation = n.getCk().zipWithIndex.find( cki => { n.getUki(cki._1).size != 0 &&
      ( n.getL().get.getModIdx( cki._2 ) == -1 ||
        n.getR().get.getModIdx( cki._2 ) == -1 ||
        !n.testAddUnion( cki._2 )
      )
    }).isDefined
    !violation
  }

  /** Check if a node satisfies constraint B
    */
  def satisfiesConstraintB( n : Node ) : Boolean = {
    /* Constraint B states that for all cki in ck,
     * cki = incr( c_(l,i-1%n) ) OR incr( c_(r,i-1%n) ) OR |cki| = 0
     */
    if ( !n.getL().isDefined || !n.getR().isDefined )
      return false

    // look for violation using find
    val violation = n.getCk().zipWithIndex.find( cki => { n.getUki(cki._1).size != 0 &&
      !n.testMux( cki._2 )
    }).isDefined
    !violation
  }

  /** Check if a node satisfies constraint C
    */
  def satisfiesConstraintC( n : Node ) : Boolean = {
    /* Constraint C states that the union of all cki in ck has a size of 1
     * and that element has a value of 0 in the first position of the list
     */
    n.testTermination( )
  }

  def satisfiesConstraints( n : Node ) : Boolean = {
    if ( n.isA() )
      return satisfiesConstraintA( n )
    if ( n.isB() )
      return satisfiesConstraintB( n )
    if ( n.isC() )
      return satisfiesConstraintC( n )
    false
  }

  /** Check that this node satisfies constraints
    * Also check lNode, rNode and parents
   */
  def verifyNode( n : Node ) : Boolean = {
    if ( !satisfiesConstraints( n ) )
      return false
    if ( n.getL().isDefined && !satisfiesConstraints( n.getL().get ) )
      return false
    if ( n.getR().isDefined && !satisfiesConstraints( n.getR().get ) )
      return false
    if ( n.getParents().size > 0 ) {
      val violated = n.getParents().find( np => {
        !satisfiesConstraints( np )
      }).isDefined
      return !violated
    }
    true
  }

  def ukPrev( uk : List[Set[Vector[Int]]] ) : List[Set[Vector[Int]]] = {
    uk.map( uki => uki.map( v => { Vector( v(0) - 1 ) ++ v.drop(1) }))
  }
  def ukNext( uk : List[Set[Vector[Int]]] ) : List[Set[Vector[Int]]] = {
    uk.map( uki => uki.map( v => { Vector( v(0) + 1 ) ++ v.drop(1) }))
  }

}

class Node( val dim : Int, val nodeSize : Int, uk : List[Set[Vector[Int]]], ck : List[Int] ) {

  assert( dim >= 1, "The dimension of p must be atleast 1" )

  private var lNode : Option[Node] = None
  private var rNode : Option[Node] = None
  private val parents = collection.mutable.Set[Node]()
  private var nodeType = -1
  private val available = new AtomicBoolean();

  def getModIdx( i : Int ) : Int = { ck( ( i + nodeSize - 1 ) % nodeSize ) }
  def getModSet( i : Int ) : Set[Vector[Int]] = {
    val idx = getModIdx( i )
    if ( idx == -1 )
      return Set[Vector[Int]]()
    uk( idx )
  }

  private def getP0( p : Vector[Int] ) = p.head
  private def incr( p : Vector[Int] ) : Vector[Int] = Vector( p.head + 1 ) ++ p.drop(1)
  private def incr( q : Set[Vector[Int]] )  : Set[Vector[Int]] = q.map( incr(_) )

  def isLocked() = !available.get()
  def unlockNode() = {
    assert( isLocked(), "Trying to unlock available node" )
    available.set( true )
  }
  def lockNode() = {
    available.compareAndSet( true, false )
  }

  def isA() = nodeType == 0
  def isB() = nodeType == 1
  def isC() = nodeType == 2
  def setA() = { nodeType = 0 }
  def setB() = { nodeType = 1 }
  def setC() = { nodeType = 2 }
  def letter() = {
    if ( nodeType == 0 )
      "A"
    else if ( nodeType == 1 )
      "B"
    else if ( nodeType == 2 )
      "C"
    else
      "_"
  }

  def getCk() = ck.toList
  def getCkPrev() = { ck.drop(1).toList ++ ck.take(1).toList }
  def getCkNext() = { ck.takeRight(1).toList ++ ck.dropRight(1).toList }
  def getUki( i : Int ) : Set[Vector[Int]] = {
    if ( i == -1 )
      return Set[Vector[Int]]()
    uk( i )
  }
  def getCki( i : Int ) : Set[Vector[Int]] = getUki( ck( i ) )

  def getUk() = uk.toList
  def getUkPrev() = { uk.toList.map( uki => uki.map( v => { Vector( v(0) - 1 ) ++ v.drop(1) })) }
  def getUkNext() = { uk.toList.map( uki => uki.map( v => { Vector( v(0) + 1 ) ++ v.drop(1) })) }
  def getL() = lNode
  def getR() = rNode
  def setL( n : Option[Node] ) = {
    if ( lNode.isDefined && lNode != rNode )
      lNode.get.removeParent( this )
    lNode = n
    if ( n.isDefined )
      n.get.addParent( this )
  }
  def setR( n : Option[Node] ) = {
    if ( rNode.isDefined && lNode != rNode )
      rNode.get.removeParent( this )
    rNode = n
    if ( n.isDefined )
      n.get.addParent( this )
  }
  def getParents() = parents.toList
  def getParentSet() = parents.toSet
  def intersectPar( otherP : Set[Node] ) = { otherP.intersect( parents.toSet ) }
  private def addParent( n : Node ) = {
    parents += n
  }
  private def removeParent( n : Node ) = {
    assert( hasParent(n), "Trying to remove non parent " + n )
    parents -= n
  }
  def hasParent( n : Node ) : Boolean = { parents.contains( n ) }

  /** Returns if cki = ( incr( c_(l,i-1%n) U c_(r,i-1%n) )
    */
  def testAddUnion( i : Int ) : Boolean = {
    if ( !lNode.isDefined || !rNode.isDefined )
      return false
    val leftSet = lNode.get.getModSet( i )
    val rightSet = rNode.get.getModSet( i )
    val allSet = incr( leftSet ++ rightSet )
    val thisSet = getCki( i )
    if ( leftSet.size + rightSet.size != thisSet.size || allSet.size != thisSet.size )
      return false
    allSet == thisSet
  }

  /** Returns if cki = incr( c_(l,i-1%n) ) or incr( c_(r,i-1%n) )
    */
  def testMux( i : Int ) : Boolean = {
    if ( !lNode.isDefined || !rNode.isDefined )
      return false
    val leftSet = incr( lNode.get.getModSet( i ) )
    val rightSet = incr( rNode.get.getModSet( i ) )
    leftSet == getCki( i ) || rightSet == getCki( i )
  }

  /** Returns if the union of all cki is size 1 and that set has a size of 1
    * And if the element in that set has it's first element as 0
    */ 
  def testTermination() : Boolean = {
    if ( lNode.isDefined || rNode.isDefined )
      return false
    if ( uk.size != 1 || uk.head.size != 1)
      return false
    uk.head.find( p => getP0( p ) == 0 ).isDefined
  }

  override def toString() : String = {
    "Node@" + hashCode + "(" + letter() + ") { " + uk.toList + " } { " + getCk() + " }"
  }

  override lazy val hashCode : Int = uk.hashCode + ck.hashCode

  override def equals( that : Any ) : Boolean = that match {
    case n : Node => { n.hashCode == hashCode }
    case _ => false
  }

}
