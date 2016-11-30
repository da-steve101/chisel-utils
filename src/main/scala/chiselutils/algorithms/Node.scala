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
  private def mapIdx( mapping : Seq[Int], idx : Int ) = {
    if ( idx == -1 )
      idx
    else
      mapping( idx )
  }

  /** Create a node cp coords
    */
  def apply( nodeVals : Seq[Set[Seq[Int]]] ) : Node = {
    val uk = nodeVals.distinct.filter( _.size != 0 )
    val ck = nodeVals.map( uk.indexOf( _ ) )
    apply( uk, ck )
  }

  /** Create a node from compressed cp coords
    */
  def apply( uk : Seq[Set[Seq[Int]]], ck : Seq[Int] ) : Node = {
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

  def apply( uk : List[Set[Seq[Int]]], ck : Seq[Int] ) : Node = {
    Node( uk.to[Seq], ck )
  }

  def apply( uk : Vector[Set[Seq[Int]]], ck : Seq[Int] ) : Node = {
    Node( uk.to[Seq], ck )
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
    val violation = n.ck.zipWithIndex.find( cki => { n.getUki(cki._1).size != 0 &&
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
    val violation = n.ck.zipWithIndex.find( cki => { n.getUki(cki._1).size != 0 &&
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

  /** Check that there are no extra numbers being put in there
    */
  def isMinimal( n : Node ) : Boolean = {
    val parents = n.getParents()
    val nUk = n.getUkNext()
    val nCk = n.getCkNext()
    // find out which mux have n as input and which cycle
    val ckPar = parents.map( p => {
      if ( p.isA() )
        p.ck
      else {
        p.ck.zip( nCk ).map( cks => {
          if ( cks._1 == -1 || cks._2 == -1 )
            -1
          else if ( p.uk( cks._1 ) == nUk( cks._2 ) )
            cks._1
          else
            -1
        })
      }
    })
    !( 0 until nCk.size ).find( idx => {
      nCk(idx) != -1 && !ckPar.find( p => p(idx) != -1 ).isDefined
    }).isDefined
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
        !satisfiesConstraints( np ) || np.isC()
      }).isDefined
      if ( violated )
        return false
      return isMinimal( n )
    }
    true
  }

  def ukPrev( uk : Seq[Set[Seq[Int]]] ) : Seq[Set[Seq[Int]]] = {
    uk.map( uki => uki.map( v => { List( v(0) - 1 ) ++ v.drop(1) }.to[Seq]))
  }
  def ukNext( uk : Seq[Set[Seq[Int]]] ) : Seq[Set[Seq[Int]]] = {
    uk.map( uki => uki.map( v => { List( v(0) + 1 ) ++ v.drop(1) }.to[Seq] ))
  }

}

class Node( val dim : Int, val nodeSize : Int, val uk : Seq[Set[Seq[Int]]], val ck : Seq[Int] ) {

  assert( dim >= 1, "The dimension of p must be atleast 1" )

  private var lNode : Option[Node] = None
  private var rNode : Option[Node] = None
  private val parents = collection.mutable.Set[Node]()
  private var nodeType = -1
  private val available = new AtomicBoolean( false );

  def getModIdx( i : Int ) : Int = { ck( ( i + nodeSize - 1 ) % nodeSize ) }
  def getModSet( i : Int ) : Set[Seq[Int]] = {
    val idx = getModIdx( i )
    if ( idx == -1 )
      return Set[Seq[Int]]()
    uk( idx )
  }

  private def getP0( p : Seq[Int] ) = p.head
  private def incr( p : Seq[Int] ) : Seq[Int] = List( p.head + 1 ) ++ p.drop(1)
  private def incr( q : Set[Seq[Int]] )  : Set[Seq[Int]] = q.map( incr(_) )

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

  def getCkPrev() = { ck.drop(1) ++ ck.take(1) }
  def getCkNext() = { ck.takeRight(1) ++ ck.dropRight(1) }
  def getUki( i : Int ) : Set[Seq[Int]] = {
    if ( i == -1 )
      return Set[Seq[Int]]()
    uk( i )
  }
  def getCki( i : Int ) : Set[Seq[Int]] = getUki( ck( i ) )

  def getUkPrev() : Seq[Set[Seq[Int]]] = { uk.map( uki => uki.map( v => { List( v(0) - 1 ) ++ v.drop(1) }.to[Seq] )) }
  def getUkNext() : Seq[Set[Seq[Int]]] = { uk.map( uki => uki.map( v => { List( v(0) + 1 ) ++ v.drop(1) }.to[Seq] )) }
  def getL() = { lNode }
  def getR() = { rNode }
  def setL( n : Option[Node] ) = {
    assert( isLocked(), "Node should be locked to setL" )
    if ( lNode.isDefined && lNode != rNode )
      lNode.get.removeParent( this )
    lNode = n
    if ( n.isDefined )
      n.get.addParent( this )
  }
  def setR( n : Option[Node] ) = {
    assert( isLocked(), "Node should be locked to setR" )
    if ( rNode.isDefined && lNode != rNode )
      rNode.get.removeParent( this )
    rNode = n
    if ( n.isDefined )
      n.get.addParent( this )
  }
  def getParents() : Seq[Node] = parents.toVector
  def getParentSet() : Set[Node] = parents.toSet
  def intersectPar( otherP : Set[Node] ) = { otherP.intersect( parents.toSet ) }
  private def addParent( n : Node ) = {
    assert( isLocked(), "Node should be locked to add parent" )
    parents += n
  }
  private def removeParent( n : Node ) = {
    assert( isLocked(), "Node should be locked to remove parent" )
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
    "Node@" + hashCode + "(" + letter() + ") { " + uk + " } { " + ck + " }"
  }

}
