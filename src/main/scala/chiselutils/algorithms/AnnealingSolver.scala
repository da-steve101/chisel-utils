/** This file implements a simulated annealing solver
  */
package chiselutils.algorithms

import collection.immutable.HashSet
import collection.mutable.ArrayBuffer
import util.Random

object AnnealingSolver {

  /** Merge all the same C nodes that can be merged
    */
  def cMerge( nodeMap : HashSet[Node] ) : ( HashSet[Node], List[Node] ) = {
    val nodeCs = nodeMap.filter( _.isC() )
    val groupings = nodeCs.toList.groupBy( _.getUk() )
    val mergedNodes = groupings.map( grp => {
      if ( grp._2.size == 1 )
        grp._2(0)
      else {
        val ckAll = grp._2.map( _.getCk() ).transpose
        val newCk = ckAll.map( cks => {
          cks.reduce( (x,y) => {
            if ( x == -1 )
              y
            else {
              assert( x == y || y == -1, "Invalid merge of nodes" )
              x
            }
          })
        })
        val newNode = Node( grp._1, newCk )
        newNode.setC()
        // set all parents properly
        for ( n <- grp._2 ) {
          for ( p <- n.getParents() ) {
            if ( p.getL().isDefined && p.getL().get == n )
              p.setL( newNode )
            if ( p.getR().isDefined && p.getR().get == n )
              p.setR( newNode )
          }
        }
        newNode
      }
    })
    ( ( nodeMap -- nodeCs ) ++ mergedNodes, mergedNodes.toList )
  }

  /** look at binary reduction to implement
    */
  private def cycRemaining( dueTime : List[Int] ) : Int = {
    var cycs = dueTime.sorted
    while ( cycs.size > 1 ) {
      val m1 = cycs( cycs.size - 1 )
      val m2 = cycs( cycs.size - 2 )
      val mNew = math.min( m1, m2 ) - 1
      cycs = ( cycs.dropRight(2) ++ List(mNew) ).sorted
    }
    cycs.head
  }

  /** Return a set of all nodes created and a list of all nodes that are termination points
    */
  private def addPartition( n : Node ) : ( HashSet[Node], List[Node] ) = {
    assert( n.getUk().size == 1, "Can only add partition on a single set" )

    var addSet = n.getUk().head

    val regDelay = cycRemaining( addSet.toList.map( v => v(0) ) )
    val allNodes = collection.mutable.HashSet( n )
    var currNode = n
    for ( i <- 0 until regDelay ) {
      val newNode = Node( currNode.getUkPrev(), currNode.getCkPrev() )
      currNode.setL( newNode )
      currNode.setR( newNode )
      currNode.setB()
      allNodes += newNode
      currNode = newNode
    }

    addSet = currNode.getUk().head

    if ( addSet.size == 1 ){
      currNode.setC()
      assert( Node.satisfiesConstraints(currNode), "currNode should satisfy constraints" )
      return ( HashSet[Node]() ++ allNodes, List( currNode ) )
    }

    val addOpOrdering = ArrayBuffer[Set[Int]]()
    val elementList = addSet.toList
    val arriveTime = elementList.map( v => v(0) )
    val idxList = ( 0 until elementList.size ).map( Set( _ ) )
    var addOrder = arriveTime.zip( idxList ).sortBy( _._1 )
    addOpOrdering += addOrder( addOrder.size - 1 )._2
    addOpOrdering += addOrder( addOrder.size - 2 )._2
    while( addOrder.size > 1 ) {
      val m1 = addOrder( addOrder.size - 1 )
      val m2 = addOrder( addOrder.size - 2 )
      val mNew = ( math.min( m1._1, m2._1 ) - 1, m1._2 ++ m2._2 )
      addOpOrdering += mNew._2
      addOrder = ( addOrder.dropRight(2) ++ List( mNew ) ).sortBy( _._1 )
    }

    val combOp = addOpOrdering.dropRight( 1 ).last.toList.sorted
    val newLuK = Node.ukPrev( List( elementList.zipWithIndex.filter( e => combOp.contains( e._2 ) ).map( _._1 ).toSet ) )
    val newLcK = currNode.getCkPrev() // same as is an add
    val newLNode = Node( newLuK, newLcK )
    val newRuK = Node.ukPrev( List( elementList.zipWithIndex.filterNot( e => combOp.contains( e._2 ) ).map( _._1 ).toSet ) )
    val newRcK = currNode.getCkPrev() // same as is an add
    val newRNode = Node( newRuK, newRcK )
    currNode.setL( newLNode )
    currNode.setR( newRNode )
    currNode.setA()
    assert( Node.satisfiesConstraints(currNode), "currNode should satisfy constraints" )
    val lSide = addPartition( newLNode )
    val rSide = addPartition( newRNode )
    ( lSide._1 ++ rSide._1 ++ allNodes, lSide._2 ++ rSide._2 )
  }

  /** Return all nodes created and a list of uncomplete nodes
    */
  private def muxPartition( n : Node ) : ( HashSet[Node], List[Node] ) = {
    if ( n.getUk().size == 1 )
      return ( HashSet( n ), List( n ) )

    val ukTime = n.getUk().map( s => cycRemaining( s.toList.map( v => v(0) ) ))
    val regDelays = cycRemaining( ukTime )
    val allNodes = collection.mutable.HashSet( n )
    var currNode = n
    for ( i <- 0 until regDelays ) {
      val newNode = Node( currNode.getUkPrev(), currNode.getCkPrev() )
      currNode.setL( newNode )
      currNode.setR( newNode )
      currNode.setB()
      allNodes += newNode
      currNode = newNode
    }

    if ( n.getUk.size == 2 ) {
      val nA = Node( List( currNode.getUkPrev().head ), currNode.getCkPrev().map( ck => if ( ck == 0 ) 0 else -1 ) )
      val nB = Node( List( currNode.getUkPrev().last ), currNode.getCkPrev().map( ck => if ( ck == 1 ) 0 else -1 ) )
      currNode.setL( nA )
      currNode.setR( nB )
      currNode.setB()
      assert( Node.satisfiesConstraints(currNode), "currNode should satisfy constraints" )
      return  ( HashSet( nA, nB ) ++ allNodes, List( nA, nB ) )
    }

    val muxOpOrdering = ArrayBuffer[Set[Int]]()
    val currNodeTime = currNode.getUk().map( s => cycRemaining( s.toList.map( v => v(0) ) ))
    val idxList = ( 0 until currNodeTime.size ).map( Set( _ ) )
    var muxOrder = currNodeTime.zip( idxList ).sortBy( _._1 )
    muxOpOrdering += muxOrder( muxOrder.size - 1 )._2
    muxOpOrdering += muxOrder( muxOrder.size - 2 )._2
    while( muxOrder.size > 1 ) {
      val m1 = muxOrder( muxOrder.size - 1 )
      val m2 = muxOrder( muxOrder.size - 2 )
      val mNew = ( math.min( m1._1, m2._1 ) - 1, m1._2 ++ m2._2 )
      muxOpOrdering += mNew._2
      muxOrder = ( muxOrder.dropRight(2) ++ List( mNew ) ).sortBy( _._1 )
    }

    val combOp = muxOpOrdering.dropRight( 1 ).last.toList.sorted
    val combNot = ( 0 until currNodeTime.size ).filterNot( e => combOp.contains( e ) )
    val newLuK = currNode.getUkPrev().zipWithIndex.filter( e => combOp.contains( e._2 ) ).map( _._1 )
    val newLcK = currNode.getCkPrev().map( ck => combOp.indexOf( ck ) )
    val newLNode = Node( newLuK, newLcK )
    val newRuK = currNode.getUkPrev().zipWithIndex.filterNot( e => combOp.contains( e._2 ) ).map( _._1 )
    val newRcK = currNode.getCkPrev().map( ck => combNot.indexOf( ck ) )
    val newRNode = Node( newRuK, newRcK )
    currNode.setL( newLNode )
    currNode.setR( newRNode )
    currNode.setB()
    assert( Node.satisfiesConstraints(currNode), "currNode should satisfy constraints" )
    val lSide = muxPartition( newLNode )
    val rSide = muxPartition( newRNode )
    ( lSide._1 ++ rSide._1 ++ allNodes, lSide._2 ++ rSide._2 )
  }

  def needLatency( cp : List[List[Set[Vector[Int]]]] ) : Int = {
    val cpRemaining = cp.map( cList => {
      cycRemaining( cList.map( cSet => {
        cycRemaining( cSet.toList.map( v => v(0) ) )
      }))
    })
    -cpRemaining.min
  }

  /** Create the initial map from cp coords
    * Does the very naive thing of all mux then adder tree
    * Returns ( All the nodes, the parent nodes, the termination nodes )
    */
  def init( cp : List[List[Set[Vector[Int]]]] ) : ( HashSet[Node], List[Node], List[Node] ) = {
    val addTimes = cp.map( node => {
      node.map( s => cycRemaining( s.toList.map( v => v(0) )) )
    })
    val muxTimes = addTimes.map( cycRemaining( _ ) )
    val parentNodes = cp.map( Node( _ ) )
    val muxRes = parentNodes.map( p => muxPartition( p ) )
    val addRes = muxRes.map( n => {
      val ap = n._2.map( t => addPartition( t ) ).reduce( (x,y) => ( x._1 ++ y._1, x._2 ++ y._2 ) )
      ( ap._1 ++ n._1, ap._2 )
    }).reduce( (x,y) => ( x._1 ++ y._1, x._2 ++ y._2 ) )
    val mergedRes = cMerge( addRes._1 )
    ( mergedRes._1, parentNodes, mergedRes._2 )
  }

  def applyOperation( nodes : HashSet[Node], applyIfIncrease : Boolean ) : HashSet[Node] = {

    // pick a node randomally
    val n = Random.nextInt(nodes.size)
    val it = nodes.iterator.drop(n)
    val node = it.next

    if ( node.isC() )
      return nodes

    assert( node.isA() || node.isB(), "Cannot operate on termination node" )

    // choose an operation to try and apply
    val chooseMerge = Random.nextInt( 2 ) == 0

    val chooseL = Random.nextInt( 2 ) == 0

    val nSwap = {
      if ( chooseL )
        node.getL().get
      else
        node.getR().get
    }

    if ( chooseMerge ) {
      val parents = nSwap.getParents()
      val selNode = parents.find( p => p != node && p.getL() == node.getL() && p.getR() == node.getR() )
      if ( !selNode.isDefined )
        return nodes

      assert( nodes.contains( selNode.get ), "Selected node " + selNode.get + " not in map" )
      // perform it with some probability if it increases the cost
      val res = Transforms.tryMerge( node, selNode.get )
      if ( !res.isDefined )
        return nodes

      // clean up parents of merged nodes
      if ( node.getL().isDefined )
        node.getL().get.removeParent( node )
      if ( node.getR().isDefined && node.getR() != node.getL() )
        node.getR().get.removeParent( node )
      if ( selNode.get.getL().isDefined )
        selNode.get.getL().get.removeParent( selNode.get )
      if ( selNode.get.getR().isDefined && selNode.get.getR() != selNode.get.getL() )
        selNode.get.getR().get.removeParent( selNode.get )

      return ( ( nodes - node ) - selNode.get ) + res.get
    }

    val nOther = {
      if ( chooseL )
        node.getR().get
      else
        node.getL().get
    }

    if ( nSwap.getParents().size > 1 || nOther.getParents.size > 1 ) {
      if ( !applyIfIncrease )
        return nodes
      val nodeToSplit = {
        if ( nSwap.getParents().size > 1 )
          nSwap
        else
          nOther
      }
      val nodeList = Transforms.trySplit( nodeToSplit )

      // add new nodes and remove old one
      if ( nodeList.size > 0 ) {
        // clean up parents of merged nodes
        if ( nodeToSplit.getL().isDefined )
          nodeToSplit.getL().get.removeParent( nodeToSplit )
        if ( nodeToSplit.getR().isDefined && nodeToSplit.getR() != nodeToSplit.getL() )
          nodeToSplit.getR().get.removeParent( nodeToSplit )
        return ( nodes - nodeToSplit ) ++ nodeList
      }
      return nodes
    }

    // else swap
    val res = Transforms.trySwap( node, nSwap, applyIfIncrease )
    if ( res.size == 0 )
      return nodes

    // clean up parents of merged nodes
    if ( nSwap.getL().isDefined )
      nSwap.getL().get.removeParent( nSwap )
    if ( nSwap.getR().isDefined && nSwap.getR() != nSwap.getL() )
      nSwap.getR().get.removeParent( nSwap )
    if ( nOther != nSwap && nOther.getL().isDefined )
      nOther.getL().get.removeParent( nOther )
    if ( nOther != nSwap && nOther.getR().isDefined && nOther.getR() != nOther.getL() )
      nOther.getR().get.removeParent( nOther )

    ( ( nodes - nSwap ) - nOther ) ++ res
  }

  def run( nodesIn : HashSet[Node], iter : Int ) : HashSet[Node] = {
    var nodes = nodesIn
    val iterDub = iter.toDouble
    val iterPer = 100/iterDub
    val A = math.log( 0.01 )/iterDub
    for ( i <- 0 until iter ) {
      // decay the likelihood of performing an operation that makes the solution worse
      val threshold = (1 - math.exp( A*i ))/0.99
      val applyIfIncrease = Random.nextDouble >= threshold
      if ( (i % 100000) == 0 )
        println( "progress = " + (i*iterPer ) + "%, threshold = " + threshold + ", cost = " + nodes.size )
      nodes = applyOperation( nodes, applyIfIncrease )
    }

    // run a merge on all nodes here?

    // look at dropping latency if possible?

    nodes
  }

  /** Convert the node set to a .dot graph file
    */
  def toDot( nodes : HashSet[Node], fileOut : String ) : Unit = {
    val file = new java.io.File( fileOut + ".tmp" )
    val bw = new java.io.BufferedWriter( new java.io.FileWriter(file) )
    bw.write("digraph G {\n")
    for ( n <- nodes ) {
      assert( n.isA() || n.isB() || n.isC(), "node has to be of type A,B or C" )
      val shape = {
        if ( n.isA() )
          "circle"
        else if ( n.isB() )
          "box"
        else
          "plaintext"
      }
      val label = {
        if ( n.isA() )
          "A_" + n.hashCode.abs
        else if ( n.isB() )
          "B_" + n.hashCode.abs
        else
          "C_" + n.hashCode.abs
      }
      bw.write( "N" + n.hashCode.abs + " [shape=" + shape + ", label=" + label + "];\n" )
      if ( n.getL().isDefined ) {
        val lValid = nodes.contains( n.getL().get ) && n.getL().get.hasParent( n )
        if ( !lValid ) {
          println( "n = " + n )
          println( "n.getL() = " + n.getL() )
          println( "n.getL().getParents() = " + n.getL().get.getParents() )
        }
        assert( lValid, "Can only reference nodes inside map" )
        bw.write( "N" + n.hashCode.abs + " -> N" + n.getL().get.hashCode.abs + ";\n" )
      }
      if ( n.getR().isDefined ) {
        val rValid = nodes.contains( n.getR().get ) && n.getR().get.hasParent( n )
        if ( !rValid ) {
          println( "n = " + n )
          println( "n.getR() = " + n.getR() )
          println( "n.getR().getParents() = " + n.getR().get.getParents() )
        }
        assert( rValid, "Can only reference nodes inside map" )
        bw.write( "N" + n.hashCode.abs + " -> N" + n.getR().get.hashCode.abs + ";\n" )
      }
    }
    bw.write("}\n")
    bw.close()
    file.renameTo( new java.io.File( fileOut ) )
  }
}
