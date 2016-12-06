/** This file implements a simulated annealing solver
  */
package chiselutils.algorithms

import Chisel._
import collection.mutable.ArrayBuffer
import util.Random

object AnnealingSolver {

  /** Merge all the same C nodes that can be merged
    */
  def cMerge( nodeMap : Set[Node] ) : ( Set[Node], Seq[Node] ) = {
    val nodeCs = nodeMap.filter( _.isC() )
    val groupings = nodeCs.toVector.groupBy( _.uk )
    val mergedNodes = groupings.map( grp => {
      if ( grp._2.size == 1 )
        grp._2(0)
      else {
        val ckAll = grp._2.map( _.ck ).transpose
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
              p.setL( Some(newNode) )
            if ( p.getR().isDefined && p.getR().get == n )
              p.setR( Some(newNode) )
          }
        }
        newNode
      }
    })
    ( ( nodeMap -- nodeCs ) ++ mergedNodes, mergedNodes.toList )
  }

  def mergeAll( nodes : Set[Node] ) : ( Set[Node], Seq[Node] ) =  {
    val cRes = cMerge( nodes )
    val newNodes = collection.mutable.Set[Node]() ++ cRes._1
    val mergedSet = collection.mutable.Set[Node]() ++ cRes._2

    // do a flood fill iteration
    while( mergedSet.size > 0 ) {

      // choose a node from mergedSet
      val node = mergedSet.iterator.next
      mergedSet -= node
      // dont merge the start or end nodes
      if ( node.isC() || node.getParents.size == 0 ) {
        mergedSet ++= node.getParents()
      } else {
        val nSwap = node.getL().get
        val nOther = node.getR().get

        // see if that node can be merged
        val parents = nOther.intersectPar( nSwap.getParentSet() )
        val selNode = parents.find( p => p != node && (
          (p.getL() == node.getL() && p.getR() == node.getR()) ||
          (p.getL() == node.getR() && p.getR() == node.getL())
        ))
        if ( selNode.isDefined ) {
          // perform it with some probability if it increases the cost
          val res = performMerge( node, selNode.get )
          if ( res.isDefined ) {
            newNodes -= node
            newNodes -= selNode.get
            newNodes += res.get
            mergedSet += res.get
          } else
            mergedSet ++= node.getParents()
        } else
          mergedSet ++= node.getParents()
      }
    }
    ( Set[Node]() ++ newNodes, cRes._2 )
  }

  /** look at binary reduction to implement
    */
  private def cycRemaining( dueTime : Seq[Int] ) : Int = {
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
  private def addPartition( n : Node ) : ( Set[Node], Seq[Node] ) = {
    assert( n.uk.size == 1, "Can only add partition on a single set" )

    var addSet = n.uk.head

    val regDelay = cycRemaining( addSet.toVector.map( v => v(0) ) )
    val allNodes = collection.mutable.Set( n )
    var currNode = n
    for ( i <- 0 until regDelay ) {
      val newNode = Node( currNode.getUkPrev(), currNode.getCkPrev() )
      currNode.setL( Some(newNode) )
      currNode.setR( Some(newNode) )
      currNode.setB()
      allNodes += newNode
      currNode = newNode
    }

    addSet = currNode.uk.head

    if ( addSet.size == 1 ){
      currNode.setC()
      assert( Node.satisfiesConstraints(currNode), "currNode should satisfy constraints" )
      return ( Set[Node]() ++ allNodes, List( currNode ) )
    }

    val addOpOrdering = ArrayBuffer[Set[Int]]()
    val elementList = addSet.toVector
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
      addOrder = ( addOrder.dropRight(2) ++ Vector( mNew ) ).sortBy( _._1 )
    }

    val combOp = addOpOrdering.dropRight( 1 ).last.toSet
    val newLuK = Node.ukPrev( Vector( elementList.zipWithIndex.filter( e => combOp.contains( e._2 ) ).map( _._1 ).toSet ) )
    val newLcK = currNode.getCkPrev() // same as is an add
    val newLNode = Node( newLuK, newLcK )
    val newRuK = Node.ukPrev( Vector( elementList.zipWithIndex.filterNot( e => combOp.contains( e._2 ) ).map( _._1 ).toSet ) )
    val newRcK = currNode.getCkPrev() // same as is an add
    val newRNode = Node( newRuK, newRcK )
    currNode.setL( Some(newLNode) )
    currNode.setR( Some(newRNode) )
    currNode.setA()
    assert( Node.satisfiesConstraints(currNode), "currNode should satisfy constraints" )
    val lSide = addPartition( newLNode )
    val rSide = addPartition( newRNode )
    ( lSide._1 ++ rSide._1 ++ allNodes, lSide._2 ++ rSide._2 )
  }

  /** Return all nodes created and a list of uncomplete nodes
    */
  private def muxPartition( n : Node ) : ( Set[Node], Seq[Node] ) = {
    if ( n.uk.size == 1 )
      return ( Set( n ), List( n ) )

    val ukTime = n.uk.map( s => cycRemaining( s.toVector.map( v => v(0) ) ))
    val regDelays = cycRemaining( ukTime )
    val allNodes = collection.mutable.Set( n )
    var currNode = n
    for ( i <- 0 until regDelays ) {
      val newNode = Node( currNode.getUkPrev(), currNode.getCkPrev() )
      currNode.setL( Some(newNode) )
      currNode.setR( Some(newNode) )
      currNode.setB()
      allNodes += newNode
      currNode = newNode
    }

    if ( n.uk.size == 2 ) {
      val nA = Node( Vector( currNode.getUkPrev().head ), currNode.getCkPrev().map( ck => if ( ck == 0 ) 0 else -1 ) )
      val nB = Node( Vector( currNode.getUkPrev().last ), currNode.getCkPrev().map( ck => if ( ck == 1 ) 0 else -1 ) )
      currNode.setL( Some(nA) )
      currNode.setR( Some(nB) )
      currNode.setB()
      assert( Node.satisfiesConstraints(currNode), "currNode should satisfy constraints" )
      return  ( Set( nA, nB ) ++ allNodes, List( nA, nB ) )
    }

    val muxOpOrdering = ArrayBuffer[Set[Int]]()
    val currNodeTime = currNode.uk.map( s => cycRemaining( s.toVector.map( v => v(0) ) ))
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

    val combOp = muxOpOrdering.dropRight( 1 ).last.toVector.sorted
    val combNot = ( 0 until currNodeTime.size ).filterNot( e => combOp.contains( e ) )
    val newLuK = currNode.getUkPrev().zipWithIndex.filter( e => combOp.contains( e._2 ) ).map( _._1 )
    val newLcK = currNode.getCkPrev().map( ck => combOp.indexOf( ck ) )
    val newLNode = Node( newLuK, newLcK )
    val newRuK = currNode.getUkPrev().zipWithIndex.filterNot( e => combOp.contains( e._2 ) ).map( _._1 )
    val newRcK = currNode.getCkPrev().map( ck => combNot.indexOf( ck ) )
    val newRNode = Node( newRuK, newRcK )
    currNode.setL( Some(newLNode) )
    currNode.setR( Some(newRNode) )
    currNode.setB()
    assert( Node.satisfiesConstraints(currNode), "currNode should satisfy constraints" )
    val lSide = muxPartition( newLNode )
    val rSide = muxPartition( newRNode )
    ( lSide._1 ++ rSide._1 ++ allNodes, lSide._2 ++ rSide._2 )
  }

  def needLatency( cp : Seq[Seq[Set[Seq[Int]]]] ) : Int = {
    val cpRemaining = cp.filter( cList => {
      cList.find( !_.isEmpty ).isDefined
    }).map( cList => {
      val listRes = cList.filterNot( _.isEmpty ).map( cSet => {
        cycRemaining( cSet.toVector.map( v => v(0) ) )
      })
      cycRemaining( listRes )
    })
    -cpRemaining.min
  }

  /** Create the initial map from cp coords
    * Does the very naive thing of all mux then adder tree
    * Returns ( All the nodes, the parent nodes, the termination nodes )
    */
  def init( cp : Seq[Seq[Set[Seq[Int]]]] ) : ( Set[Node], Seq[Node], Seq[Node] ) = {
    val addTimes = cp.filter( cList => {
      cList.find( !_.isEmpty ).isDefined
    }).map( node => {
      node.filterNot( _.isEmpty ).map( s =>
        cycRemaining( s.toVector.map( v => v(0) ))
      )
    })
    val muxTimes = addTimes.map( cycRemaining( _ ) )
    val parentNodes = cp.map( Node( _ ) )
    val muxRes = parentNodes.map( p => muxPartition( p ) )
    val addRes = muxRes.map( n => {
      val ap = n._2.map( t => addPartition( t ) ).reduce( (x,y) => ( x._1 ++ y._1, x._2 ++ y._2 ) )
      ( ap._1 ++ n._1, ap._2 )
    }).reduce( (x,y) => ( x._1 ++ y._1, x._2 ++ y._2 ) )
    println( "merge nodes for smaller initial graph" )
    val mergedRes = mergeAll( addRes._1 )
    for ( n <- mergedRes._1 )
      n.unlockNode()
    ( mergedRes._1, parentNodes, mergedRes._2 )
  }

  private def lockNodes( nodesIn : Set[Node], alreadyLocked : Set[Node] ) : (Boolean, Set[Node]) = {
    val nodesLocked = collection.mutable.Set[Node]()
    // lock parents of node
    for( p <- nodesIn) {
      if ( !alreadyLocked.contains( p ) && !nodesLocked.contains(p) ) {
        val lockRes = p.lockNode()
        if ( !lockRes ) {
          nodesLocked.map( n => n.unlockNode() )
          return (false, Set[Node]() )
        }
        nodesLocked += p
      }
    }
    return ( true, nodesLocked.toSet )
  }

  /** Lock nodes needed
    * Need to lock in order to ensure that the node cant change
    */
  private def acquireLocks( node : Node, lockRes : Boolean ) : (Boolean, Set[Node]) = {
    if ( node.isC() ) {
      if ( lockRes )
        node.unlockNode()
      return ( false, Set[Node]() )
    }

    if ( !lockRes )
      return (false, Set[Node]())
    assert( node.getL().isDefined && node.getR().isDefined, "Node " + node + " is invalid" )

    val nodesLocked = collection.mutable.Set[Node]()
    nodesLocked += node

    // lock L
    val nodeL = node.getL().get
    val lockLRes = nodeL.lockNode()
    if ( !lockLRes ) {
      nodesLocked.map( n => n.unlockNode() )
      return (false, Set[Node]())
    }
    nodesLocked += nodeL

    // lock R
    val nodeR = node.getR().get
    if ( !nodesLocked.contains(nodeR) ) {
      val lockRRes = nodeR.lockNode()
      if ( !lockRRes ) {
        nodesLocked.map( n => n.unlockNode() )
        return (false, Set[Node]())
      }
      nodesLocked += nodeR
    }

    // lock L/R of L/R
    val nodeLL = { if ( nodeL.getL().isDefined ) List( nodeL.getL().get ) else List[Node]() }
    val nodeLR = { if ( nodeL.getR().isDefined ) List( nodeL.getR().get ) else List[Node]() }
    val nodeRL = { if ( nodeR.getL().isDefined ) List( nodeR.getL().get ) else List[Node]() }
    val nodeRR = { if ( nodeR.getR().isDefined ) List( nodeR.getR().get ) else List[Node]() }
    for ( nIn <- nodeLL ++ nodeLR ++ nodeRL ++ nodeRR ) {
      if ( !nodesLocked.contains(nIn) ) {
        val lock = nIn.lockNode()
        if ( !lock ) {
          nodesLocked.map( n => n.unlockNode() )
          return (false, Set[Node]())
        }
        nodesLocked += nIn
      }
    }

    // lock parents of node, nodeL and nodeR
    val lockPar = lockNodes( node.getParentSet() ++ nodeL.getParentSet() ++ nodeR.getParentSet(), nodesLocked.toSet )
    if ( !lockPar._1 ) {
      nodesLocked.map( n => n.unlockNode() )
      return (false, Set[Node]())
    }
    nodesLocked ++= lockPar._2

    (true, nodesLocked.toSet)
  }

  def performMerge( nA : Node, nB : Node ) : Option[Node] = {

    val res = Transforms.tryMerge( nA, nB )
    if ( !res.isDefined )
      return res

    // clean up parents of merged nodes
    nA.setL( None )
    nA.setR( None )
    nB.setL( None )
    nB.setR( None )

    res
  }

  def performSplit( nodeToSplit : Node ) : Seq[Node] = {

    val nodeList = Transforms.trySplit( nodeToSplit )

    // add new nodes and remove old one
    if ( nodeList.size > 0 ) {
      nodeToSplit.setL( None )
      nodeToSplit.setR( None )
    }

    nodeList
  }

  def performSwap( node : Node, nSwap : Node, nOther : Node, applyIfIncrease : Boolean ) : Seq[Node] = {

    val res = Transforms.trySwap( node, nSwap, applyIfIncrease )
    if ( res.size == 0 )
      return res

    // clean up parents of merged nodes
    nSwap.setL( None )
    nSwap.setR( None )
    nOther.setL( None )
    nOther.setR( None )

    res
  }

  def applyOperation( nodes : Set[Node], applyIfIncrease : Boolean ) : Set[Node] = {

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
      val selNode = parents.find( p => p != node && {
        ( p.getL() == node.getL() && p.getR() == node.getR() ) ||
        ( p.getL() == node.getR() && p.getR() == node.getL() )
      })
      if ( !selNode.isDefined )
        return nodes

      assert( nodes.contains( selNode.get ), "Selected node " + selNode.get + " not in map" )
      // perform it with some probability if it increases the cost
      val res = performMerge( node, selNode.get )
      if ( !res.isDefined )
        return nodes

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
      val nodeList = performSplit( nodeToSplit )
      if ( nodeList.size == 0 )
        return nodes

      return ( nodes - nodeToSplit ) ++ nodeList
    }

    // else swap
    val res = performSwap( node, nSwap, nOther, applyIfIncrease )
    if ( res.size == 0 )
      return nodes

    ( ( nodes - nSwap ) - nOther ) ++ res
  }

  def run( nodesIn : Set[Node], iter : Int ) : Set[Node] = {
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

    // look at dropping latency if possible?

    nodes
  }

  /** Run in parallel
    */
  def runPar( nodesIn : Set[Node], iter : Int, innerLoopSize : Int = 100000 ) : Set[Node] = {
    val nodes = new NodeSet( nodesIn )

    val iterDub = iter.toDouble/innerLoopSize
    val A = math.log( 0.01 )/iterDub

    var oldTime = System.currentTimeMillis()
    val myTRand = java.util.concurrent.ThreadLocalRandom.current()

    for( i <- 0 until iter/innerLoopSize ) {
      // decay the likelihood of performing an operation that makes the solution worse
      val threshold = (1 - math.exp( A*(i + 1)))/0.99
      val currTime = System.currentTimeMillis()
      println( "progress = " + (i*innerLoopSize) + "/" + iter + ", threshold = " + threshold +
        ", cost = " + nodes.size + ", time = " + (( currTime - oldTime ).toDouble/60000) + " mins")
      oldTime = currTime
      val mergeCount = new java.util.concurrent.atomic.AtomicInteger()
      val splitCount = new java.util.concurrent.atomic.AtomicInteger()
      val swapCount = new java.util.concurrent.atomic.AtomicInteger()
      println( "Start inner loop" )
      (  0 until innerLoopSize ).par.foreach( j => {

        val tmpSync = nodes.randomNode()

        val node = tmpSync._1
        val nodeLock = tmpSync._2
        val lockRes = acquireLocks( node, nodeLock )

        if ( !node.isC() && lockRes._1 ) {

          // choose operation to perform
          val applyIfIncrease = myTRand.nextDouble(1) >= threshold

          val chooseMerge = myTRand.nextInt( 0, 2 ) == 0
          val nSwap = node.getL().get
          val nOther = node.getR().get

          assert( lockRes._2.contains( nSwap ) && lockRes._2.contains( nOther ), "nSwap and nOther should be locked" )

          // perform a merge
          if ( chooseMerge && !node.parentsIsEmpty ) {
            val parents = nOther.intersectPar( nSwap.getParentSet() )
            val selNode = parents.find( p => p != node && (
              (p.getL() == node.getL() && p.getR() == node.getR()) ||
              (p.getL() == node.getR() && p.getR() == node.getL())
            ))
            if ( selNode.isDefined ) {
              // lock selected node parents too ... filter out already locked via other
              val selPar = selNode.get.getParentSet().filterNot( n => lockRes._2.contains( n ) )
              val parLocks = lockNodes( selPar, lockRes._2 )

              if ( parLocks._1 ) {
                // perform it with some probability if it increases the cost
                val res = performMerge( node, selNode.get )
                if ( res.isDefined ) {
                  assert( node.isLocked() && selNode.get.isLocked(), "Should be removing locked nodes" )
                  assert( lockRes._2.contains( node ) && lockRes._2.contains( selNode.get ), "Should hold locks" )
                  assert( node.parentsIsEmpty && selNode.get.parentsIsEmpty, "Should not be connected" )

                  nodes -= node
                  nodes -= selNode.get
                  nodes += res.get

                  res.get.unlockNode()
                }
                parLocks._2.map( n => n.unlockNode() )
                mergeCount.incrementAndGet()
              }
            }
          } else  {
            // check if have to do split instead of swap
            if ( nSwap.getParentSet.size > 1 || nOther.getParentSet.size > 1 ) {
              if ( applyIfIncrease ) {
                val nodeToSplit = {
                  if ( nSwap.getParentSet.size > 1 )
                    nSwap
                  else
                    nOther
                }
                val nodeList = performSplit( nodeToSplit )
                if ( nodeList.size > 0 ) {
                  assert( nodeToSplit.isLocked(), "Should be removing locked nodes" )
                  assert( lockRes._2.contains( nodeToSplit ), "Should hold locks" )
                  assert( nodeToSplit.parentsIsEmpty, "Should not be connected" )

                  nodes -= nodeToSplit
                  nodes ++= nodeList

                  nodeList.map( n => n.unlockNode() )
                  splitCount.incrementAndGet()
                }
              }
            } else { // else swap
              val res = performSwap( node, nSwap, nOther, applyIfIncrease )
              if ( res.size > 0 ) {
                assert( nSwap.isLocked() && nOther.isLocked(), "Should be removing locked nodes" )
                assert( lockRes._2.contains( nSwap ) && lockRes._2.contains( nOther ), "Should hold locks" )
                assert( nSwap.parentsIsEmpty && nOther.parentsIsEmpty, "Should not be connected" )

                nodes -= nSwap
                if ( nOther != nSwap )
                  nodes -= nOther
                nodes ++= res.drop(1)

                res.filterNot( _.parentsIsEmpty ).foreach( n => assert( Node.isMinimal( n ), "node " + n + " should be minimal" ) )
                res.drop(1).map( n => n.unlockNode() ) // only unlock new nodes
                swapCount.incrementAndGet()
              }
            }
          }
          lockRes._2.map( n => n.unlockNode() )
        }
      })
      println( "mergeCount = " + mergeCount.get() )
      println( "splitCount = " + splitCount.get() )
      println( "swapCount = " + swapCount.get() )
    }
    // final verification
    nodes.foreach( n => {
      assert( !n.isLocked(), "should not be locked here" )
      assert( Node.satisfiesConstraints( n ), "Node must satisfy constraints" )
      if ( n.getL().isDefined )
        assert( nodes.contains( n.getL().get ), "L must be in set" )
      if ( n.getR().isDefined )
        assert( nodes.contains( n.getL().get ), "R must be in set" )
      if ( !n.parentsIsEmpty )
        assert( Node.isMinimal( n ), "Node " + n + " should be minimal" )
      for ( p <- n.getParents() )
        assert( nodes.contains( p ), "Parents must be in set" )
    })
    nodes.toSet
  }

  /** Convert the node set to a .dot graph file
    */
  def toDot( nodes : Set[Node], fileOut : String ) : Unit = {
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

  def toChisel( nodes : Set[Node], inputs : Set[(Fixed, Node)], validIn : Bool ) : Set[(Fixed, Node)] = {
    for ( n <- inputs )
      n._2.setChisel( n._1 )
    val outNodes = nodes.filter( _.getParentSet().size == 0 ).map( n => { ( n.genChisel(), n ) })


    outNodes
  }

}
