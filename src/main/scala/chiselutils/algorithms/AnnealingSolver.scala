/** This file implements a simulated annealing solver
  */
package chiselutils.algorithms

import Chisel._
import collection.mutable.ArrayBuffer
import util.Random
import com.github.tototoshi.csv._
import java.io.File

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
          for ( p <- n.getParents() )
            p.replaceIfChild( n, newNode )
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
        // see if that node can be merged
        val parents = node.getChildren().map( _.getParentSet() ).reduce( _.intersect( _ ) )
        val selNode = parents.find( p => p != node && ( p.getChildren() == node.getChildren() ))
        if ( selNode.isDefined ) {
          // perform it with some probability if it increases the cost
          val res = performMerge( node, selNode.get )
          if ( res.isDefined ) {
            newNodes -= node
            newNodes -= selNode.get
            newNodes += res.get
            mergedSet += res.get
            mergedSet -= selNode.get
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
      val newNode = Node( currNode.getUkPrev, currNode.getCkPrev )
      currNode.addChild( newNode )
      currNode.addChild( newNode )
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
    val newLcK = currNode.getCkPrev // same as is an add
    val newLNode = Node( newLuK, newLcK )
    val newRuK = Node.ukPrev( Vector( elementList.zipWithIndex.filterNot( e => combOp.contains( e._2 ) ).map( _._1 ).toSet ) )
    val newRcK = currNode.getCkPrev // same as is an add
    val newRNode = Node( newRuK, newRcK )
    currNode.addChild( newLNode )
    currNode.addChild( newRNode )
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
      val newNode = Node( currNode.getUkPrev, currNode.getCkPrev )
      currNode.addChild( newNode )
      currNode.addChild( newNode )
      currNode.setB()
      allNodes += newNode
      currNode = newNode
    }

    if ( n.uk.size == 2 ) {
      val nA = Node( Vector( currNode.getUkPrev.head ), currNode.getCkPrev.map( ck => if ( ck == 0 ) 0 else -1 ) )
      val nB = Node( Vector( currNode.getUkPrev.last ), currNode.getCkPrev.map( ck => if ( ck == 1 ) 0 else -1 ) )
      currNode.addChild( nA )
      currNode.addChild( nB )
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
    val newLuK = currNode.getUkPrev.zipWithIndex.filter( e => combOp.contains( e._2 ) ).map( _._1 )
    val newLcK = currNode.getCkPrev.map( ck => combOp.indexOf( ck ) )
    val newLNode = Node( newLuK, newLcK )
    val newRuK = currNode.getUkPrev.zipWithIndex.filterNot( e => combOp.contains( e._2 ) ).map( _._1 )
    val newRcK = currNode.getCkPrev.map( ck => combNot.indexOf( ck ) )
    val newRNode = Node( newRuK, newRcK )
    currNode.addChild( newLNode )
    currNode.addChild( newRNode )
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
    -cpRemaining.min // TODO: fix error when cpRemaining is empty
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
          for ( n <- nodesLocked )
            n.unlockNode()
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

    val nodesLocked = collection.mutable.Set[Node]()
    nodesLocked += node

    val nodeChildren = node.getChildren()

    for ( nC <- nodeChildren ) {
      if ( !nodesLocked.contains( nC ) ) {
        val lockCRes = nC.lockNode()
        if ( !lockCRes ) {
          for ( n <- nodesLocked )
            n.unlockNode()
          return (false, Set[Node]())
        }
        nodesLocked += nC
      }
    }

    for ( nIn <- nodeChildren.map( nC => nC.getChildren() ).reduce( _ ++ _ ) ) {
      if ( !nodesLocked.contains(nIn) ) {
        val lock = nIn.lockNode()
        if ( !lock ) {
          for ( n <- nodesLocked )
            n.unlockNode()
          return (false, Set[Node]())
        }
        nodesLocked += nIn
      }
    }

    // lock parents of node, nodeL and nodeR
    val lockPar = lockNodes( node.getParentSet() ++ nodeChildren.map( _.getParentSet() ).reduce( _ ++ _ ), nodesLocked.toSet )
    if ( !lockPar._1 ) {
      for ( n <- nodesLocked )
        n.unlockNode()
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
    nA.removeChildren()
    nB.removeChildren()

    res
  }

  def performSplit( nodeToSplit : Node ) : Seq[Node] = {

    val nodeList = Transforms.trySplit( nodeToSplit )

    // add new nodes and remove old one
    if ( nodeList.size > 0 )
      nodeToSplit.removeChildren()

    nodeList
  }

  def performSwap( node : Node, applyIfIncrease : Boolean ) : (Seq[Node], Int) = {
    Transforms.trySwap( node, applyIfIncrease )
  }

  /** Run in parallel
    */
  def runPar( nodesIn : Set[Node], iter : Int, innerLoopSize : Int = 100000, safeMode : Boolean = false ) : Set[Node] = {
    val nodes = new NodeSet( nodesIn )

    val iterDub = iter.toDouble
    val A = math.log( 0.01 )/iterDub

    var oldTime = System.currentTimeMillis()
    val myTRand = java.util.concurrent.ThreadLocalRandom.current()

    for( i <- 0 until iter ) {
      // decay the likelihood of performing an operation that makes the solution worse
      val threshold = (1 - math.exp( A*(i + 1)))/0.99
      val currTime = System.currentTimeMillis()
      println( "progress = " + i + "/" + iter + ", threshold = " + threshold +
        ", cost = " + nodes.size + ", time = " + (( currTime - oldTime ).toDouble/60000) + " mins")
      oldTime = currTime
      val mergeCount = new java.util.concurrent.atomic.AtomicInteger()
      val splitCount = new java.util.concurrent.atomic.AtomicInteger()
      val swapCount = Vector.fill( 23 ) ( new java.util.concurrent.atomic.AtomicInteger() )
      if ( safeMode ) {
        // do expensive checks
        for ( n <- nodes ) {
          for ( nc <- n.getChildren() ++ n.getParents() )
            assert( nodes.contains( nc ) )
          assert( !n.isLocked() )
          assert( Node.verifyNode( n ) )
        }
      }
      println( "Start inner loop" )
      (  0 until innerLoopSize ).par.foreach( j => {
      // (  0 until innerLoopSize ).foreach( j => {

        val tmpSync = nodes.randomNode()

        val node = tmpSync._1
        val nodeLock = tmpSync._2
        val lockRes = acquireLocks( node, nodeLock )

        if ( !node.isC() && lockRes._1 ) {

          // choose operation to perform
          val applyIfIncrease = myTRand.nextDouble(1) >= threshold

          val chooseMerge = myTRand.nextInt( 0, 2 ) == 0

          val nonLocked = node.getChildren().find( n => !lockRes._2.contains( n ) )
          assert( !nonLocked.isDefined, "child nodes should be locked: " + nonLocked )

          // perform a merge
          if ( chooseMerge && !node.parentsIsEmpty ) {
            val parents = node.getChildren().map( n => n.getParentSet() ).reduce( _ intersect _ )
            val parChild = parents.map( _.getChildren ).reduce( _ ++ _ )
            val parChildLocks = lockNodes( parChild, lockRes._2 )
            if ( parChildLocks._1 ) {
              val selNode = parents.find( p => {
                val nC = node.getChildren
                val pC = p.getChildren
                p != node && {
                  ( node.isA() && p.isA() && pC == nC ) ||
                  ( node.isB() && p.isB() && ( pC.subsetOf( nC ) || nC.subsetOf( pC ) ) )
                }
              })
              if ( selNode.isDefined ) {
                // lock selected node parents too ... filter out already locked via other
                val selPar = selNode.get.getParentSet()
                val parLocks = lockNodes( selPar, lockRes._2 ++ parChildLocks._2 )

                if ( parLocks._1 ) {
                  // perform it with some probability if it increases the cost
                  if ( safeMode ) {
                    for ( n <- List( node, selNode.get ) ++ node.getChildren ++ selNode.get.getChildren )
                      assert( Node.satisfiesConstraints( n ), "Node " + n + " should satisfy constraints " )
                  }
                  val nodeChildren = node.getChildren()
                  val selChildren = selNode.get.getChildren()
                  val nodeParents = node.getParents()
                  val selParents = selNode.get.getParents()
                  val res = performMerge( node, selNode.get )
                  if ( res.isDefined ) {
                    assert( node.isLocked() && selNode.get.isLocked(), "Should be removing locked nodes" )
                    assert( lockRes._2.contains( node ) && lockRes._2.contains( selNode.get ), "Should hold locks" )
                    assert( node.parentsIsEmpty && selNode.get.parentsIsEmpty, "Should not be connected" )

                    nodes -= node
                    nodes -= selNode.get
                    nodes += res.get

                    if ( safeMode ) {
                      for ( n <- List( res.get ) ++ res.get.getChildren() ) {
                        assert( Node.satisfiesConstraints( n ), "Node " + n + " should satisfy constraints after merge of " +
                          node + " and " + selNode.get + " has parents " + n.getParentSet() +
                          " node children " + nodeChildren + " node parents " + nodeParents +
                          " sel children " + selChildren + " sel parents " + selParents)
                        assert( Node.isMinimal( n ),
                          "node " + n + " should be minimal after merge of " + node + " and " + selNode.get +
                            " has parents " + n.getParentSet() +
                            " node children " + nodeChildren + " node parents " + nodeParents +
                            " sel children " + selChildren + " sel parents " + selParents
                        )
                      }
                    }

                    res.get.unlockNode()
                    mergeCount.incrementAndGet()
                  }
                  for ( n <- parLocks._2 )
                    n.unlockNode()
                }
              }
            }
            for ( n <- parChildLocks._2 )
              n.unlockNode()
          } else  {
            // check if have to do split instead of swap
            val nodeToSplit = node.getChildren().find( n => n.getParentSet.size > 1 )
            if ( nodeToSplit.isDefined ) {
              if ( applyIfIncrease ) {
                val oldParents = nodeToSplit.get.getParentSet
                val oldChildren = nodeToSplit.get.getChildren
                if ( safeMode ) {
                  assert( Node.isMinimal( nodeToSplit.get ) )
                  assert( Node.satisfiesConstraints( oldParents ++ oldChildren ++ nodeToSplit ) )
                }
                val nodeList = performSplit( nodeToSplit.get )
                if ( safeMode )
                  assert( Node.satisfiesConstraints( oldParents ++ oldChildren ) )
                if ( nodeList.size > 0 ) {
                  assert( nodeToSplit.get.isLocked(), "Should be removing locked nodes" )
                  assert( lockRes._2.contains( nodeToSplit.get ), "Should hold locks" )
                  assert( nodeToSplit.get.parentsIsEmpty, "Should not be connected" )

                  nodes -= nodeToSplit.get
                  nodes ++= nodeList

                  if ( safeMode ) {
                    for( n <- nodeList ++ nodeList.map( _.getChildren() ).reduce( _ ++ _ ) ++ nodeList.map( _.getParents ).reduce( _ ++ _ ) ) {
                      assert( Node.satisfiesConstraints( n ) )
                      assert( Node.isMinimal( n ),
                        "node " + n + " should be minimal after split of " + nodeToSplit.get + " has parents " + n.getParentSet() +
                          " old children " + oldChildren + " old parents " + oldParents )
                    }
                  }

                  for ( n <- nodeList )
                    n.unlockNode()
                  splitCount.incrementAndGet()
                }
              }
            } else { // else swap
              val nodeChildren = node.getChildren()
              val grandChildren = nodeChildren.map( _.getChildren() )
              if ( safeMode ) {
                for ( n <- nodeChildren )
                  assert( Node.isMinimal( n ), "node " + n + " should be minimal and has parents " + n.getParentSet() +
                    " old children " + nodeChildren + " grandchildren " + grandChildren )
                if ( !node.parentsIsEmpty )
                  assert( Node.isMinimal( node ),
                    "Node " + node + " should be minimal and has parents " + node.getParentSet() )
              }
              val res = performSwap( node, applyIfIncrease )
              if ( res._1.size > 0 ) {
                val oldNodes = nodeChildren -- res._1
                for ( n <- oldNodes ) {
                  assert( n.isLocked(), "Should be removing locked nodes" )
                  assert( lockRes._2.contains( n ), "Should hold locks" )
                  assert( n.parentsIsEmpty , "Should not be connected" )
                  nodes -= n
                }

                val newNodes = res._1.drop(1).toSet -- nodeChildren
                nodes ++= newNodes

                if ( safeMode ) {
                  for ( n <- res._1 ) {
                    assert( Node.satisfiesConstraints( n ), "Node " + n + " does not satisfy constraints after swap " + res._2 +
                      " has parents " + n.getParentSet() + " old children " + nodeChildren + " grandchildren " + grandChildren )
                    assert( Node.isMinimal( n ),
                      "node " + n + " should be minimal after swap " + res._2 +
                        " has parents " + n.getParentSet() + " old children " + nodeChildren + " grandchildren " + grandChildren )
                  }
                  if ( !node.parentsIsEmpty )
                    assert( Node.isMinimal( node ),
                      "Node " + node + " should be minimal after swap " + res._2 + " has parents " + node.getParentSet() )
                }
                for ( n <- newNodes )
                  n.unlockNode() // only unlock new nodes
                swapCount( res._2 - 1 ).incrementAndGet()
              }
            }
          }
          lockRes._2.map( n => n.unlockNode() )
        }
      })
      println( "mergeCount = " + mergeCount.get() )
      println( "splitCount = " + splitCount.get() )
      for ( i <- 0 until swapCount.size )
        println( "swapCount( " + (i + 1 ) + " )  = " + swapCount(i).get() )
    }
    // final verification
    for ( n <- nodes ) {
      assert( !n.isLocked(), "should not be locked here" )
      assert( Node.satisfiesConstraints( n ), "Node must satisfy constraints" )
      for ( nc <- n.getChildren() )
        assert( nodes.contains( nc ), "Children must be in set" )
      if ( !n.parentsIsEmpty )
        assert( Node.isMinimal( n ), "Node " + n + " should be minimal" )
      for ( p <- n.getParents() )
        assert( nodes.contains( p ), "Parents must be in set" )
    }
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
      for ( nc <- n.getChildren() ) {
        val lValid = nodes.contains( nc ) && nc.hasParent( n )
        if ( !lValid ) {
          println( "n = " + n )
          println( "n Child = " + nc )
          println( "n Child getParents() = " + nc.getParents() )
        }
        assert( lValid, "Can only reference nodes inside map" )
        bw.write( "N" + n.hashCode.abs + " -> N" + nc.hashCode.abs + ";\n" )
      }
    }
    bw.write("}\n")
    bw.close()
    file.renameTo( new java.io.File( fileOut ) )
  }

  def toChisel( nodes : Set[Node], inputs : Set[(Fixed, Node)], validIn : Bool ) : Bool = {
    for ( n <- inputs )
      n._2.setChisel( n._1 )
    val outNodes = nodes.filter( _.parentsIsEmpty() ).map( n => { ( n.genChisel(), n ) })
    val latency = outNodes.map( n => Node.latency( n._2 ) ).max
    val validRegs = Vector( validIn ) ++ Vector.fill( latency ) { RegInit( Bool(false) ) }
    for ( i <- 1 until validRegs.size )
      validRegs(i) := validRegs(i - 1)

    validRegs.last
  }

  private def nodeToStr( nodesVec : Seq[Node], n : Node ) : Seq[String] = {
    List(n.toString()) ++ n.getChildren().map( nc => nodesVec.indexOf( nc ).toString() ).toList
  }

  private def nodeFromStr( s : String ) : Node = {
    val typeReg = "[ABC]".r
    val typeSearch = typeReg.findFirstIn( s )
    assert( typeSearch.isDefined, "Must be either type A, B or C" )
    val nodeType = typeSearch.get
    val curlyReg = """((?<=\{)[^}]*)""".r
    val ukCk = curlyReg.findAllIn( s ).toList
    assert( ukCk.size == 2 )
    val uk = ukCk(0)
    val ck = ukCk(1)
    val ukNums = uk.split( """Set\(""" ).drop(1).map( s => // select each set
      s.split("""Vector\(""").map( v => // select each vector in the set
        """\d+""".r.findAllIn(v).toVector.to[Seq].map( d => d.toInt ) // convert each number in the vector
      ).filterNot( _.isEmpty ).toSet // convert to Set
    ).toVector
    val ckNums = """[-]*\d""".r.findAllIn( ukCk(1) ).toVector.to[Seq].map( _.toInt )
    val n = Node( ukNums, ckNums )
    if ( nodeType == "A" )
      n.setA()
    else if ( nodeType == "B" )
      n.setB()
    else
      n.setC()
    n
  }

  def binFilterToCp( convFilter : Seq[Seq[Seq[Seq[Int]]]], imgSize : (Int, Int), throughput : Int = 1 ) : Seq[Seq[Set[Seq[Int]]]] = {

    val filterSize = ( convFilter(0)(0).size, convFilter(0)(0)(0).size, convFilter(0).size )

    val cp = ( 0 until convFilter.size ).map( convIdx => { // for each filter
      val imgOut = ( 0 until imgSize._2 ).map( y => { // for each column in the img
        ( 0 until imgSize._1 ).map( x => { // for each row in that column
          ( 0 until filterSize._1 ).map( px => {
            ( 0 until filterSize._2 ).map( py => {
              ( 0 until filterSize._3 ).map( d => {
                val xpx = x + px - (filterSize._1/2)
                val ypy = y + py - (filterSize._2/2)
                ( xpx, ypy, px, py, d )
              }).filter{ case ( xpx, ypy, px, py, d ) => { // filter out zeros and any edge parts
                val isZero = ( convFilter( convIdx )(d)(py)(px) == 0 )
                ( xpx >= 0 && xpx < imgSize._1 && ypy >= 0 && ypy < imgSize._2 && !isZero )
              }}.map{ case ( xpx, ypy, px, py, d ) => {
                val addIdx = if ( convFilter( convIdx )(d)(py)(px) == 1 ) 1 else 0
                val cyc = ( ypy*imgSize._2 + xpx )/throughput
                val pos = (2*d) + addIdx + 2*filterSize._3*( ( ypy*imgSize._2 + xpx ) % throughput )
                Vector( cyc, pos )
              }}.toSet
            }).reduce( _ ++ _ ) // sum over filter cols
          }).reduce( _ ++ _ ) // sum over filter rows
        })
      }).reduce( _ ++ _ ) // collect image into a list
      imgOut.zipWithIndex.groupBy( _._2 % throughput ).toVector.sortBy( _._1 ).map( _._2 ).map( v => v.map( s => s._1 ) )
    }).reduce( _ ++ _ ).filter( cList => cList.find( !_.isEmpty ).isDefined ) // collect all outputs

    val cpCoords = cp.map( convFilt => {
      convFilt.zipWithIndex.map( cSet => {
        cSet._1.map( v => {Vector( cSet._2 - v(0)) ++ v.drop(1)}.to[Seq] )
      }).toVector.to[Seq]
    }).toVector.to[Seq]
    val latAdd = AnnealingSolver.needLatency( cpCoords )
    println( "latAdd = " + latAdd )
    val cpFinal = cpCoords.map( convFilt => {
      convFilt.map( cSet => {
        cSet.map( v => { Vector( latAdd + v(0) ) ++ v.drop(1) }.to[Seq])
      })
    })
    cpFinal
  }

  def readCsv( filename : String ) : Seq[Seq[Seq[Seq[Int]]]] = {
    val reader = CSVReader.open(new File(filename))
    val floatData = reader.all().map( x => x.map( _.toFloat.toInt ).to[Seq] )
    reader.close()
    // group into the convolutional filters
    floatData.toVector.to[Seq].grouped(3).toVector.to[Seq].grouped(3).toVector.to[Seq]
  }

  def save( nodes : Set[Node], filename : String ) : Boolean = {
    val writer = CSVWriter.open( new File( filename ) )
    val nodesVec = nodes.toVector
    for ( n <- nodesVec )
      writer.writeRow( nodeToStr( nodesVec, n ) )
    writer.close
    true
  }

  def load( filename : String ) : Set[Node] = {
    val reader = CSVReader.open( new File( filename ) )
    val nodeEntries = reader.iterator.map( ln => {
      ( nodeFromStr( ln.head ), ln.tail.map( _.toInt ) )
    }).toVector
    reader.close
    for ( nodeConn <- nodeEntries ) {
      for ( cIdx <- nodeConn._2 )
        nodeConn._1.addChild( nodeEntries( cIdx )._1 )
    }
    nodeEntries.map( _._1 ).toSet
  }
}
