
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

object SumScheduler {

  /** find if sub is a cycle shifted subset of the masterSet
    * return ( idx of shift, is subset )
    */
  def isSubsetCycShift( masterSet : Set[(Int, Int)], sub : Set[(Int, Int)] ) : (Int, Boolean) = {
    val subMin = sub.minBy( _._1 )._1
    // loop through masterSet cycles
    for ( msCyc <- masterSet.map( _._1 - subMin ) ) {
      if ( sub.map( x => { ( x._1 + msCyc , x._2 ) }).subsetOf(masterSet) )
        return ( msCyc, true )
    }
    return ( -1, false )
  }

  /** cycle shift sums and only keep the unique ones
    */
  def filterUniqueSums( sumStructure : List[Set[(Int, Int)]] ) : List[Set[(Int, Int)]] = {
    val uniqueSums = ArrayBuffer[Set[(Int, Int)]]()
    uniqueSums += sumStructure(0)
    for ( sumSet <- sumStructure ) {
      val isSubset = uniqueSums.map( uS => {
        isSubsetCycShift( uS, sumSet )._2
      }).reduce( _ || _ )
      if ( !isSubset )
        uniqueSums += sumSet
    }
    uniqueSums.toList
  }

  /** check if op is a mux */
  def isMux( thisOp : (Int, Int, Int, Int, Boolean) ) = {
    !thisOp._5 && ( thisOp._1, thisOp._2 ) != ( thisOp._3, thisOp._4 )
  }

  /** check if op is an add */
  def isAdd( thisOp : (Int, Int, Int, Int, Boolean) ) = {
    thisOp._5
  }

  /** check if op is a delay */
  def isDelay( thisOp : (Int, Int, Int, Int, Boolean) ) = {
    !thisOp._5 && ( thisOp._1, thisOp._2 ) == ( thisOp._3, thisOp._4 )
  }

  /** delay the cp stage using stage idxs */
  def cpStageDelay( currStage : Int, stageFrom : Int, cp : Set[(Int, Int)] ) : Set[(Int, Int)] = {
    cp.map( c => { ( c._1 + currStage - stageFrom, c._2 ) } )
  }

  /** Get the min number of cycles needed to realize the set of CP */
  def cpToCycRequired( cp : Set[(Int, Int)] ) : Int = {
    val cpCycMap = cp.groupBy( _._1 )
    val cpCycKeys = cpCycMap.keys.toList.sortBy( x => x )
    val cpMin = cp.minBy( _._1 )._1
    // extra cycles needed for adder tree if multiple on same cycle
    val cpCycs = cpCycMap.map( x => ( x._1, x._2.size ) )
    // calculate cumulative max cycle
    var currCyc = 0
    var cpCycMax = 0
    for( kIdx <- 0 until cpCycKeys.size ) {
      val key = cpCycKeys(kIdx)
      currCyc = {
        if ( kIdx == 0 )
          0
        else {
          val keyDiff = key - cpCycKeys(kIdx - 1)
          val rCyc = {
            if ( currCyc < 1 )
              0
            else
              log2Ceil( currCyc ) - keyDiff
          }
          val remainingAdd = { // add from prev cycles
            if ( rCyc > 0 )
              1 << rCyc
            else
              1
          }
          cpCycs( key ) + remainingAdd
        }
      }
      val thisKeyCyc = key + {
        if ( currCyc <= 1 )
          0
        else
          log2Ceil( currCyc ) - 1
      }
      if ( thisKeyCyc > cpCycMax )
        cpCycMax = thisKeyCyc
    }
    cpCycMax - cpMin
  }

  /** query currentCps to get cp set */
  def getCp( stage : Int, pos : Int, currentCps : List[List[Set[Set[(Int, Int)]]]] ) : Set[Set[(Int, Int)]] = {
    if ( stage > 0 )
      currentCps( stage - 1 )( pos ).map( cpSet => cpSet.map( cp => {
        ( cp._1 + currentCps.size - stage, cp._2 )
      }) )
      else
        Set( Set( ( currentCps.size, pos ) ) )
  }

  /** Get the cp set from parents
    */
  def getCpFromParents( thisOp : (Int, Int, Int, Int, Boolean), currentCps : List[List[Set[Set[(Int, Int)]]]] ) : Set[Set[(Int, Int)]] = {
    val parent1 = getCp( thisOp._1, thisOp._2, currentCps )

    if ( isDelay( thisOp ) )
      return parent1

    val parent2 = getCp( thisOp._3, thisOp._4, currentCps )

    if ( isAdd(thisOp) ) {
      // all combinations of sets
      parent1.map( p1 => {
        parent2.map( p2 => {
          p1 ++ p2
        })
      }).reduce(_ ++ _)
    } else
      parent1 ++ parent2
  }

  /** Convert a path to all possible CP coordinates for a particular output
    */
  def pathToCP( path : List[List[(Int, Int, Int, Int, Boolean)]], rootIdx : Int ) : List[Set[(Int, Int)]] = {
    val thisOp = path.last( rootIdx )

    if ( thisOp._1 == 0 && thisOp._3 == 0 ) {
      if ( !thisOp._5 ) {
        if ( thisOp._2 == thisOp._4  )
          return List( Set( ( path.size - 1, thisOp._2 ) ) ) // delay
        return List( Set( ( path.size - 1, thisOp._2 ) ), Set( ( path.size - 1, thisOp._4 ) ) ) // mux
      } else
        return List( Set( ( path.size - 1, thisOp._2 ), ( path.size - 1, thisOp._4 ) ) ) // add
    } else if ( thisOp._1 == 0 ) {
      val cycSkip = path.size - thisOp._3
      val pathBSets = pathToCP( path.dropRight( cycSkip ), thisOp._4 ).map( s => s.map( x => { ( x._1 + cycSkip - 1, x._2 ) } ) )
      if ( !thisOp._5 )
        return pathBSets ++ List( Set( ( path.size - 1, thisOp._2 ) ) ) // mux
      return pathBSets.map( p => p ++ Set( ( path.size - 1, thisOp._2 ) ) ) // add
    } else if ( thisOp._3 == 0 ) {
      val cycSkip = path.size - thisOp._1
      val pathBSets = pathToCP( path.dropRight( cycSkip ), thisOp._2 ).map( s => s.map( x => { ( x._1 + cycSkip - 1, x._2 ) } ) )
      if ( !thisOp._5 )
        return pathBSets ++ List( Set( ( path.size - 1, thisOp._4 ) ) ) // mux
      return pathBSets.map( p => p ++ Set( ( path.size - 1, thisOp._4 ) ) ) // add
    } else {
      if ( ( thisOp._1, thisOp._2 ) != ( thisOp._3, thisOp._4 ) ) {
        val pathACycSkip = path.size - thisOp._1
        val pathBCycSkip = path.size - thisOp._3
        val pathASets = pathToCP( path.dropRight( pathACycSkip ), thisOp._2 ).map( s => s.map( x => { ( x._1 + pathACycSkip - 1, x._2 ) } ) )
        val pathBSets = pathToCP( path.dropRight( pathBCycSkip ), thisOp._4 ).map( s => s.map( x => { ( x._1 + pathBCycSkip - 1, x._2 ) } ) )
        // generate all combinations
        if ( thisOp._5 ) {
          // is add
          return pathASets.map( paS => {
            pathBSets.map( pbS => {
              paS ++ pbS
            })
          }).reduce( _ ++ _ )
        } else {
          // mux just selects between
          return pathASets ++ pathBSets
        }
      } else {
        assert( !thisOp._5 , "Only delay should have same op input" )
        val cycSkip = path.size - thisOp._3
        return pathToCP( path.dropRight( cycSkip ), thisOp._4 ).map( s => s.map( x => { ( x._1 + cycSkip - 1, x._2 ) } ) )
      }
    }
  }

  /** validate a path from input to node and see if in use by any sum
    * Note can remove cyc shifted sums to reduce checking time
    */
  def validatePath( path : List[List[(Int, Int, Int, Int, Boolean)]],
    uniqueSums : List[Set[(Int, Int)]], requireEqual : Boolean  = false) : Boolean = {
    val thisOp = path.last(0)
    val pathsToCheck = {
      // check if mux
      if ( !thisOp._5 && ( thisOp._1, thisOp._2 ) != ( thisOp._3, thisOp._4 ) ) {
        val pathA = {
          if ( thisOp._1 == 0 )
            List( Set( ( path.size - 1, thisOp._2 ) ) )
          else {
            val cycSkip = path.size - thisOp._1
            pathToCP( path.dropRight( cycSkip ), thisOp._2 ).map( s => s.map( x => { ( x._1 + cycSkip - 1, x._2 ) } ) )
          }
        }
        val pathB = {
          if ( thisOp._3 == 0 )
            List( Set( ( path.size - 1, thisOp._4 ) ) )
          else {
            val cycSkip = path.size - thisOp._3
            pathToCP( path.dropRight( cycSkip ), thisOp._4 ).map( s => s.map( x => { ( x._1 + cycSkip - 1, x._2 ) } ) )
          }
        }
        List( pathA, pathB )
      } else
        List(pathToCP( path, 0 ))
    }
    pathsToCheck.map( cpList => {
      // check if is subset of unique sums
      cpList.map( cp => {
        uniqueSums.map( us => {
          if ( requireEqual )
            isSubsetCycShift( us, cp )._2 && ( us.size == cp.size )
          else
            isSubsetCycShift( us, cp )._2
        }).reduce( _ || _ )
      }).reduce( _ || _ )
    }).reduce( _ && _ ) // for muxes, ensure that both paths are useful
  }

  /** Get all useful CP sets for these particular sums as a given cycle
    */
  def partitionAtCycle( uniqueSums : List[Set[(Int, Int)]], cycleToPartition : Int ) : List[Set[(Int, Int)]] = {
    val partitionSet = HashSet[Set[(Int, Int)]]()
    for ( us <- uniqueSums ) {
      val shiftedSubsets = us.subsets.filter( sub => ( sub.size > 0 && cpToCycRequired( sub ) <= cycleToPartition ) ).map( sub => {
        // shift all to start at 0 so not duplicated
        val minCyc = sub.minBy( _._1 )._1
        sub.map( x => ( x._1 - minCyc, x._2 ) )
      })
      for ( ss <- shiftedSubsets )
        partitionSet += ss
    }
    partitionSet.toList
  }

  /** Similar to partitionAtCycle, but get all without shifting cycle
    * These are the constraints for partitionAtCycle outputs
    * Attach muxes to all output sets they contain
    * Need to constrain on the cycle expected that they are swtiched correctly
    */
  def generateCpForSums( sums : List[Set[(Int, Int)]], depthToPartition : Int ) : Map[Set[(Int, Int)], Set[Int]] = {
    val partitionMap = collection.mutable.Map[Set[(Int, Int)], Set[Int]]()
    for ( us <- sums ) {
      val shiftedSubsets = us.subsets.filter( sub => {
        sub.size > 0 && cpToCycRequired( sub ) <= depthToPartition
      }).map( sub => {
        val minCyc = sub.minBy( _._1 )._1
        ( sub.map( x => ( x._1 - minCyc, x._2 ) ), minCyc )
      })
      for ( ss <- shiftedSubsets ) {
        val newCycSet = partitionMap.getOrElse( ss._1, Set[Int]() ) ++ Set( ss._2 )
        partitionMap.put( ss._1, newCycSet )
      }
    }
    partitionMap.toMap
  }

  def getBinaryAndConstraints( x1 : MPIntVar, x2 : Expression )(
    implicit problem : MIProblem ) : ( List[Constraint], MPIntVar ) = {
    val y1 = MPIntVar("y", 0 to 1 )
    val constList : List[Constraint] = List( ( y1 >= x1 + x2 - One ), ( y1 <= x1 ), (y1 <= x2 ) )
    ( constList, y1 )
  }

  def getMuxConstraint( muxActive : MPIntVar, muxCyc : MPIntVar, require1 : Boolean ) : Constraint = {
    if ( require1 )
      muxActive - muxCyc <= 0
    else
      muxActive + muxCyc <= 1
  }

  /** Get the parent sets
    * for the set output of thisOp find all parent set index combos
    * returns List[( parSet1Idx, parSet2Idx ) ]
    */
  def getParentSetIdx( currStage : Int, thisOp : (Int, Int, Int, Int, Boolean), cpSet : Set[(Int, Int)],
    allCps : List[List[List[Set[(Int, Int)]]]] ) : List[(Int, Int)] = {
    val setsA = {
      if ( thisOp._1 == 0 ) List( Set( ( currStage - 1, thisOp._2 ) ) )
      else allCps( thisOp._1 - 1 )( thisOp._2 )
    }
    val setsB = {
      if ( thisOp._3 == 0 ) List( Set( ( currStage - 1, thisOp._4 ) ) )
      else allCps( thisOp._3 - 1 )( thisOp._4 )
    }

    if ( isAdd( thisOp ) ) {
      if ( setsA.size == 1 ) {
        val branchSet = cpSet -- setsA(0)
        val selSets = setsB.zipWithIndex.filter( s => branchSet.subsetOf(s._1) )
        assert( selSets.size >= 1, "selSets should have size of atleast 1" )
        selSets.map( s => ( 0, s._2 ) )
      } else if ( setsB.size == 1 ) {
        val branchSet = cpSet -- setsB(0)
        val selSets = setsA.zipWithIndex.filter( s => branchSet.subsetOf(s._1) )
        assert( selSets.size >= 1, "selSets should have size of atleast 1" )
        selSets.map( s => ( s._2, 0 ) )
      } else {
        ( 0 until setsA.size ).map( saIdx => {
          ( 0 until setsB.size ).map( sbIdx => {
            val combSet = setsA( saIdx ) ++ setsB( sbIdx )
            ( combSet.size == cpSet.size && combSet.subsetOf(cpSet), saIdx, sbIdx )
          }).toList.filter( x => x._1 ).map( x => ( x._2, x._3 ) )
        }).reduce( _ ++ _ )
      }
    } else if ( isDelay( thisOp ) ) {
      setsA.zipWithIndex.filter( s => {
        cpSet.size == s._1.size && s._1.subsetOf( cpSet )
      }).map( x => ( x._2, -1 ) )
    } else {
      val setAComb = setsA.zipWithIndex.filter( s => {
        s._1.size == cpSet.size && cpSet.subsetOf(s._1)
      }).map( s => ( s._2, -1 ) )
      val setBComb = setsB.zipWithIndex.filter( s => {
        s._1.size == cpSet.size && cpSet.subsetOf(s._1)
      }).map( s => ( -1, s._2 ) )
      setAComb ++ setBComb
    }
  }

  /** recursively generate mux cycle constraints
    */
  def createCycleConstraints( nodePathBack : MPIntVar, stageIdx : Int, posIdx : Int, setIdx : Int,
    nodeEnables : List[List[List[MPIntVar]]], allCps : List[List[List[Set[(Int, Int)]]]], cycle : Int,
    currentPath : List[List[(Int, Int, Int, Int, Boolean)]], tmpNodeName : String ) (
    implicit problem : MIProblem ) : List[Constraint] = {

    val thisOp = currentPath( stageIdx )( posIdx )
    val cpSet = allCps( stageIdx ) ( posIdx ) ( setIdx )
    val constraintList = ArrayBuffer[Constraint]()
    // get the idxs of parent sets
    val parentSets = getParentSetIdx( stageIdx + 1, thisOp, cpSet, allCps )

    // println( "Constraining cycle = " + cycle + " stage = " + (stageIdx + 1) + " pos = " + posIdx + " op = " + thisOp )
    // println( "ParentSets = " + parentSets )

    if ( isMux( thisOp ) ) {
      // add constraint for this op
      val allLeft = parentSets.map( _._2 == -1 ).reduce( _ && _ )
      val allRight = parentSets.map( _._1 == -1 ).reduce( _ && _ )
      if ( allLeft || allRight ) {
        val mux = nodeEnables( stageIdx ) ( posIdx )
        val binAnd = getBinaryAndConstraints( nodePathBack, mux(0) )
        constraintList ++= binAnd._1
        constraintList += getMuxConstraint( binAnd._2, mux(cycle + 1), allRight )
      }
    }
    // set all parent constraints if more muxes above
    val thisNode = nodeEnables( stageIdx )( posIdx )
    // if path to output is active AND this node is active AND it is active on this cycle
    val binAnd = getBinaryAndConstraints( nodePathBack, thisNode( 0 ) )
    constraintList ++= binAnd._1
    val nodePathBackLeft = {
      if ( isMux( thisOp ) ) {
        val muxCycAnd = getBinaryAndConstraints( binAnd._2, ( One - thisNode( cycle ) ) )
        constraintList ++= muxCycAnd._1
        muxCycAnd._2
      } else
        binAnd._2
    }
    val nodePathBackRight = {
      if ( isMux( thisOp ) ) {
        val muxCycAnd = getBinaryAndConstraints( binAnd._2, thisNode( cycle ) )
        constraintList ++= muxCycAnd._1
        muxCycAnd._2
      } else
        binAnd._2
    }
    for ( ps <- parentSets ) {
      // if there are more muxes on left
      if ( thisOp._1 != 0 && allCps( thisOp._1 - 1 )( thisOp._2 ).size > 1 ) {
        // there are more muxes on left path so create cycle constraints on all of them
        if ( ps._1 != -1 ) {
          constraintList ++= createCycleConstraints( nodePathBackLeft, thisOp._1 - 1,
            thisOp._2, ps._1, nodeEnables, allCps, cycle - 1, currentPath, tmpNodeName + "_L" + ps._1 )
        }
      }
      // if there are more muxes on right
      if ( thisOp._3 != 0 && allCps( thisOp._3 - 1 )( thisOp._4 ).size > 1 ) {
        if ( ps._2 != -1 ) {
          constraintList ++= createCycleConstraints( nodePathBackRight, thisOp._3 - 1,
            thisOp._4, ps._2, nodeEnables, allCps, cycle - 1, currentPath, tmpNodeName + "_R" + ps._2 )
        }
      }
    }
    constraintList.toList
  }

  def runILP( currentPath : List[List[(Int, Int, Int, Int, Boolean)]], currentCps : List[List[Set[Set[(Int, Int)]]]],
    noCycles : Int, validOuts : Map[Set[(Int, Int)], Set[Int]], solverName : String = "gurobi"
  ) : ( List[List[(Int, Int, Int, Int, Boolean)]], Map[(Int, Int), List[Boolean]] ) = {

    val allCps = currentCps.map( cpStage => cpStage.map( _.toList ) )
    val outCps = allCps.last
    val outputs = validOuts.keys.toList
    val maxCycles = validOuts.map( x =>  x._2.max ).max + currentPath.size

    println( "currentPath = " )
    currentPath.foreach(  x => println(x.zipWithIndex) )
    println( "validOuts = " + validOuts )
    println( "currentCps = " )
    currentCps.foreach( x => println(x.zipWithIndex) )
    println( "maxCycles = " + maxCycles )
    println( "noCycles = " + noCycles )
    for ( i <- 0 until currentPath.size )
      println( "stage(" + (i + 1) + ").size = " + currentPath(i).size )

    val ilpSolver = {
      if ( solverName == "lp_solve" )
        SolverLib.lp_solve
      else if ( solverName == "oJalgo" )
        SolverLib.oJalgo
      else
        SolverLib.gurobi
    }

    // outputs of the solver
    val muxCyc = collection.mutable.Map[(Int, Int), List[Boolean]]()
    val idxMapping = List.fill( currentPath.size ) { collection.mutable.Map[Int, Int]() }

    implicit val problem = MIProblem( ilpSolver )

    try {

      // declare the ILP vars
      val nodeEnables = ( 0 until currentPath.size ).map( s => ( 0 until currentPath(s).size ).map( p => {
        val thisOp = currentPath(s)(p)
        if ( isMux(thisOp) )
          ( 0 until maxCycles + 1 ).map( idx => MPIntVar( "node_" + s + "_" + p + "_" + idx, 0 to 1 ) ).toList
        else
          List( MPIntVar( "node_" + s + "_" + p, 0 to 1 ) )
      }).toList ).toList

      // connections between last layer and outputs
      val outputEdgeMapping = ( 0 until outputs.size ).map( kIdx => {
        val outIdxList = ( 0 until outCps.size ).filter( outCps(_).contains( outputs(kIdx ) ) ).toList
        assert( !outIdxList.isEmpty, "outIdxList should never be empty but was for " + outputs(kIdx) )
        outIdxList
      })
      println( "outputEdgeMapping = " + outputEdgeMapping )

      // edges mapping validOuts to output edges
      val outputEdges = ( 0 until outputEdgeMapping.size ).map( kIdx => {
        outputEdgeMapping(kIdx).map( si => {
          MPIntVar( "oe_" + kIdx + "_" + si, 0 to 1 )
        })
      }).toList

      // create the objective, just min number of nodes
      val obj = nodeEnables.reduce( _ ++ _ ).reduce( _ ++ _ ).reduce( (x : Expression, y : Expression ) => ( x + y ) )
      minimize( obj )

      // create constraint that exactly one oe must be active
      for ( oeList <- outputEdges.zipWithIndex ) {
        val oeSum = oeList._1.reduce( (x : Expression, y : Expression ) => ( x + y ) )
        add( oeSum := One )
      }

      // create constraint that if output edge is used then output node must be used
      for ( kIdx <- 0 until outputEdges.size ) {
        for ( oe <- ( outputEdgeMapping(kIdx) zip outputEdges(kIdx) ) )
          add( nodeEnables.last( oe._1 )( 0 ) >= oe._2 )
      }

      // create constraint that if node is used, then parent nodes must be active
      for ( stageIdx <- 0 until currentPath.size ) {
        for ( posIdx <- 0 until currentPath(stageIdx).size ) {
          val op = currentPath( stageIdx )( posIdx )
          val posA = ( op._1 - 1, op._2 )
          val posB = ( op._3 - 1, op._4 )
          if ( posA._1 >= 0 )
            add( nodeEnables( stageIdx )( posIdx )( 0 ) <= nodeEnables( posA._1 )( posA._2 )( 0 ) )
          if ( posA != posB && posB._1 >= 0 )
            add( nodeEnables( stageIdx )( posIdx )( 0 ) <= nodeEnables( posB._1 )( posB._2 )( 0 ) )
        }
      }

      /* Create mux cycle select constraints
       * Look at the 0 shifted cp out, constraint is cycle no is which stage it is in + cyc offset
       * Track back along all paths, constraint is active if path to output is active
       */
      for ( kIdx <- 0 until outputEdges.size ) {
        val outSet = outputs(kIdx)
        println( "creating mux constraints for validOuts(" + outSet + ") = " + validOuts( outSet ) )
        for ( cpIdx <- 0 until outputEdges( kIdx ).size ) {
          val posIdx = outputEdgeMapping( kIdx )( cpIdx )
          val si = outCps( posIdx ).indexOf( outSet )
          for ( cyc <- validOuts( outSet ) ) {
            val muxConstraints = createCycleConstraints( outputEdges(kIdx)(cpIdx), currentPath.size - 1,
              posIdx, si, nodeEnables, allCps, cyc + currentPath.size - 1, currentPath, "_" + posIdx + "_" + si )
            // add them all to the problem
            for ( mc <- muxConstraints )
              add( mc )
          }
        }
      }

      // launch ILP solver
      start()
      println("number of hardware blocks: " + objectiveValue)

      // create path from result
      for ( stageIdx <- 0 until currentPath.size ) {
        var count = 0
        for ( posIdx <- 0 until currentPath(stageIdx).size ) {
          if ( nodeEnables( stageIdx )( posIdx )( 0 ).value.get > 0.99 ) {
            idxMapping( stageIdx ).put( posIdx, count )
            count += 1
          }
        }
      }

      for ( stageIdx <- 0 until currentPath.size ) {
        for ( posIdx <- 0 until currentPath( stageIdx ).size ) {
          val thisNode = nodeEnables( stageIdx )( posIdx )
          if ( thisNode.size > 1 && idxMapping( stageIdx ).contains( posIdx ) )
            muxCyc.put( (stageIdx, idxMapping( stageIdx )( posIdx ) ), thisNode.drop(1).map( _.value.get > 0.99 ) )
        }
      }

      println( "muxCyc = " + muxCyc )

      for ( i <- 0 until outputEdges.size )
        println( "outputs(" + i + ") = " + outputs(i) + " has " + outputEdges(i).size + " edges for " + validOuts( outputs(i) ))

      for ( outIdx <- 0 until outputEdgeMapping.size ) {
        assert( outputEdges( outIdx ).map( _.value.get ).count( _ > 0.99 ) == 1, "Exactly one output edge should be active" )
        for ( oeIdx <- 0 until outputEdgeMapping( outIdx ).size ) {
          if ( outputEdges( outIdx )( oeIdx ).value.get > 0.99 ) {
            println( "output(" + outIdx + ") = " + outputs(outIdx) + " connected to output cp " +
              outputEdgeMapping( outIdx )( oeIdx ) + " -> " + idxMapping.last( outputEdgeMapping( outIdx )( oeIdx ) ) )
            println( "and has output set of " + outCps( outputEdgeMapping( outIdx )( oeIdx ) ) )
          }
        }
      }

    } finally {
      release()
    }

    val newPath = currentPath.zipWithIndex.map( stage => {
      stage._1.zipWithIndex.filter( pos => {
        idxMapping( stage._2 ).contains( pos._2 )
      }).map( pos => {
        val thisOp = pos._1
        val pos1 = {
          if ( thisOp._1 > 0 )
            idxMapping( thisOp._1 - 1 )( thisOp._2 )
          else
            thisOp._2
        }
        val pos2 = {
          if ( thisOp._3 > 0 )
            idxMapping( thisOp._3 - 1 )( thisOp._4 )
          else
            thisOp._4
        }
        ( thisOp._1, pos1, thisOp._3, pos2, thisOp._5 )
      })
    })

    ( newPath, muxCyc.toMap )
  }

  /** Branch at this level to find all ways of making the valid outs, then run ILP to select the best ones
    * validOuts is a map from output point to the cycle shifted points which require it
    * perhaps input should be latency from first num in, need to check no violations ...
    * forcing it to wait not best ... will create more waiting hardware
    * return the chosen paths
    */
  def cpPathExploreIter( currentPath : List[List[(Int, Int, Int, Int, Boolean)]], currentCps : List[List[Set[Set[(Int, Int)]]]], numPos : Int,
    validOuts : Map[Set[(Int, Int)], Set[Int]] ) : ( List[List[(Int, Int, Int, Int, Boolean)]], Map[(Int, Int), List[Boolean]] ) = {
    // validOuts are the outputs and constraints for this iteration
    // generate new stage are in validOuts
    // posIns and prev stage with everything else ( including themselves )
    // filter out if not in validOuts ( only apply to adds as only they can grow a set which isn't useful )

    val proposedStage = ArrayBuffer[(Int, Int, Int, Int, Boolean)]()
    val proposedCps = ArrayBuffer[Set[Set[(Int, Int)]]]()
    val noCycles = validOuts.keys.map( vo => cpToCycRequired( vo ) + vo.minBy( _._1 )._1 ).max + 1

    val useAllPath = false

    // first just the input positions
    for ( posIdx <- 0 until numPos ) {
      // only have in first layer
      if ( currentPath.size == 0 ) {
        for ( posIdx2 <- posIdx until numPos ) {
          proposedStage += { ( 0, posIdx, 0, posIdx2, false ) }
          if ( posIdx2 !=  posIdx && validOuts.contains( Set( (0, posIdx), (0, posIdx2) ) ) )
            proposedStage += { ( 0, posIdx, 0, posIdx2, true ) }
        }
      }

      // pair on all other stages ... or just previous?
      for ( stageIdx <- 0 until currentPath.size ) {
        for ( posIdx2 <- 0 until currentPath(stageIdx).size ) {
          // only incorporate adder if some muxset is in validOuts
          val muxSet = currentCps(stageIdx)(posIdx2).map( ms => cpStageDelay( currentPath.size, stageIdx + 1, ms ) )
          if ( muxSet.map( ms => validOuts.contains( Set( (currentPath.size, posIdx) ) ++ ms )).reduce( _ || _ ) )
            proposedStage +=  { ( 0, posIdx, stageIdx + 1, posIdx2, true ) }
          // cant have this as will be picked and replace cycle 0 inputs
          // proposedStage +=  { ( 0, posIdx, stageIdx + 1, posIdx2, false ) }
        }
      }
    }

    // now previous stage with all other stages ( including itself )
    if ( currentPath.size > 0 ) {
      val prevStage = currentPath.size
      for ( posIdx <- 0 until currentPath.last.size ) {
        val muxSetA = currentCps(prevStage - 1)(posIdx)
        val stageList = {
          if ( useAllPath )
            0 until prevStage
          else // only comb with prev stage
            prevStage - 1 until prevStage
        }
        val opA = currentPath( prevStage - 1 ) ( posIdx )
        val opAs = List(( opA._1, opA._2 ), ( opA._3, opA._4 ))
        for ( stageIdx <- stageList ) {
          for ( posIdx2 <- 0 until currentPath( stageIdx ).size ) {
            // incorporate adder if some muxset is in validOuts
            val muxSetB = currentCps(stageIdx)(posIdx2)
            if ( ( prevStage, posIdx ) != ( stageIdx + 1, posIdx2 ) && muxSetA.map( msA => {
              muxSetB.map( msB => validOuts.contains( msA ++ msB ) ).reduce( _ || _ ) }).reduce( _ || _ ) ) {
              val opB = currentPath( stageIdx )( posIdx2 )
              if ( !opAs.map( oa => Set( (opB._1, opB._2), (opB._3, opB._4) ).contains( oa ) ).reduce( _ || _ ) )
                proposedStage += { ( prevStage, posIdx, stageIdx + 1, posIdx2, true ) }
            }
            // filter muxes that can't be used for all output sets ( none overlap )
            proposedStage += { ( prevStage, posIdx, stageIdx + 1, posIdx2, false ) }
          }
        }
      }
    }

    for ( posIdx <- 0 until proposedStage.size ) {
      val thisOp = proposedStage( posIdx )
      proposedCps += getCpFromParents( thisOp, currentCps )
    }

    // select which path set is the best using ILP
    val ilpRes = runILP( currentPath ++ List(proposedStage.toList), currentCps ++ List(proposedCps.toList), noCycles, validOuts )

    // return the path
    ilpRes
  }

  def layeredCpPathExplore( sumStructure : List[Set[(Int, Int)]], outSize : Int ) : (
    List[List[(Int, Int, Int, Int, Boolean)]], Map[(Int, Int), List[Boolean]] )  = {
    val noPos = sumStructure.reduce( _ ++ _ ).map( _._2 ).max + 1
    val noCycles = sumStructure.map( ss => cpToCycRequired( ss ) ).max

    var currentPath = List[List[(Int, Int, Int, Int, Boolean)]]()

    var currentCps = List[List[Set[Set[(Int, Int)]]]]()

    // iterate over sum levels
    for ( cyc <- 0 until noCycles ) {
      // for each level determine the set of valid outs and call cpPathExploreIter
      val validOuts = generateCpForSums( sumStructure, cyc )
      val ilpRes = cpPathExploreIter( currentPath, currentCps, noPos, validOuts )
      currentPath = ilpRes._1

      // get new CPs for newLayer ( shifted to 0 )
      println( "updating cps" )
      val newCps = ArrayBuffer[List[ Set[Set[(Int, Int)]] ]]()
      for ( pathStage <- currentPath ) {
        val newLayerCps = ArrayBuffer[ Set[Set[(Int, Int)]] ]()
        for ( thisOp <- pathStage )
          newLayerCps += getCpFromParents( thisOp, newCps.toList )
        newCps += newLayerCps.toList
      }
      currentCps = newCps.toList
      println( "currentCps = " + currentCps )
    }

    // last iteration is sum structure
    val validOuts = collection.mutable.Map[Set[(Int, Int)], Set[Int]]()
    for ( sum <- sumStructure ) {
      val minCyc = sum.minBy( _._1 )._1
      val shiftedSet = { ( sum.map( x => ( x._1 - minCyc, x._2 ) ), minCyc ) }
      val newCycSet = validOuts.getOrElse( shiftedSet._1, Set[Int]() ) ++ Set( shiftedSet._2 )
      validOuts.put( shiftedSet._1, newCycSet )
    }
    val ilpRes = cpPathExploreIter( currentPath, currentCps, noPos, validOuts.toMap )

    println( "updating cps" )

    currentPath = ilpRes._1
    val newCps = ArrayBuffer[List[ Set[Set[(Int, Int)]] ]]()
    for ( pathStage <- currentPath ) {
      val newLayerCps = ArrayBuffer[ Set[Set[(Int, Int)]] ]()
      for ( thisOp <- pathStage )
        newLayerCps += getCpFromParents( thisOp, newCps.toList )
      newCps += newLayerCps.toList
    }
    currentCps = newCps.toList
    println( "currentCps = " + currentCps )

    // reorder last stage of current path to have same sum order
    val newLastIdx = sumStructure.map( sum => {
      var sIdx = -1
      var idx = 0
      while( idx < currentCps.last.size ) {
        if ( currentCps.last( idx ).contains( sum ) ) {
          sIdx = idx
          idx = currentCps.last.size
        }
        idx += 1
      }
      sIdx
    })

    val newLast = newLastIdx.map( currentPath.last(_) )
    println( "idx mapping = " + newLastIdx )
    println( "oldMap = " + ilpRes._2 )
    val newMap = ilpRes._2.map( x => {
      if ( x._1._1 == currentPath.size - 1 ) {
        ( ( x._1._1, newLastIdx.indexOf( x._1._2 ) ), x._2 )
      } else
        x
    })
    println( "newMap = " + newMap )

    // append newLast layer on end
    ( ilpRes._1.slice( 0, ilpRes._1.size - 1 ) :+ newLast, newMap )
  }

  def cpToPath( sumStructure : List[Set[(Int, Int)]], outSize : Int ) : List[List[(Int, Int, Int, Int, Boolean)]] = {
    val noPos = sumStructure.reduce( _ ++ _ ).map( _._2 ).max + 1
    val noCycles = sumStructure.reduce( _ ++ _ ).map( _._1 ).max + 1

    // each stage takes combos and either makes an add, mux or delay with all prev stages
    // prune any path that isn't used by any of the sums with validatePath
    // if mux in path get both options. only if both are subset then keep

    // stages are ( stageA, idxA, stageB, idxB, typeIdx ) to locate the two inputs
    // typeIdx = false for delay or mux, true for add
    val stages = ArrayBuffer[List[(Int, Int, Int, Int, Boolean)]]()

    val uniqueSums = filterUniqueSums( sumStructure )
    val maxSumLen = sumStructure.map( ss => {
      cpToCycRequired( ss )
    }).max
    val genMux = false

    // use CP to generate valid paths rather than gen all and trim
    // Have CP cache for each node
    // iteratively solve the ILP?
    // ie) find best way to generate all unique cyc shifted sets then reduce to those paths, extend to next layer
    // is iteratively solving A*?
    // number of sets = 2^n so number of components in sum matters
    // muxes are required otherwise infeasible?
    // how to insert later? work back from sums required?
    // why can't just include in? might be big but will be selected. no need all combinations ... or just relevant ones between related sums?

    for ( stageCount <- 0 until maxSumLen ) {
      val newStage = ArrayBuffer[(Int, Int, Int, Int, Boolean)]()
      // add to newStage every possible pos combination, every pos and prev stage combination
      val stageSize = { if ( stageCount == 0 ) 0 else stages(stageCount - 1).size }
      for ( posIdx <- 0 until noPos ) {
          // combinations of prev stage and position
        if ( stageCount > 0 ) {
          for ( posIdx2 <- ( 0 until stages(stageCount - 1).size ) ) {
            if ( genMux )
              newStage += { ( 0, posIdx, stageCount, posIdx2, false ) }
            newStage += { ( 0, posIdx, stageCount, posIdx2, true ) }
          }
        } else {
          // combinations of position with this stage delay
          for ( posIdx2 <- posIdx until noPos ) {
            if ( genMux )
              newStage += { ( 0, posIdx, 0, posIdx2, false ) }
            else if ( posIdx == posIdx2 )
              newStage += { ( 0, posIdx, 0, posIdx2, false ) }
            if ( posIdx != posIdx2 )
              newStage += { ( 0, posIdx, 0, posIdx2, true ) }
          }
        }
      }
      if ( stageCount > 0 ) {
        val thisStageSize = stages.last.size
        for ( posIdx <- 0 until thisStageSize ) {
          // combinations of prev stage with other stages ( including itself )
          for ( stageIdx <- ( 0 until stages.size ) ) {
            val startPos = { if ( stageIdx == stageCount - 1 ) posIdx else 0 }
            for ( posIdx2 <- ( startPos until stages(stageIdx).size ) ) {
              if ( genMux )
                newStage += { ( stageCount, posIdx, stageIdx + 1, posIdx2, false ) }
              else if ( ( stageCount, posIdx) == ( stageIdx + 1, posIdx2 ) )
                newStage += { ( stageCount, posIdx, stageIdx + 1, posIdx2, false ) }
              if ( posIdx != posIdx2 || stageCount != stageIdx + 1 )
                newStage += { ( stageCount, posIdx, stageIdx + 1, posIdx2, true ) }
            }
          }
        }
      }
      // val tmpStageBlock = stages.toList ++ List(newStage.toList) try for faster?
      // validate new ops
      val stageList = stages.toList
      for ( s <- stageList )
        println( s.size )
      println( "newStage.size = " + newStage.size )
      println( "filtering paths ... " )
      if ( stageCount == maxSumLen - 1 )
        stages += newStage.filter( ns => validatePath( stageList ++ List(List(ns)), uniqueSums, true ) ).toList
      else
        stages += newStage.filter( ns => validatePath( stageList ++ List(List(ns)), uniqueSums) ).toList
      println( "cycle " + stageCount + " done " )
    }
    println( "Pruning unused positions" )
    // remove unused stages
    val unusedPositions = HashSet[(Int, Int)]() // ( stage, pos )
    for ( stageIdx <- ( 0 until stages.size - 1 ).reverse ) {
      // check if any following stages use this one
      val usedPos = ( stageIdx + 1 until stages.size ).map( si => {
        ( 0 until stages(si).size ).map( (si, _) ).filterNot( unusedPositions.contains( _ ) )
      }).reduce( _ ++ _ ).map( x => {
        val s = stages(x._1)(x._2)
        HashSet( (s._1, s._2), (s._3, s._4) )
      }).reduce( _ ++ _ ).filter( _._1 == stageIdx ).map( _._2 )
      val unusedPos = ( 0 until stages(stageIdx).size ).filterNot( usedPos.contains(_) )
      unusedPositions ++= unusedPos.map( (stageIdx, _ ) )
    }
    println( "Generating map between index systems" )
    val idxMaps = ( 0 until stages.size - 1 ).map( stageIdx => {
      val thisStageUnused = unusedPositions.filter( _._1 == stageIdx + 1 ).map( _._2 )
      val positionNeeded = ( 0 until stages(stageIdx).size ).map( thisStageUnused.contains(_) )
      val thisStageIdxMap = collection.mutable.Map[Int, Int]()
      var count = 0
      for ( i <- ( 0 until stages(stageIdx).size ) ) {
        if ( positionNeeded( i ) ) {
          thisStageIdxMap.updated( i, count )
          count += 1
        }
      }
      thisStageIdxMap.toMap // change to immutable
    }).toList

    println( "Updating structure ... " )
    stages.zipWithIndex.map( s => {
      s._1.zipWithIndex.filter( p => {
        // check if in map
        idxMaps( s._2 ).contains( p._2 )
      }).map( p => {
        val thisOp = p._1
        ( thisOp._1, idxMaps(thisOp._1)(thisOp._2), thisOp._3, idxMaps(thisOp._3)(thisOp._4), thisOp._5 )
      })
    }).toList
  }

  def implementStructure( path : List[List[ (Int, Int, Int, Int, Boolean) ]],
    muxMap : Map[(Int, Int), List[Boolean]], inputs : List[Fixed], validIn : Bool ) : List[Fixed] = {
    val addStructure = ArrayBuffer[List[Fixed]]()
    val cycles = {
      val keys = muxMap.keys.toList
      if ( keys.size > 0 )
        muxMap( keys(0) ).size
      else
        0
    }
    println( "muxMap = " + muxMap )
    val cntr = Counter( validIn, cycles )
    addStructure += inputs
    for ( layer <- path.zipWithIndex ) {
      val newLayer = ArrayBuffer[Fixed]()
      for ( op <- layer._1.zipWithIndex ) {
        val thisOp = op._1
        if ( isDelay( thisOp ) ) {
          println( "add delay to (" + thisOp._1 + ")(" + thisOp._2 + ")" )
          newLayer += RegNext( addStructure( thisOp._1 )( thisOp._2 ) )
        } else if ( isAdd( thisOp ) ) {
          println( "adder on (" + thisOp._1 + ", " + thisOp._2 + ") + (" + thisOp._3 + ", " + thisOp._4 + ")" )
          val addRes = addStructure( thisOp._1 )( thisOp._2 ) + addStructure( thisOp._3 )( thisOp._4 )
          newLayer += RegNext( addRes )
        } else {
          // implement cycle conditions on mux
          val muxSwitches = muxMap( (layer._2, op._2) )
          println( "mux on (" + thisOp._1 + ")(" + thisOp._2 + "), (" + thisOp._3 + ", " + thisOp._4 + ")" )
          println( "muxMap( (" + layer._2 + ", " + op._2 + ") ) = " + muxSwitches )
          // TODO: do better than this ... look for don't care cycles and pick simplest pattern
          val muxCond = muxSwitches.zipWithIndex.filter( _._1 ).map( ms => {
            cntr._1 === UInt( ms._2 )
          }).reduceOption( _ || _ ).getOrElse( Bool(false) )
          val muxOut = Mux( muxCond, addStructure( thisOp._3 )( thisOp._4 ), addStructure( thisOp._1 )( thisOp._2 ) )
          newLayer += RegNext( muxOut )
        }
      }
      addStructure += newLayer.toList
    }
    addStructure.last
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

