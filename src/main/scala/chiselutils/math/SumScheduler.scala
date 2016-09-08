
package chiselutils.math

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import optimus.optimization._
import optimus.algebra.Expression
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

  /** Select a root and generate a new path back from it

  def filterPath( path : List[List[(Int, Int, Int, Int, Int)]], rootIdx : Int ) : List[List[(Int, Int, Int, Int, Int)]] = {
    val thisOp = path.last(rootIdx)
    if ( path.size == 2 ) {
      if ( thisOp._1 != 0 && thisOp._3 != 0 ) {
        if ( thisOp._2 == thisOp._4 ) // make sure no duplicate
          return List( List( path(0)(thisOp._2) ), List( ( 1, 0, 1, 0, thisOp._5 ) ) )
        return List( List( path(0)(thisOp._2), path(0)(thisOp._4) ), List( ( 1, 0, 1, 1, thisOp._5 ) ) )
      } else if ( thisOp._1 != 0 )
        return List( List( path(0)(thisOp._2) ), List( ( 1, 0, 0, thisOp._4, thisOp._5 ) ) )
      else if ( thisOp._3 != 0 )
        return List( List( path(0)(thisOp._4) ), List( ( 0, thisOp._2, 1, 0, thisOp._5 ) ) )
      else
        return List( List[(Int, Int, Int, Int, Int)](), List( thisOp ) )
    }
  
    if ( thisOp._1 != 0 && thisOp._3 != 0 ) {
      val pathA = filterPath( path.dropRight( path.size - thisOp._1 ), thisOp._2 )
      val pathB = filterPath( path.dropRight( path.size - thisOp._3 ), thisOp._4 )

    } else if ( thisOp._1 != 0 ) {

    } else if ( thisOp._3 != 0 ) {

    } else
  
    return List.fill( path.size - 1 ) { List[(Int, Int, Int, Int, Int)]() } ++ List( thisOp )
  }
    */

  /** Convert a path to all possible CP coordinates
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

  /** Get the min number of cycles needed to realize the set of CP */
  def cpToCycRequired( cp : Set[(Int, Int)] ) : Int = {
    // get the min extra cycle needed for adder tree if multiple on same cycle
    val cpCycMax = cp.groupBy( _._1 ).map( x => x._1 + log2Ceil(x._2.size) ).max
    cpCycMax - cp.minBy( _._1 )._1
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

  /** Validate for muxes ... is there some way just to generate the required ones rather than generate all then filter?
    * Maybe mux only on prev stage and new input? lose generality but makes alot smaller
    * Look at sums and generate subcomponents?
    * Only generate mux with prev stage and everything else( including itself ) rely on ILP to choose multiple layers
    * ILP is looking at cycle deps for all sums so just statisfying that, still full generality
    */
  def isMuxUseful( uniqueSums : List[Set[(Int, Int)]], muxSet : Set[Set[(Int, Int)]], noLayer : Int ) : Boolean = {
    // check if could be realized with less mux
    if ( log2Ceil(muxSet.size) < noLayer )
      return false
    /* A mux is useful if:
     * it is muxing something from muxSet.size or more sums that have other components in common
     * those sums can't need the mux on the same cycle, but up to ILP to figure that out
     */
    val muxesInUse : Int = uniqueSums.map( us => {
      muxSet.toList.map( ms => isSubsetCycShift( us, ms )._2 )
    }).reduce( (x, y) => ( x zip y ).map( z => z._1 || z._2 ) ).count( b => b == true ) // reduce to see how many are used
    // if insufficient number can be used, then no not useful
    if ( log2Ceil( muxesInUse ) < noLayer )
      return false
    true
  }

  def runILP( currentPath : List[List[(Int, Int, Int, Int, Boolean)]],
    outCps : List[List[Set[(Int, Int)]]], noCycles : Int,
    validOuts : Map[Set[(Int, Int)], Set[Int]] ) : List[List[(Int, Int, Int, Int, Boolean)]] = {

    // declare the ILP vars
    val nodeEnables = ( 0 until currentPath.size ).map( s => ( 0 until currentPath(s).size ).map( p => {
      val n = currentPath(s)(p)
      // if mux
      if ( !n._5 && ( n._1, n._2 ) != ( n._3, n._4 ) )
        ( 0 until noCycles + 1 ).map( idx => MPIntVar( "node_" + s + "_" + p + "_" + idx, 0 to 1 ) ).toList
      else
        List( MPIntVar( "node_" + s + "_" + p, 0 to 1 ) )
    }).toList ).toList

    val outputEdges = ( 0 until outCps.size ).map( oe => {
      ( 0 until outCps(oe).size ).map( si => {
        MpIntVar( "oe_" + oe + "_" + si, 0 to 1 )
      })
    })

    // create the objective, just min number of nodes
    val obj = nodeEnables.reduce( _ ++ _ ).reduce( _ ++ _ ).reduce( (x, y) => ( x + y ) )

    // create constraint that exactly one oe must be active
    val outputConstraintMap = collection.mutable.Map[Set[(Int, Int)], Expression]()
    for ( oe <- 0 until outCps.size ) {
      for ( si <- 0 until outCps(oe).size ) {
        val key = outCps(oe)(si)
        val value = outputConstraintMap.getOrElse( key, ZERO ) + outputEdges(oe)(si)
        outputConstraintMap.put( key, value )
      }
    }
    // TODO: check here that all outputs are accounted for
    // apply the constraint
    for ( value <- outputConstraintMap.values )
      add( value := 1 )

    // create constraint that if output edge is used then node must be used
    for ( oe <- 0 until outCps.size ) {
      for ( x <- outputEdges(oe) )
        add( nodeEnables.last( oe )( 0 ) >= outputEdges( oe )( x ) )
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


    // launch ILP solver

    // create path from result

  }

  /** Branch at this level to find all ways of making the valid outs, then run ILP to select the best ones
    * validOuts is a map from output point to the cycle shifted points which require it
    * perhaps input should be latency from first num in, need to check no violations ...
    * forcing it to wait not best ... will create more waiting hardware
    * return the chosen paths
    */
  def cpPathExploreIter( currentPath : List[List[(Int, Int, Int, Int, Boolean)]],
    currentCps : List[List[Set[Set[(Int, Int)]]]], numPos : Int,
    validOuts : Map[Set[(Int, Int)], Set[Int]] ) : (
    List[List[(Int, Int, Int, Int, Boolean)]], List[List[Set[Set[(Int, Int)]]]] ) = {
    // validOuts are the outputs and constraints for this iteration
    // generate new stage are in validOuts
    // posIns and prev stage with everything else ( including themselves )
    // filter out if not in validOuts ( only apply to adds as only they can grow a set which isn't useful )

    val proposedStage = ArrayBuffer[(Int, Int, Int, Int, Boolean)]()

    // first just the input positions
    for ( posIdx <- 0 until noPos ) {
      // pair on stage 0
      for ( posIdx2 <- posIdx until noPos ) {
        proposedStage += ( 0, posIdx, 0, posIdx2, false )
        if ( posIdx2 !=  posIdx && validOuts.contains( Set( (0, posIdx), (0, posIdx2) ) ) )
          proposedStage += ( 0, posIdx, 0, posIdx2, true )
      }

      // pair on all other stages
      for ( stageIdx <- 0 until currentPath.size ) {
        for ( pos2Idx <- 0 until currentPath(stageIdx).size ) {
          // only incorporate adder if some muxset is in validOuts
          val muxSet = currentCps(stageIdx)(posIdx2)
          if ( muxSet.map( ms => validOuts.contains( Set( (0, posIdx) ) ++ ms )).reduce( _ || _ ) )
            proposedStage +=  ( 0, posIdx, stageIdx + 1, posIdx2, true )
          proposedStage +=  ( 0, posIdx, stageIdx + 1, posIdx2, false )
        }
      }
    }

    // now previous stage with all other stages ( including itself )
    if ( currentPath.size > 0 ) {
      val prevStage = currentPath.size
      for ( posIdx <- 0 until currentPath.last.size ) {
        val muxSetA = currentCps(prevStage - 1)(posIdx2)
        for ( stageIdx <- 0 until prevStage ) {
          for ( posIdx2 <- 0 until currentPath( prevStage - 1 ).size ) {
            // incorporate adder if some muxset is in validOuts
            val muxSetB = currentCps(stageIdx)(posIdx2)
            if ( ( prevStage, posIdx ) != ( stageIdx + 1, posIdx2 ) && muxSetA.map( msA => {
              muxSetB.map( msB => validOuts.contains( msA ++ msB ) ).reduce( _ || _ ) }).reduce( _ || _ ) )
              proposedStage += ( prevStage, posIdx, stageIdx + 1, posIdx2, true )
            proposedStage += ( prevStage, posIdx, stageIdx + 1, posIdx2, false )
          }
        }
      }
    }

    // select which path set is the best using ILP
    val newPath = runILP( currentPath ++ List(proposedStage.toList), validOuts )


    // find which are best newLayer

    // get new CPs for newLayer ( shifted to 0 )

    // return the path
    ( newPath, currentCps ++ List(newCps) )
  }

  def layeredCpPathExplore( sumStructure : List[Set[(Int, Int)]], outSize : Int ) : List[List[(Int, Int, Int, Int, Boolean)]] = {
    val noPos = sumStructure.reduce( _ ++ _ ).map( _._2 ).max + 1
    val noCycles = sumStructure.map( ss => cpToCycRequired( ss ) ).max

    val stages = ArrayBuffer[List[(Int, Int, Int, Int, Boolean)]]()

    val genMux = false

    // iterate over sum levels
    for ( cyc <- 0 until noCycles ) {
      // for each level determine the set of valid outs and call cpPathExploreIter
      val validOuts = generateCpForSums( sumStructure, cyc )
    }
    List[List[(Int, Int, Int, Int, Boolean)]]()
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

