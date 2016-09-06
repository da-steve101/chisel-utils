package chiselutils.algorithms

import optimus.optimization._
import optimus.algebra.Expression
import scala.collection.mutable.ArrayBuffer

object PmcmILP {

  def solveILP( numsIn : List[BigInt], solverName : String = "gurobi" ) : List[Set[(BigInt, BigInt, BigInt, Int)]] = {
    val adMin = numsIn.map( n => RPAG.getAdMin( n ) ).max
    println( "adMin = " + adMin )
    // a list of numbers in each stage
    val stages = ArrayBuffer[List[BigInt]]()
    // a list of hyper edges in each stage ( idxW, idxU, idxV )
    val hyperEdges = ArrayBuffer[List[(Int, Int, Int)]]()
    stages += numsIn
    for ( ad <- ( 1 until adMin ).reverse ) {
      // find all csd representations for previous stage of numbers
      val csdReps = stages.last.map( n => {
        RPAG.getMsdComb( RPAG.toCsd(n) ) })
      // for each csd combination find all predecessors with adMin less than x
      val bitComb = csdReps.map( msd => {
        // for each representation
        val msdComb = msd.map( c => {
          // find combinations
          RPAG.bitCombinations( c, ad ).map( b => {
            // convert to BigInts
            ( RPAG.fromCsd( b._1 ), RPAG.fromCsd( b._2 ) )
          })
        }).reduce( _ ++ _ ) // combine all representations
        val r = RPAG.fromCsd( msd.toList(0) )
        if ( RPAG.getAdMin( r ) <= ad ) {
          msdComb ++ Set( ( r, r ) )
        } else
          msdComb
      })
      // get unique numbers in this stage
      val commonStage = bitComb.map( bSet => {
        bSet.map( b => Set( b._1, b._2 ) ).reduce( _ ++ _ )
      }).reduce( _ ++ _ ).toList
      stages += commonStage

      val commonMap = commonStage.zipWithIndex.toMap
      // add hyper edges from current stage to common stage
      hyperEdges += bitComb.zipWithIndex.map( bz => {
        bz._1.map( bPair => {
          ( bz._2, commonMap(bPair._1), commonMap(bPair._2) )
        })
      }).reduce( _ ++ _ ).toList
    }

    // attach the adMin = 2 layer to 1
    hyperEdges += ( 0 until stages.last.size).map( i => ( i, 0, 0 ) ).toList
    stages += List( BigInt(1) )

    val heAll = hyperEdges.reverse.toList
    val stagesAll = stages.reverse.toList

    val ilpSolver = {
      if ( solverName == "lp_solve" )
        SolverLib.lp_solve
      else if ( solverName == "oJalgo" )
        SolverLib.oJalgo
      else
        SolverLib.gurobi
    }

    implicit val problem = MIProblem( ilpSolver )

    // convert stages and hyperedges to binary variables
    val edgeVars = heAll.zipWithIndex.map( heStage => {
      heStage._1.zipWithIndex.map( he => {
        MPIntVar("he_" + heStage._2 + "_" + he._2, (0 to 1) )
      })
    })

    // use cost = 1 for FPGA
    val numAdders = edgeVars.reduce( _ ++ _ ).reduce( ( x : Expression, y : Expression ) => x + y )
    minimize( numAdders )

    // constrain one active hyperedge on each output
    for ( idx <- ( 0 until numsIn.size ) ) {
      val edgeIdxs = heAll.last.zipWithIndex.filter( _._1._1 == idx ).map( _._2 )
      val edgeList = edgeIdxs.map(
        eIdx => edgeVars.last( eIdx )
      )
      if ( !edgeList.isEmpty ) {
        val edgeSum = edgeList.reduce( ( x : Expression, y : Expression ) => x + y )
        add( edgeSum := 1 )
      }
    }

    // constrain if hyperedge is used then at least one hyperedge leading to it
    for ( s <- ( 1 until heAll.size ) ) {
      for ( he <- heAll(s).zipWithIndex ) {
        val uIdx = he._1._2
        val vIdx = he._1._3
        // possible edges to fill u
        val heUIdxs = heAll( s - 1 ).zipWithIndex.filter( _._1._1 == uIdx ).map( _._2 )
        val edgeInputSumU = heUIdxs.map( heIdx => edgeVars( s - 1 )( heIdx ) ).reduce( ( x : Expression, y : Expression ) => x + y )
        add( edgeInputSumU >= edgeVars( s )( he._2 ) )
        // dont double constrain
        if ( uIdx != vIdx ) {
          val heVIdxs = heAll( s - 1 ).zipWithIndex.filter( _._1._1 == vIdx ).map( _._2 )
          val edgeInputSumV = heVIdxs.map( heIdx => edgeVars( s - 1 )( heIdx ) ).reduce( ( x : Expression, y : Expression ) => x + y )
          add( edgeInputSumV >= edgeVars( s )( he._2 ) )
        }
      }
    }

    start()
    println("number of adders: " + objectiveValue)
    // extract the vals
    val edgeIdxs = edgeVars.map( x => x.zipWithIndex.filter( _._1.value.get == 1 ).map( _._2 ) )
    val adderStructure = ( 0 until heAll.size ).toList.map( heIdx => {
      edgeIdxs( heIdx ).map( ei => {
        val edge = heAll(heIdx)(ei)
        val u = stagesAll( heIdx )( edge._2 )
        val v = stagesAll( heIdx )( edge._3 )
        val w = stagesAll( heIdx + 1 )( edge._1 )
        ( w, u, v, -1 )
      }).toSet
    })

    release()

    adderStructure
  }
}
