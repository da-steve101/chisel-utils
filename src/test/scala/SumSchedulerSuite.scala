import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import Chisel._
import chiselutils.math.SumScheduler

class SumSchedulerSuite extends TestSuite {

  def getConvSums( imgSize : Int, filterSize : Int ) : List[Set[(Int, Int)]] = {
    // no padding, 1 filter
    // stream pixel by pixel
    // only odd filter
    // need to flip
    // input is 3 by 3 => positions 0 to 8
    // columns are x, rows are y
    val positions = ( 0 until filterSize*filterSize )
    val outSums = ( 0 until imgSize ).map( x => {
      ( 0 until imgSize ).map( y => {
        positions.map( p => {
          val px = ( p % filterSize ) - ( filterSize / 2 )
          val py = ( p / filterSize ) - ( filterSize / 2 )
          val xpx = x + px
          val ypy = y + py
          ( xpx, ypy, p )
        }).filter { case ( xpx, ypy, p ) => {
          ( xpx >= 0 && xpx < imgSize && ypy >= 0 && ypy < imgSize )
        }}.map { case ( xpx, ypy, p ) => {
          ( ypy*imgSize + xpx, p )
        }}.toSet
      })
    }).reduce( _ ++ _ ).toList
    outSums
  }

  def setToSum( inputNums : List[List[BigInt]], cpSet : Set[(Int, Int)] ) : BigInt = {
    cpSet.toList.map( x => inputNums( x._1 )( x._2 ) ).reduce( _ + _ )
  }

  def testHardware( path : List[List[ (Int, Int, Int, Int, Boolean) ]],
    muxMap : Map[(Int, Int), List[Boolean]], inputNums : List[(Boolean, List[BigInt])],
    outputNums : List[List[(BigInt, Boolean)]] ) : Unit = {

    println( "path = " + path )
    println( "inputNums = " + inputNums )
    println( "outputNums = " + outputNums )

    val noInPos = inputNums(0)._2.size
    val noOutPos = outputNums(0).size

    class SumSchedMod extends Module {
      val io = new Bundle {
        // val validIn = Bool( INPUT )
        val dataIn = Vec( noInPos, Fixed( INPUT, 16, 8 ) )
        val dataOut = Vec( noOutPos, Fixed( OUTPUT, 16, 8 ) )
      }
      val sumRes = SumScheduler.implementStructure( path, muxMap, io.dataIn.toList, Bool(true) )
      io.dataOut := Vec( sumRes )
    }

    class SumSchedTests( c : SumSchedMod ) extends Tester( c ) {
      val noCyc = outputNums.size
      for ( x <- 0 until noCyc ) {
        if ( x < inputNums.size ) {
          val thisInput = inputNums(x)
          // poke( c.io.validIn, thisInput._1 )
          poke( c.io.dataIn, thisInput._2.toArray )
        }

        step(1)

        val thisOutput = outputNums(x)
        for ( oIdx <- 0 until thisOutput.size ) {
          if ( thisOutput( oIdx )._2  )
            expect( c.io.dataOut( oIdx ), thisOutput( oIdx )._1 )
        }
      }
    }
    chiselMainTest( Array("--genHarness", "--compile", "--test",
      "--backend", "c", "--targetDir", dir.getPath.toString()),
      () => Module( new SumSchedMod ) ) { c => new SumSchedTests(c) }
    // launchCppTester( ( m : SumSchedMod ) => new SumSchedTests(m) )
  }

  @Test def adderTreeCPTest {
    // generate adder tree
    for ( depth <- 1 until 12 ) {
      val adderTreePath = ( 0 until depth ).map( lyrIdx => {
        ( 0 until (1 << ( depth - lyrIdx - 1 ) )).map( addIdx => {
          ( lyrIdx, addIdx*2, lyrIdx, addIdx*2 + 1, true )
        }).toList
      }).toList
      val cpCoords = SumScheduler.pathToCP( adderTreePath, 0 )
      assert( 1 == cpCoords.size, "Adder tree should only have one possible set of CP coords" )
      val cpSet = cpCoords(0)
      assert( (1 << depth) == cpSet.size, "cpSet should have all positions" )
      for ( posIdx <- ( 0 until ( 1 << depth )) )
        assert( cpSet.contains( ( 0, posIdx ) ), "cpSet should contain all positions with cycle 0" )
    }
  }

  @Test def muxTreeCPTest {
    // generate adder tree
    for ( depth <- 1 until 12 ) {
      val muxTreePath = ( 0 until depth ).map( lyrIdx => {
        ( 0 until (1 << ( depth - lyrIdx - 1 ) )).map( addIdx => {
          ( lyrIdx, addIdx*2, lyrIdx, addIdx*2 + 1, false )
        }).toList
      }).toList
      val cpCoords = SumScheduler.pathToCP( muxTreePath, 0 )
      assert( ( 1 << depth ) == cpCoords.size, "Mux tree should have all possible inputs" )
      val cpCoordsUnSet = cpCoords.reduce( _ ++ _ ).toList.sortBy( _._2 )
      for ( cpSet <- cpCoords.zipWithIndex ) {
        assert( 1 == cpSet._1.size, "cpSet should have a single input" )
        assert( cpCoordsUnSet(cpSet._2) == ( 0, cpSet._2 ), "cpSet should contain correct inputs" )
      }
    }
  }

  @Test def delayCPTest {
    // generate adder tree
    for ( depth <- 1 until 12 ) {
      val delayPath = ( 0 until depth ).map( lyrIdx => {
        ( 0 until (1 << depth)).map( addIdx => {
          ( lyrIdx, addIdx, lyrIdx, addIdx, false )
        }).toList
      }).toList
      for ( i <- ( 0 until ( 1 << depth ) ) ) {
        val cpCoords = SumScheduler.pathToCP( delayPath, i )
        assert( 1 == cpCoords.size, "Delay path should only have one possible set of input" )
        val cpSet = cpCoords(0)
        assert( 1 == cpSet.size, "cpSet should have a single input" )
        assert( cpSet.contains( ( 0, i ) ), "cpSet should contain correct inputs" )
      }
    }
  }

  @Test def linearAddCPTest {
    for ( depth <- 1 until 12 ) {
      val adderPath = ( 0 until depth ).map( lyrIdx => {
        List( ( lyrIdx, 0, 0, lyrIdx + 1, true ) )
      }).toList
      val cpCoords = SumScheduler.pathToCP( adderPath, 0 )
      assert( 1 == cpCoords.size, "Adder tree should only have one possible set of CP coords" )
      val cpSet = cpCoords(0)
      assert( depth + 1 == cpSet.size, "cpSet should have all positions" )
      assert( cpSet.contains( ( 0, 0) ), "cpSet should contain all positions" )
      for ( posIdx <- ( 0 until depth ) )
        assert( cpSet.contains( ( posIdx, posIdx + 1 ) ), "cpSet should contain all positions" )
    }
  }

  @Test def adderTreeAndLinearCPTest {
    for ( treeNums <- 2 until 10 ) {
      // one layer stage of adder tree
      val initalAdderTree = ( 0 until treeNums ).map( i => {
        ( 0, 2*i, 0, 2*i + 1, true )
      }).toList
      // delay first
      val add0Delay = List( ( 1, 0, 1, 0, false ) )
      // then linear sum
      val adderPath = ( 1 until treeNums ).map( lyrIdx => {
        List( ( lyrIdx + 1, 0, 1, lyrIdx, true ) )
      }).toList
      val cpCoords = SumScheduler.pathToCP( List( initalAdderTree ) ++ List( add0Delay) ++ adderPath, 0 )
      assert( 1 == cpCoords.size, "Adders should only have one possible set of CP coords" )
      val cpSet = cpCoords(0)
      assert( treeNums*2 == cpSet.size, "cpSet should have all positions" )
      for ( posIdx <- ( 0 until treeNums ) )
        assert( cpSet.contains( ( posIdx, 2*posIdx ) ) && cpSet.contains( ( posIdx, 2*posIdx + 1) ), "cpSet should contain all positions" )
    }
  }

  @Test def partitionAtCycleTest {
    val convSums = getConvSums( 5, 3 )
    val uniqueSums = SumScheduler.filterUniqueSums( convSums )
    val cycReq = convSums.map( cs => SumScheduler.cpToCycRequired( cs ) ).max + 1 // add 1 as need to mux last
    println( "cycReq = " + cycReq )
    var partitions = List[Set[(Int, Int)]]()
    for ( cyc <- 0 until cycReq )
      partitions = SumScheduler.partitionAtCycle( uniqueSums, cyc )
    for ( cs <- convSums ) {
      val csTest = partitions.map( p => SumScheduler.isSubsetCycShift( p, cs )._2 ).reduce( _ || _ )
      if ( !csTest )
        println( "cs = " + cs + " not found in " + partitions.filter( p => p.size == cs.size ) )
      assert( csTest, "All sums must have aleast one path available" )
    }
  }

  @Test def generateCpForSumsTest {
    val convSums = getConvSums( 5, 3 )
    val cycReq = convSums.map( cs => SumScheduler.cpToCycRequired( cs ) ).max + 1 // add 1 as need to mux last
    println( "cycReq = " + cycReq )
    var partitions = Map[Set[(Int, Int)], Set[Int]]()
    for ( cyc <- 0 until cycReq )
      partitions = SumScheduler.generateCpForSums( convSums, cyc )
    for ( cs <- convSums ) {
      val csShift = cs.minBy( _._1 )._1
      val csShifted = cs.map( x => ( x._1 - csShift, x._2 ) )
      val csTest = partitions(csShifted).contains( csShift )
      if ( !csTest )
        println( "cs = " + cs + " not found in " + partitions(csShifted) )
      assert( csTest, "All sums must have aleast one path available" )
    }
  }

  @Test def manualGenerateCpForSumsTest {
    val sums = List(
      Set( (0,0), (1, 3), (4, 4), (2, 3 ) ),
      Set( (0,1), (1, 3), (3, 4), (2, 3 ) ),
      Set( (0,0), (3, 3), (2, 2), (2, 3 ) )
    )
    val genMap = SumScheduler.generateCpForSums( sums, 0 )
    val manualMap = collection.mutable.Map[ Set[(Int, Int)], Set[Int] ]()
    manualMap.put( Set((0,0)), Set(0) )
    manualMap.put( Set((0,1)), Set(0) )
    manualMap.put( Set((0,2)), Set(2) )
    manualMap.put( Set((0,3)), Set(1,2,3) )
    manualMap.put( Set((0,4)), Set(3,4) )
    manualMap.put( Set((0,3), (0,2)), Set(2) )
    assert( manualMap.toMap == genMap, "Should generate combinations correctly" )
  }

  /*
  @Test def convSumRun {
    val convSums = getConvSums( 5, 3 )
    val ilpRes = SumScheduler.layeredCpPathExplore( convSums, 1 )
    println( ilpRes )
  }
   */
  /*
  @Test def testConvPath {
    val convSums = getConvSums( 5, 3 )
    val convPath = SumScheduler.cpToPath( convSums, 1 )
    println( convPath )
  }
   */
  /*
  @Test def manualSumRun {
    val sums = List(
      Set( (0,0), (1, 3), (4, 4), (2, 3 ) ),
      Set( (0,1), (1, 3), (3, 4), (2, 3 ) ),
      //      Set( (0,4), (2, 3), (4, 2), (3, 3 ) ),
      //      Set( (2,5), (3, 3), (3, 5), (4, 3 ) ),
      Set( (0,0), (3,3), (1, 2), (2, 3 ) )
    )
    val ilpRes = SumScheduler.layeredCpPathExplore( sums, 1 )
    println( "ilpRes = " + ilpRes )
  }
   */

  @Test def smallSum {
    val sums = List(
      Set( (0,0), (1, 3), (2, 3 ) ),
      Set( (0,1), (1, 3), (2, 3 ) ),
      Set( (0,0), (3,3), (1, 2), (2, 3 ) )
    )
    val ilpRes = SumScheduler.layeredCpPathExplore( sums, 1 )

    println( "ilpRes = " + ilpRes )

    val myRand = new Random
    val inputNums = List.fill( 20 ){ (true, List.fill( 4 ){ BigInt(myRand.nextInt(100)) }) }
    val outputNums = ( 0 until 20 ).map( xIdx => {
      val shiftedSums = sums.map( s => {
        val sumSet = s.map( cp => { ( cp._1 + xIdx - 3, cp._2 ) })
        if ( xIdx >= 3 )
          ( setToSum( inputNums.map( _._2 ), sumSet ), true )
        else
          ( BigInt(0), false )
      })
      shiftedSums
    }).toList

    println( "inputNums = " + inputNums )
    println( "outputNums = " + outputNums )

    testHardware( ilpRes._1, ilpRes._2, inputNums, outputNums )
    // testHardware( ilpRes._1.slice( 0, 1 ), ilpRes._2, inputNums, outputNums.map( x => x.slice( 0, 2 ) ) )
  }

  /*
   chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
   Module( new ConstMult( bw, fw, numsIn.toList ) ) }) { c => new ConstTester( c ) }
   */
}
