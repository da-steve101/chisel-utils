import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import Chisel._
import chiselutils.math.SumScheduler
import chiselutils.math.Road
import chiselutils.math.Person

class SumSchedulerSuite extends TestSuite {

  val randNoMax = 1 << 8

  def getConvSums( imgSize : Int, filterSize : Int ) : List[Set[(Int, Int)]] = {
    // no padding, 1 filter
    // stream pixel by pixel
    // only odd filter
    // need to flip
    // input is 3 by 3 => positions 0 to 8
    // columns are x, rows are y
    val positions = ( 0 until filterSize*filterSize )
    val outSums = ( 0 until imgSize ).map( y => {
      ( 0 until imgSize ).map( x => {
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

  def generateInOut( cities : List[List[Set[Person]]] ) : (List[(Boolean, List[BigInt])], List[List[(BigInt, Boolean)]] ) = {
    val myRand = new Random
    val noInputs = cities.map( city => city.map( convoy => convoy.maxBy( _.beachId ).beachId ).max ).max + 1
    val noCyc = 2*cities.head.size
    val inputs = List.fill( noCyc ) { ( true, List.fill( noInputs ){ BigInt(myRand.nextInt(randNoMax)) } ) }
    println( inputs )
    // todo: do a better calc of latency ( ensure everything can be computed in time )
    val latency = cities.map( city => city.head.maxBy( _.roadTripTime ).roadTripTime ).max
    val outputCities = cities.map( city => city.zipWithIndex.map( convoy => {
      convoy._1.map( p => { inputs( latency + convoy._2 - p.roadTripTime )._2( p.beachId ) }).reduce( _ + _ )
    }).map( x => ( x, true ) ) )
    val outputs = ( 0 until outputCities.head.size ).map( idx => outputCities.map( _(idx) ) ).toList
    ( inputs, List.fill( latency - 1 ){ List.fill( cities.size ) { ( BigInt(0), false ) } } ++ outputs )
  }

  def setToSum( inputNums : List[List[BigInt]], cpSet : Set[(Int, Int)] ) : BigInt = {
    cpSet.toList.map( x => inputNums( x._1 )( x._2 ) ).reduce( _ + _ )
  }

  def testHardware( cities : List[Road], inputNums : List[(Boolean, List[BigInt])],
    outputNums : List[List[(BigInt, Boolean)]] ) : Unit = {

    println( "cities = " + cities )
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
      val sumRes = SumScheduler.implementStructure( cities, io.dataIn.toList, Bool(true) )
      for ( rd <- cities )
        rd.printfCond( this )
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
          else
            peek( c.io.dataOut( oIdx ) )
        }
      }
    }
    chiselMainTest( Array("--genHarness", "--compile", "--test",
      "--backend", "c", "--targetDir", dir.getPath.toString()),
      () => Module( new SumSchedMod ) ) { c => new SumSchedTests(c) }
    // launchCppTester( ( m : SumSchedMod ) => new SumSchedTests(m) )
  }

/*
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

 */
  @Test def convSumRun {
    val convSums = getConvSums( 5, 3 ).map( s => s.map( p => ( p._1, p._2, 0, false ) ) )
    println( convSums )
    val city = SumScheduler.cpToCity( convSums, ( 0 until convSums.size ).map( _ + 10 ).toList )
    val cities = List( city )
    val cityRoads = SumScheduler.buildRoads( cities )
    val inOut = generateInOut( cities )
    testHardware( cityRoads, inOut._1, inOut._2 )
  }

  @Test def customFollow {
    val city = List(
      Set( Person( 5, 0, 0, false ), Person( 4, 1, 0, false ) ),
      Set( Person( 5, 0, 0, false ), Person( 4, 1, 0, false ), Person( 3, 2, 0, false ) ),
      Set( Person( 4, 1, 0, false ), Person( 3, 2, 0, false ) )
    )
    val cities = List(city)
    val testCity = List(Set(Person( 2, 1, 0, false )), Set[Person](), Set(Person( 3, 0, 0, false ), Person( 2, 1, 0, false )))
    println( SumScheduler.cityMinRoad( testCity ) )
    val cityRoads = SumScheduler.buildRoads( cities )
    val inOut = generateInOut( cities )
    testHardware( cityRoads, inOut._1, inOut._2 )
  }
/*

  @Test def testConvPath {
    val convSums = getConvSums( 5, 3 )
    val convPath = SumScheduler.cpToPath( convSums, 1 )
    println( convPath )
  }

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

  @Test def smallSum {
    val sums = List(
      Set( (0,0), (1, 3), (2, 3) ),
      Set( (1,1), (2, 3), (3, 3) ),
      Set( (0,0), (3,3), (1, 2), (2, 3 ) )
    )
    val city = SumScheduler.cpToCity( sums.map( s => s.map( x => ( x._1, x._2, 0, false ) )), List( 3, 4, 5 ) )

    println( "city = " + city )

    val cities = SumScheduler.buildRoads( List( city ) )

    val myRand = new Random
    val inputNums = List.fill( 20 ){ (true, List.fill( 4 ){ BigInt(myRand.nextInt(100)) }) }
    val outputNums = ( 0 until 20 ).map( xIdx => {
      val shiftedSums = sums.map( s => {
        val sumSet = s.map( cp => { ( cp._1 + xIdx - 3, cp._2 ) })
        if ( xIdx % 4 == 0 )
          ( setToSum( inputNums.map( _._2 ), sumSet ), true )
        else
          ( BigInt(0), false )
      })
      shiftedSums
    }).toList

    println( "inputNums = " + inputNums )
    println( "outputNums = " + outputNums )

    testHardware( cities, inputNums, outputNums )
  }
*/

  /*
   chiselMainTest(Array("--genHarness", "--compile", "--test", "--backend", "c"), () => {
   Module( new ConstMult( bw, fw, numsIn.toList ) ) }) { c => new ConstTester( c ) }
   */
}
