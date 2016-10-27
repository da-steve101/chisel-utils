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

  class SumSchedulerTester( c : SumScheduler, inputNums : List[(Boolean, List[BigInt])],
    outputNums : List[List[(BigInt, Boolean)]] ) extends Tester( c ) {
    val noCyc = outputNums.size
    for ( x <- 0 until noCyc ) {
      if ( x < inputNums.size ) {
        val thisInput = inputNums(x)
        // poke( c.io.validIn, thisInput._1 ) // TODO: put valid in
        poke( c.io.in, thisInput._2.toArray )
      }

      step(1)

      val thisOutput = outputNums(x)
      for ( oIdx <- 0 until thisOutput.size ) {
        if ( thisOutput( oIdx )._2  )
          expect( c.io.out( oIdx ), thisOutput( oIdx )._1 )
        else
          peek( c.io.out( oIdx ) )
      }
    }
  }

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
    println( "latency for I/O = " + latency )
    val outputCities = cities.map( city => city.zipWithIndex.map( convoy => {
      convoy._1.toList.map( p => { inputs( latency + convoy._2 - p.roadTripTime )._2( p.beachId ) }).reduce( _ + _ )
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

  def testSumSched(sumStructure : List[List[Set[(Int, Int, Int, Boolean)]]], outCyc : List[List[Int]]) : Unit = {

    val cities = ( sumStructure zip outCyc ).map( x => SumScheduler.cpToCity( x._1, x._2 ) )
    val inOut = generateInOut( cities )

    chiselMainTest( Array("--genHarness", "--compile", "--test",
      "--backend", "c", "--targetDir", dir.getPath.toString()),
      () => Module( new SumScheduler( 16, 8, sumStructure, outCyc ) )
    ) { c => new SumSchedulerTester(c, inOut._1, inOut._2) }

  }

  @Test def conv_5_3_SumRun {
    val convSums = getConvSums( 5, 3 ).map( s => s.map( p => ( p._1, p._2, 0, false ) ) )
    println( convSums )
    val outCyc = ( 0 until convSums.size ).map( _ + 10 ).toList
    testSumSched( List(convSums), List(outCyc) )
  }

  /*
  @Test def conv_5_3_SumDoubleRun {
    val rate = 2
    val convSums = getConvSums( 5, 3 ).map( s => s.map( p => ( p._1/rate, p._2 + (p._1 % rate)*9, 0, false ) ) )
    println( convSums )
    val outCyc = ( 0 until convSums.size ).map( x => (x/rate) + 10 ).toList
    testSumSched( List(convSums), List(outCyc) )
  }
 */

  /*
  @Test def clusterSplitTest {
    val convoys = List(Set(Person( 4, 3, 0, false )), Set(Person( 2, 5, 0, false )), Set(Person( 4, 3, 0, false ), Person( 2, 5, 0, false )))
    println( SumScheduler.clusterSplit( convoys ) )
  }
   */

/*
  @Test def smallFollowTest {
    val cp = List( Set( (0,0), (1,1) ), Set( (1,0), (2,1), (3,2) ), Set( (3,1), (4,2) ) )
    val cpMod = cp.map( c => c.map( p => ( p._1, p._2, 0, false ) ) )
    val outCyc = List( 5, 6, 7 )
    testSumSched( List(cpMod), List(outCyc) )
  }
  @Test def conv_64_3_SumRun {
    val convSums = getConvSums( 96, 3 ).map( s => s.map( p => ( p._1, p._2, 0, false ) ) )
    println( convSums )
    val outCyc = ( 0 until convSums.size ).map( _ + 154 ).toList
    testSumSched( List(convSums), List(outCyc) )
  }
 */

}
