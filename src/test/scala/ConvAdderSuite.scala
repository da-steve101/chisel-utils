import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import Chisel._
import chiselutils.math.ConvAdder

class ConvAdderSuite extends TestSuite {
  val myRand = new Random
  val fracWidth = 8

  def genMat( rows : Int, cols : Int ) : List[List[Int]] = {
    List.fill( rows ) { List.fill( cols ) { myRand.nextInt( 512 ) - 256 } }
  }

  def computeConv( filter : List[List[Int]], img : List[List[Int]] ) : List[List[Int]] = {
    // each individual number
    val convLocs = ( 0 until img.size ).map( rowIdx => {
      ( 0 until img(rowIdx).size ).map( colIdx => {
        ( 0 until filter.size ).map( fRow => {
          ( 0 until filter(fRow).size ).map( fCol => {
            val imgRow = rowIdx + fRow - 1
            val imgCol = colIdx + fCol - 1
            if ( imgRow >= 0 && imgRow < img.size && imgCol >= 0 && imgCol < img( imgRow ).size )
              filter( fRow )( fCol )*img( imgRow )( imgCol ) >> fracWidth
            else
              0
          }).toList
        }).toList
      }).toList
    }).toList

    val convRes = convLocs.map( a => a.map( b => b.map( _.reduce( _ + _ )).reduce( _ + _ ) ) )
    // val rowSums = convLocs.map( a => a.map( b => b.map( _.reduce( _ + _ )) ) )

    convRes
  }

  class ConvAdderTester( c : ConvAdder, filter : List[List[Int]],
    img : List[List[Int]] ) extends Tester( c ) {
    val imgSize = ( img.size, img(0).size )
    val filterSize = ( filter.size, filter(0).size, 1 )
    val noImg = 1
    val noPixel = noImg * imgSize._1 * imgSize._2

    val convRes = computeConv( filter, img )

    poke( c.io.validIn, true )

    for ( pIdx <- 0 until noPixel ) {
      val imgRow = ( pIdx/imgSize._2 ) % imgSize._1
      val imgCol = pIdx % imgSize._2

      val multIn = filter.map( fRow => fRow.map( f => {
        f*img( imgRow )( imgCol ) >> fracWidth
      })).reduce( _ ++ _ ).map( x => Array( BigInt(x) ) ).toArray

      for ( idx <- 0 until filterSize._1*filterSize._2 )
        poke( c.io.multIn(idx), multIn(idx) )
      step(1)

      if ( c.latency <= pIdx ) {
        val convRow = ( (pIdx - c.latency)/imgSize._2 ) % imgSize._1
        val convCol = ( pIdx - c.latency ) % imgSize._2
        expect( c.io.convOut, Array( BigInt(convRes( convRow )( convCol )) ) )
      }
    }
    for ( pIdx <- ( noPixel - c.latency) until noPixel ) {
      val convRow = ( pIdx/imgSize._2 ) % imgSize._1
      val convCol = pIdx % imgSize._2
      step(1)
      expect( c.io.convOut, Array( BigInt(convRes( convRow )( convCol )) ) )
    }
  }


  @Test def conv_3x3_5x5_Test {
    for ( filterLen <- List(5) ) {
      for ( imgLen <- List(7) ) {
        val imgSize = ( imgLen, imgLen )
        val filterSize = ( filterLen, filterLen, 1 )
        val img = genMat( imgSize._1, imgSize._2 )
        val filter = genMat( filterSize._1, filterSize._2 )

        println( "img = " + img )
        println( "filter = " + filter )

        chiselMainTest( Array("--genHarness", "--compile", "--test", "--vcd", "--backend", "c"),
          () => Module( new ConvAdder( filterSize, imgSize ) ) ) { c => new ConvAdderTester( c, filter, img ) }
      }
    }
  }

}

