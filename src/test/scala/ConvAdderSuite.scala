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

  /** Generate a random matrix
    */
  def genMat( rows : Int, cols : Int ) : List[List[Int]] = {
    List.fill( rows ) { List.fill( cols ) { myRand.nextInt( 512 ) - 256 } }
  }

  /** Compute the convolution with a filter and image
    */
  def computeConv( filter : List[List[Int]], img : List[List[Int]] ) : List[List[Int]] = {
    // each individual number
    val convLocs = ( 0 until img.size ).map( rowIdx => {
      ( 0 until img(rowIdx).size ).map( colIdx => {
        ( 0 until filter.size ).map( fRow => {
          ( 0 until filter(fRow).size ).map( fCol => {
            val imgRow = rowIdx + fRow - ( filter.size / 2 )
            val imgCol = colIdx + fCol - ( filter( 0 ).size / 2 )
            if ( imgRow >= 0 && imgRow < img.size && imgCol >= 0 && imgCol < img( imgRow ).size )
              filter( fRow )( fCol )*img( imgRow )( imgCol ) >> fracWidth
            else
              0
          }).toList
        }).toList
      }).toList
    }).toList

    val convRes = convLocs.map( a => a.map( b => b.map( _.reduce( _ + _ )).reduce( _ + _ ) ) )

    convRes
  }

  class ConvAdderTester( c : ConvAdder, filterSize : ( Int, Int, Int ),
    imgSize : ( Int, Int ) ) extends Tester( c ) {

    val noImg = 2
    val imgPixels = imgSize._1 * imgSize._2
    val filterPixels = filterSize._1*filterSize._2
    val noPixel = noImg * imgPixels

    val filters = ( 0 until filterSize._3 ).map( x => genMat( filterSize._1, filterSize._2 ) )
    println( "filters = " + filters )

    val imgs = ( 0 until noImg ).map( x => genMat( imgSize._1, imgSize._2 ) ).toList
    println( "imgs = " + imgs )

    val convResAry = imgs.map( img => filters.map( filter => computeConv( filter, img ) ) )

    println( "convResAry = " + convResAry )
    println( "latency = " + c.latency )

    poke( c.io.validIn, true )

    for ( pIdx <- 0 until noPixel ) {
      val img = imgs( pIdx / imgPixels )

      val imgRow = ( pIdx/imgSize._2 ) % imgSize._1
      val imgCol = pIdx % imgSize._2

      val filterInputs = filters.map( filter => {
        filter.map( fRow => {
          fRow.map( f => {
            f*img( imgRow )( imgCol ) >> fracWidth
          })
        }).reduce( _ ++ _ ).map( x => Array( BigInt(x) ) )
      })

      val multIn = ( 0 until filterPixels ).map( idx => filterInputs.map( f => f(idx) ).reduce( _ ++ _ ) )

      for ( idx <- 0 until filterPixels )
        poke( c.io.multIn(idx), multIn(idx) )

      if ( c.latency <= pIdx ) {
        val imgIdx = ( pIdx - c.latency ) / imgPixels
        val convRes = convResAry( imgIdx )
        val convRow = ( (pIdx - c.latency)/imgSize._2 ) % imgSize._1
        val convCol = ( pIdx - c.latency ) % imgSize._2
        println( "test result at ( " + imgIdx + ", " + convRow + ", " + convCol + " )" )
        expect( c.io.convOut, ( 0 until filterSize._3 ).map( fIdx => BigInt( convRes( fIdx )( convRow )( convCol ) ) ).toArray )
      }
      step(1)
    }
    for ( pIdx <- ( noPixel - c.latency ) until noPixel ) {
      if ( pIdx >= 0 ) {
        val imgIdx = pIdx / imgPixels
        val convRes = convResAry( imgIdx )
        val convRow = ( pIdx/imgSize._2 ) % imgSize._1
        val convCol = pIdx % imgSize._2
        println( "test result at ( " + imgIdx + ", " + convRow + ", " + convCol + " )" )
        expect( c.io.convOut, ( 0 until filterSize._3 ).map( fIdx => BigInt( convRes( fIdx )( convRow )( convCol ) ) ).toArray )
      }
      step(1)
    }
  }


  @Test def convTest {
    for ( filterLen <- List(3, 5, 7, 9) ) {
      for ( imgLen <- ( filterLen + 2 until 21 by 2 ) ) {
        val imgSize = ( imgLen, imgLen )
        val filterSize = ( filterLen, filterLen, 15 )

        chiselMainTest( Array("--genHarness", "--compile", "--test", "--vcd", "--backend", "c"),
          () => Module( new ConvAdder( filterSize, imgSize ) ) ) { c => new ConvAdderTester( c, filterSize, imgSize ) }
      }
    }
  }

}

