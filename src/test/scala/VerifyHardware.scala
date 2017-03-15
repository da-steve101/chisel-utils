import chiselutils.algorithms.AnnealingSolver
import chiselutils.algorithms.Node
import scala.util.Random
import Chisel._

private class MyMod( val myNodes : Set[chiselutils.algorithms.Node], val outNodes : Seq[chiselutils.algorithms.Node] ) extends Module {
  val inNodes = myNodes.filter( _.isC() ).toVector.sortBy( n => n.uk(0).toList(0)(1) ) // have to put in order
  Predef.assert( inNodes.size == ( inNodes.last.uk(0).map( x => x(1) ).max + 1 ), "Should be correct number of inNodes" )
  val io = new Bundle {
    val in = Valid( Vec( inNodes.size, Fixed( INPUT, 16, 8 ) ) ).asInput
    val out = Valid( Vec( outNodes.size, Fixed( OUTPUT, 16, 8 ) ) )
  }
  val nodeInputs = io.in.bits.zip( inNodes ).toSet
  io.out.valid := AnnealingSolver.toChisel( myNodes, nodeInputs, io.in.valid )
  for ( n <- outNodes.zipWithIndex )
    io.out.bits(n._2) := n._1.getChisel().get
}

private class MyModTests( c : MyMod ) extends Tester( c ) {
  val myRand = new Random
  val maxVal = 10
  val numIn = c.io.in.bits.size
  val numOut = c.io.out.bits.size
  val latency = c.myNodes.map( n => Node.latency( n ) ).max
  val cycs = math.max( c.myNodes.iterator.next.ck.size, latency ) * 2
  val testInputs = ( 0 until cycs ).toSeq.map( c =>
    ( 0 until numIn ).toSeq.map( ni => BigInt( myRand.nextInt( maxVal ) ) )
  )
  poke( c.io.in.valid, true )
  for ( cyc <- 0 until cycs ) {
    for ( i <- 0 until numIn )
      poke( c.io.in.bits( i ), testInputs( cyc )( i ) )
    val validOut = peek( c.io.out.valid ) == 1
    peek( c.io.out )
    for ( n <- c.myNodes )
      peek( n.getChisel().get )
    if ( validOut ) {
      for ( n <- c.outNodes.zipWithIndex ) {
        val node = n._1
        val currCyc = ( cyc - 1 ) % node.ck.size
        val currOut = node.ck( currCyc )
        if ( currOut != -1 ) {
          val uki = node.uk( currOut )
          val rdy = uki.map( num => cyc - num( 0 ) >= 0 ).reduce( _ && _ )
          if ( rdy ) {
            val outNums = uki.toVector.map( num => testInputs( cyc - num( 0 ) )( num( 1 ) ) )
            expect( c.io.out.bits( n._2 ), outNums.sum )
          }
        }
      }
    }
    step( 1 )
  }
}

object VerifyHardware {

  def testLinks( nodes : Set[Node] ) : Boolean = {
    for ( n <- nodes ) {
      for ( c <- n.getChildren() ) {
        if ( !nodes.contains( c ) ) {
          println( c + " not in nodes" )
          return false
        }
        if ( !c.getParents().contains( n ) ) {
          println( "Parents doesn't have :" + n  )
          return false
        }
        for ( p <- c.getParents() ) {
          if ( !nodes.contains( p ) ) {
            println( "Parent node not in nodes:" + p )
            return false
          }
        }
      }
    }
    true
  }

  def getConvSums( imgSize : Int, filterSize : Int ) : List[Set[Vector[Int]]] = {
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
          Vector( ypy*imgSize + xpx, p )
        }}.toSet
      })
    }).reduce( _ ++ _ ).toList
    outSums
  }

  def apply( nodes : Set[Node], outNodes : Seq[Node] ) {
    assert( testLinks( nodes ) )
    for ( n <- nodes )
      assert( Node.satisfiesConstraints(n), "Nodes must satisfy constraints" )
    for ( n <- nodes )
      assert( Node.isMinimal( n ), "Node must be minimal: " + n )
    chiselMainTest( Array("--genHarness", "--compile", "--test", "--backend", "c"),
      () => Module( new MyMod( nodes, outNodes ) ) ) { c => new MyModTests( c ) }
  }

  def generateHardware( nodes : Set[Node], outNodes : Seq[Node] ) {
    assert( testLinks( nodes ) )
    chiselMain( Array("--genHarness", "--backend", "v"), () => Module( new MyMod( nodes, outNodes ) ) )
  }

}
