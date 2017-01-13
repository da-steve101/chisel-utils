import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import chiselutils.algorithms.Node
import chiselutils.algorithms.Transforms
import chiselutils.algorithms.AnnealingSolver
import com.github.tototoshi.csv._
import java.io.File
import Chisel._


class SumScheduleSuite extends TestSuite {

  val myRand = new Random
  val dim = 2
  val nodeSize = 20
  val maxVal = 10

  class MyMod( val myNodes : Set[chiselutils.algorithms.Node], val outNodes : Seq[chiselutils.algorithms.Node] ) extends Module {
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

  class MyModTests( c : MyMod ) extends Tester( c ) {
    val numIn = c.io.in.bits.size
    val numOut = c.io.out.bits.size
    val latency = c.myNodes.map( n => Node.latency( n ) ).max
    val cycs = c.myNodes.iterator.next.ck.size * 2
    val testInputs = ( 0 until cycs ).toSeq.map( c =>
      ( 0 until numIn ).toSeq.map( ni => BigInt( myRand.nextInt( maxVal ) ) )
    )
    poke( c.io.in.valid, true )
    for ( cyc <- 0 until cycs ) {
      for ( i <- 0 until numIn )
        poke( c.io.in.bits( i ), testInputs( cyc )( i ) )
      val validOut = peek( c.io.out.valid ) == 1
      peek( c.io.out )
      //for ( n <- c.myNodes )
      //  peek( n.getChisel().get )
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

  def genUk( noUk : Int, setSize : Int ) : Seq[Set[Seq[Int]]] = {
    Vector.fill( noUk ) { List.fill( setSize ) { List.fill( dim ) { myRand.nextInt( maxVal ) + 1 }.to[Seq]}.to[Set] }
  }

  def genTermUk( x : Int  ) : Seq[Set[Seq[Int]]] = {
    Vector(  List( (List( 0 ) ++ List.fill( dim - 1 ) { x }).to[Seq] ).to[Set] )
  }

  def genCk( noUk : Int ) : Seq[Int] = {
    Vector.fill( nodeSize ) { myRand.nextInt( noUk + 1 ) - 1 }
  }

  def decr( uk : Seq[Set[Seq[Int]]] ) : Seq[Set[Seq[Int]]] = {
    uk.map( s => s.map( v => { Vector( v(0) - 1 ) ++ v.drop(1) }.to[Seq]))
  }

  def incr( uk : Seq[Set[Seq[Int]]] ) : Seq[Set[Seq[Int]]] = {
    uk.map( s => s.map( v => { Vector( v(0) + 1 ) ++ v.drop(1) }.to[Seq]))
  }

  def testLinks( nodes : Set[Node] ) : Boolean = {
    for ( n <- nodes ) {
      if ( n.getL().isDefined ) {
        if ( !nodes.contains( n.getL().get ) ) {
          println( n.getL().get + " not in nodes (L) " )
          return false
        }
        if ( !n.getL().get.getParents().contains( n ) ) {
          println( "Parents doesn't have n (L)" )
          return false
        }
        for ( p <- n.getL().get.getParents() ) {
          if ( !nodes.contains( p ) ) {
            println( "Parent node not in nodes (L)" )
            return false
          }
        }
      }
      if ( n.getR().isDefined ) {
        if ( !nodes.contains( n.getR().get ) ) {
          println( n.getR().get + " not in nodes (R) " )
          return false
        }
        if ( !n.getR().get.getParents().contains( n ) ) {
          println( "Parents doesn't have n (R)" )
          return false
        }
        for ( p <- n.getR().get.getParents() ) {
          if ( !nodes.contains( p ) ) {
            println( "Parent node not in nodes (R)" )
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

  def verifyHardware( nodes : Set[Node], outNodes : Seq[Node] ) {
    chiselMainTest( Array("--genHarness", "--compile", "--test", "--vcd", "--backend", "c"),
      () => Module( new MyMod( nodes, outNodes ) ) ) { c => new MyModTests( c ) }
    // chiselMain( Array("--genHarness", "--backend", "v"), () => Module( new MyMod( nodes, outNodes ) ) )
  }

  /** Test the sum constraint
    */
  @Test def testConstraintA {
    val nodeAuK = genUk( 5, 5 )
    val nodeAcK = genCk( 5 )
    val nodeA = Node( nodeAuK, nodeAcK )
    val nodeBCuK = nodeAuK.map( uki => {
      val listB = ArrayBuffer[Seq[Int]]()
      val listC = ArrayBuffer[Seq[Int]]()
      for ( s <- uki ) {
        if ( listB.isEmpty || ( myRand.nextInt( 2 ) == 0 && !listC.isEmpty )  )
          listB += s
        else
          listC += s
      }
      ( listB.toSet, listC.toSet )
    })
    val nodeB = Node( decr(nodeBCuK.map( _._1 )), nodeAcK.drop(1) ++ nodeAcK.take(1) )
    val nodeC = Node( decr(nodeBCuK.map( _._2 )), nodeAcK.drop(1) ++ nodeAcK.take(1) )
    nodeA.setL( Some(nodeB) )
    nodeA.setR( Some(nodeC) )
    assert( Node.satisfiesConstraintA( nodeA ) )
  }

  /** Test the mux constraint
    * NB: this test will fail if you get unlucky and all mux on oneside (other is empty)
    */
  @Test def testConstraintB {
    val nodeAuK = genUk( 5, 5 )
    val nodeAcK = genCk( 5 )
    val nodeA = Node( nodeAuK, nodeAcK )
    val muxSwitch = nodeAuK.map( uki => myRand.nextInt( 2 ) )
    val shiftCk = nodeAcK.drop(1) ++ nodeAcK.take(1)
    val lIdxMap = muxSwitch.scanLeft(0)( _ + _ )
    val rIdxMap = muxSwitch.map( 1 - _ ).scanLeft(0)( _ + _ )
    val lCk = shiftCk.map( cki => {
      if ( cki == -1 || muxSwitch( cki ) == 0 )
        -1
      else
        lIdxMap( cki )
    })
    val rCk = shiftCk.map( cki => {
      if ( cki == -1 || muxSwitch( cki ) == 1 )
        -1
      else
        rIdxMap( cki )
    })

    val nodeBuK = decr(nodeAuK.zip( muxSwitch ).filter( _._2 == 1 ).map( _._1 ))
    val nodeCuK = decr(nodeAuK.zip( muxSwitch ).filter( _._2 == 0 ).map( _._1 ))
    val nodeB = {
      if ( nodeBuK.size != 0 )
        Node( nodeBuK, lCk )
      else
        Node( nodeCuK, rCk )
    }
    val nodeC = {
      if ( nodeBuK.size == 0 || nodeCuK.size == 0 )
        nodeB
      else
        Node( nodeCuK, rCk )
    }
    nodeA.setL( Some(nodeB) )
    nodeA.setR( Some(nodeC) )
    assert( Node.satisfiesConstraintB( nodeA ) )
  }

  @Test def testConstraintC {
    val nodeAuK = genTermUk( 1 )
    val nodeAcK = genCk( 1 )
    val nodeA = Node( nodeAuK, nodeAcK )
    assert( Node.satisfiesConstraintC( nodeA ) )
  }

  @Test def testSwap1 {
    val nodeAuK = genTermUk( 0 ) // termination setA
    val nodeBuK = genTermUk( 1 ) // termination setB
    val node_cK = genCk( 1 )
    val nodeA = Node( nodeAuK, node_cK )
    nodeA.setC()
    val nodeB = Node( nodeBuK, node_cK )
    nodeB.setC()
    val nodeSwapUk = incr( nodeAuK.zip( nodeBuK ).map( z => z._1 ++ z._2 ) )
    val nodeSwapCk = node_cK.takeRight( 1 ) ++ node_cK.dropRight( 1 )
    val nodeSwap = Node( nodeSwapUk, nodeSwapCk )
    nodeSwap.setA()
    nodeSwap.setL( Some(nodeA) )
    nodeSwap.setR( Some(nodeB) )
    val nodePar = Node( nodeSwap.getUkNext(), nodeSwap.getCkNext() )
    nodePar.setB()
    nodePar.setL( Some(nodeSwap) )
    nodePar.setR( Some(nodeSwap) )
    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintA( nodeSwap ) )
    assert( Node.satisfiesConstraintB( nodePar ) )

    val nodeList = Transforms.trySwap( nodePar, nodeSwap )._1

    assert( Node.satisfiesConstraintA( nodeList(0) ) && nodeList(0).isA() )
    assert( Node.satisfiesConstraintB( nodeList(1) ) && nodeList(1).isB() )
    assert( Node.satisfiesConstraintB( nodeList(2) ) && nodeList(2).isB() )
  }

  @Test def testSwap2 {
    val nodeAuK = genTermUk( 0 ) // termination setA
    val nodeBuK = genTermUk( 1 ) // termination setB
    val node_cK = genCk( 2 )
    val nodeA = Node( nodeAuK, node_cK.map( x => if ( x == 0 ) 0 else -1 ) )
    nodeA.setC()
    val nodeB = Node( nodeBuK, node_cK.map( x => if ( x == 1 ) 0 else -1 ) )
    nodeB.setC()
    val nodeSwapUk = incr( nodeAuK ++ nodeBuK )
    val nodeSwapCk = node_cK.takeRight( 1 ) ++ node_cK.dropRight( 1 )
    val nodeSwap = Node( nodeSwapUk, nodeSwapCk )
    nodeSwap.setB()
    nodeSwap.setL( Some(nodeA) )
    nodeSwap.setR( Some(nodeB) )
    val nodeParUk = incr( nodeSwapUk )
    val nodeParCk = nodeSwapCk.takeRight( 1 ) ++ nodeSwapCk.dropRight( 1 )
    val nodePar = Node( nodeParUk, nodeParCk )
    nodePar.setB()
    nodePar.setL( Some(nodeSwap) )
    nodePar.setR( Some(nodeSwap) )
    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintB( nodeSwap ) )
    assert( Node.satisfiesConstraintB( nodePar ) )

    val nodeList = Transforms.trySwap( nodePar, nodeSwap )._1

    assert( Node.satisfiesConstraintB( nodeList(0) ) && nodeList(0).isB() )
    assert( Node.satisfiesConstraintB( nodeList(1) ) && nodeList(1).isB() )
    assert( Node.satisfiesConstraintB( nodeList(2) ) && nodeList(2).isB() )
  }

  @Test def testSwap4 {
    val nodeAuK = genTermUk( 0 ) // termination setA
    val nodeBuK = genTermUk( 1 ) // termination setB
    val nodeCuK = genTermUk( 2 ) // termination setC
    val node_cK = genCk( 1 )
    val nodeA = Node( nodeAuK, node_cK )
    val nodeB = Node( nodeBuK, node_cK )
    val nodeC = Node( nodeCuK, node_cK )
    nodeA.setC()
    nodeB.setC()
    nodeC.setC()
    val nodeSwap = Node( nodeA.getUkNext(), nodeA.getCkNext() )
    nodeSwap.setB()
    val nodeOtherUk = incr( nodeBuK.zip( nodeCuK ).map( z => z._1 ++ z._2 ) )
    val nodeOtherCk = nodeB.getCkNext()
    val nodeOther = Node( nodeOtherUk, nodeOtherCk )
    nodeOther.setA()

    nodeSwap.setL( Some(nodeA) )
    nodeSwap.setR( Some(nodeA) )
    nodeOther.setL( Some(nodeB) )
    nodeOther.setR( Some(nodeC) )

    val nodeParUk = incr( nodeSwap.uk.zip( nodeOther.uk ).map( z => z._1 ++ z._2 ) )
    val nodeParCk = nodeSwap.getCkNext()
    val nodePar = Node( nodeParUk, nodeParCk )
    nodePar.setA()
    nodePar.setL( Some(nodeSwap) )
    nodePar.setR( Some(nodeOther) )

    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintC( nodeC ) )
    assert( Node.satisfiesConstraintB( nodeSwap ) )
    assert( Node.satisfiesConstraintA( nodeOther ) )
    assert( Node.satisfiesConstraintA( nodePar ) )

    val nodeList = Transforms.trySwap( nodePar, nodeSwap )._1

    assert( Node.satisfiesConstraintA( nodeList(0) ) && nodeList(0).isA() )
    assert( Node.satisfiesConstraintB( nodeList(1) ) && nodeList(1).isB() )
    assert( Node.satisfiesConstraintA( nodeList(2) ) && nodeList(2).isA() )
  }

  @Test def testSwap5 {
    val nodeAuK = genTermUk( 0 ) // termination setA
    val nodeBuK = genTermUk( 1 ) // termination setB
    val nodeCuK = genTermUk( 2 ) // termination setC
    val nodeDuK = genTermUk( 3 ) // termination setC
    val nodeParCk = genCk( 1 )
    val node_cK = nodeParCk.drop(2) ++ nodeParCk.take(2)
    val node_cK1 = nodeParCk.drop(1) ++ nodeParCk.take(1)
    val nodeA = Node( nodeAuK, node_cK )
    val nodeB = Node( nodeBuK, node_cK )
    val nodeC = Node( nodeCuK, node_cK )
    val nodeD = Node( nodeDuK, node_cK )
    nodeA.setC()
    nodeB.setC()
    nodeC.setC()
    nodeD.setC()
    val nodeSwap = Node( nodeA.getUkNext().map( z => nodeB.getUkNext().head ++ z ), node_cK1 )
    val nodeOther = Node( nodeC.getUkNext().map( z => nodeD.getUkNext().head ++ z ), node_cK1 )
    nodeSwap.setA()
    nodeSwap.setL( Some(nodeA) )
    nodeSwap.setR( Some(nodeB) )
    nodeOther.setL( Some(nodeC) )
    nodeOther.setR( Some(nodeD) )
    nodeOther.setA()

    val nodeParUk = nodeSwap.getUkNext().map( z => nodeOther.getUkNext().head ++ z )
    val nodePar = Node( nodeParUk, nodeParCk )
    nodePar.setA()
    nodePar.setL( Some(nodeSwap) )
    nodePar.setR( Some(nodeOther) )

    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintC( nodeC ) )
    assert( Node.satisfiesConstraintC( nodeD ) )
    assert( Node.satisfiesConstraintA( nodeSwap ) )
    assert( Node.satisfiesConstraintA( nodeOther ) )
    assert( Node.satisfiesConstraintA( nodePar ) )

    val nodeList = Transforms.trySwap( nodePar, nodeSwap )._1

    assert( Node.satisfiesConstraintA( nodeList(0) ) && nodeList(0).isA() )
    assert( Node.satisfiesConstraintA( nodeList(1) ) && nodeList(1).isA() )
    assert( Node.satisfiesConstraintA( nodeList(2) ) && nodeList(2).isA() )
  }

  @Test def testSwap6 {
    val nodeAuK = genTermUk( 0 ) // termination setA
    val nodeBuK = genTermUk( 1 ) // termination setB
    val node_cK = genCk( 1 )
    val nodeA = Node( nodeAuK, node_cK )
    val nodeB = Node( nodeBuK, node_cK )
    nodeA.setC()
    nodeB.setC()
    val nodeSwap = Node( nodeA.getUkNext(), nodeA.getCkNext() )
    val nodeOther = Node( nodeB.getUkNext(), nodeB.getCkNext() )
    nodeSwap.setB()
    nodeOther.setB()
    nodeSwap.setL( Some(nodeA) )
    nodeSwap.setR( Some(nodeA) )
    nodeOther.setL( Some(nodeB) )
    nodeOther.setR( Some(nodeB) )

    val nodeParUk = incr( nodeSwap.uk.zip( nodeOther.uk ).map( z => z._1 ++ z._2 ) )
    val nodeParCk = nodeSwap.getCkNext()
    val nodePar = Node( nodeParUk, nodeParCk )
    nodePar.setA()
    nodePar.setL( Some(nodeSwap) )
    nodePar.setR( Some(nodeOther) )

    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintB( nodeSwap ) )
    assert( Node.satisfiesConstraintB( nodeOther ) )
    assert( Node.satisfiesConstraintA( nodePar ) )

    val nodeList = Transforms.trySwap( nodePar, nodeSwap )._1

    assert( Node.satisfiesConstraintB( nodeList(0) ) && nodeList(0).isB() )
    assert( Node.satisfiesConstraintA( nodeList(1) ) && nodeList(1).isA() )
  }

  @Test def testSwap7 {
    val nodeAuK = genTermUk( 0 ) // termination setA
    val nodeBuK = genTermUk( 1 ) // termination setB
    val nodeCuK = genTermUk( 2 ) // termination setC
    val node_cK = genCk( 1 )
    val nodeA = Node( nodeAuK, node_cK )
    val nodeB = Node( nodeBuK, node_cK )
    val nodeC = Node( nodeCuK, node_cK )
    nodeA.setC()
    nodeB.setC()
    nodeC.setC()
    val nodeOtherUk = nodeB.getUkNext() ++ nodeC.getUkNext()
    val nodeSwap = Node( nodeA.getUkNext(), nodeA.getCkNext() )
    val nodeOther = Node( nodeOtherUk, nodeB.getCkNext().map( x => if ( x == -1 ) -1 else myRand.nextInt(2) ))
    nodeSwap.setB()
    nodeOther.setB()
    nodeSwap.setL( Some(nodeA) )
    nodeSwap.setR( Some(nodeA) )
    nodeOther.setL( Some(nodeB) )
    nodeOther.setR( Some(nodeC) )

    val nodeParUk = incr( nodeOther.uk.map( z => nodeSwap.uk.head ++ z ) )
    val nodeParCk = nodeOther.getCkNext()
    val nodePar = Node( nodeParUk, nodeParCk )
    nodePar.setA()
    nodePar.setL( Some(nodeSwap) )
    nodePar.setR( Some(nodeOther) )

    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintC( nodeC ) )
    assert( Node.satisfiesConstraintB( nodeSwap ) )
    assert( Node.satisfiesConstraintB( nodeOther ) )
    assert( Node.satisfiesConstraintA( nodePar ) )

    val nodeList = Transforms.trySwap( nodePar, nodeSwap )._1

    assert( Node.satisfiesConstraintB( nodeList(0) ) && nodeList(0).isB() )
    assert( Node.satisfiesConstraintA( nodeList(1) ) && nodeList(1).isA() )
    assert( Node.satisfiesConstraintA( nodeList(2) ) && nodeList(2).isA() )
  }

  @Test def testSwap10 {
    val nodeAuK = genTermUk( 0 ) // termination setA
    val nodeBuK = genTermUk( 1 ) // termination setB
    val nodeParCk = genCk( 2 )
    val node_cK = nodeParCk.drop(2) ++ nodeParCk.take(2)
    val nodeA = Node( nodeAuK, node_cK.map( x => if ( x == 0 ) 0 else -1 ) )
    val nodeB = Node( nodeBuK, node_cK.map( x => if ( x == 1 ) 0 else -1 ) )
    nodeA.setC()
    nodeB.setC()
    val nodeSwap = Node( nodeA.getUkNext(), nodeA.getCkNext() )
    val nodeOther = Node( nodeB.getUkNext(), nodeB.getCkNext() )
    nodeSwap.setB()
    nodeSwap.setL( Some(nodeA) )
    nodeSwap.setR( Some(nodeA) )
    nodeOther.setB()
    nodeOther.setL( Some(nodeB) )
    nodeOther.setR( Some(nodeB) )

    val nodeParUk = nodeSwap.getUkNext() ++ nodeOther.getUkNext()
    val nodePar = Node( nodeParUk, nodeParCk )
    nodePar.setB()
    nodePar.setL( Some(nodeSwap) )
    nodePar.setR( Some(nodeOther) )

    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintB( nodeSwap ) )
    assert( Node.satisfiesConstraintB( nodeOther ) )
    assert( Node.satisfiesConstraintB( nodePar ) )

    val nodeList = Transforms.trySwap( nodePar, nodeSwap )._1

    assert( Node.satisfiesConstraintB( nodeList(0) ) && nodeList(0).isB() )
    assert( Node.satisfiesConstraintB( nodeList(1) ) && nodeList(1).isB() )
  }

  @Test def testSwap11 {
    val nodeAuK = genTermUk( 0 ) // termination setA
    val nodeBuK = genTermUk( 1 ) // termination setB
    val nodeCuK = genTermUk( 2 ) // termination setC
    val nodeParCk = genCk( 2 )
    val node_cK = nodeParCk.drop(2) ++ nodeParCk.take(2)
    val nodeA = Node( nodeAuK, node_cK.map( x => if ( x == 0 ) 0 else -1 ) )
    val nodeB = Node( nodeBuK, node_cK.map( x => if ( x != -1 ) 0 else -1 ) )
    val nodeC = Node( nodeCuK, node_cK.map( x => if ( x == 1 ) 0 else -1 ) )
    nodeA.setC()
    nodeB.setC()
    nodeC.setC()
    val nodeSwap = Node( nodeA.getUkNext().map( z => nodeB.getUkNext().head ++ z ), nodeA.getCkNext() )
    val nodeOther = Node( nodeC.getUkNext().map( z => nodeB.getUkNext().head ++ z ), nodeC.getCkNext() )
    nodeSwap.setA()
    nodeSwap.setL( Some(nodeA) )
    nodeSwap.setR( Some(nodeB) )
    nodeOther.setA()
    nodeOther.setL( Some(nodeB) )
    nodeOther.setR( Some(nodeC) )

    val nodeParUk = nodeSwap.getUkNext() ++ nodeOther.getUkNext()
    val nodePar = Node( nodeParUk, nodeParCk )
    nodePar.setB()
    nodePar.setL( Some(nodeSwap) )
    nodePar.setR( Some(nodeOther) )

    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintC( nodeC ) )
    assert( Node.satisfiesConstraintA( nodeSwap ) )
    assert( Node.satisfiesConstraintA( nodeOther ) )
    assert( Node.satisfiesConstraintB( nodePar ) )

    val nodeList = Transforms.trySwap( nodePar, nodeSwap )._1

    assert( Node.satisfiesConstraintA( nodeList(0) ) && nodeList(0).isA() )
    assert( Node.satisfiesConstraintB( nodeList(1) ) && nodeList(1).isB() )
    assert( Node.satisfiesConstraintB( nodeList(2) ) && nodeList(2).isB() )
  }

  @Test def testSwap12 {
    val nodeAuK = genTermUk( 0 ) // termination setA
    val nodeBuK = genTermUk( 1 ) // termination setB
    val nodeCuK = genTermUk( 2 ) // termination setC
    val nodeDuK = genTermUk( 3 ) // termination setC
    val nodeParCk = genCk( 4 )
    val node_cK = nodeParCk.drop(2) ++ nodeParCk.take(2)
    val node_cK1 = nodeParCk.drop(1) ++ nodeParCk.take(1)
    val nodeA = Node( nodeAuK, node_cK.map( x => if ( x == 0 ) 0 else -1 ) )
    val nodeB = Node( nodeBuK, node_cK.map( x => if ( x == 1 ) 0 else -1 ) )
    val nodeC = Node( nodeCuK, node_cK.map( x => if ( x == 2 ) 0 else -1 ) )
    val nodeD = Node( nodeDuK, node_cK.map( x => if ( x == 3 ) 0 else -1 ) )
    nodeA.setC()
    nodeB.setC()
    nodeC.setC()
    nodeD.setC()
    val nodeSwap = Node( nodeA.getUkNext() ++ nodeB.getUkNext(), node_cK1.map( x => if ( x != -1 && x < 2 ) x else -1 ) )
    val nodeOther = Node( nodeC.getUkNext() ++ nodeD.getUkNext(), node_cK1.map( x => if ( x < 2 ) -1 else x - 2 ) )
    nodeSwap.setB()
    nodeSwap.setL( Some(nodeA) )
    nodeSwap.setR( Some(nodeB) )
    nodeOther.setB()
    nodeOther.setL( Some(nodeC) )
    nodeOther.setR( Some(nodeD) )

    val nodeParUk = nodeSwap.getUkNext() ++ nodeOther.getUkNext()
    val nodePar = Node( nodeParUk, nodeParCk )
    nodePar.setB()
    nodePar.setL( Some(nodeSwap) )
    nodePar.setR( Some(nodeOther) )

    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintC( nodeC ) )
    assert( Node.satisfiesConstraintC( nodeD ) )
    assert( Node.satisfiesConstraintB( nodeSwap ) )
    assert( Node.satisfiesConstraintB( nodeOther ) )
    assert( Node.satisfiesConstraintB( nodePar ) )

    val nodeList = Transforms.trySwap( nodePar, nodeSwap )._1

    assert( Node.satisfiesConstraintB( nodeList(0) ) && nodeList(0).isB() )
    assert( Node.satisfiesConstraintB( nodeList(1) ) && nodeList(1).isB() )
    assert( Node.satisfiesConstraintB( nodeList(2) ) && nodeList(2).isB() )
  }

  @Test def testSwap13 {
    val nodeAuK = genTermUk( 0 ) // termination setA
    val nodeBuK = genTermUk( 1 ) // termination setB
    val nodeCuK = genTermUk( 2 ) // termination setC
    val nodeParCk = genCk( 3 )
    val node_cK = nodeParCk.drop(2) ++ nodeParCk.take(2)
    val node_cK1 = nodeParCk.drop(1) ++ nodeParCk.take(1)
    val nodeA = Node( nodeAuK, node_cK.map( x => if ( x == 0 ) 0 else -1 ) )
    val nodeB = Node( nodeBuK, node_cK.map( x => if ( x == 1 ) 0 else -1 ) )
    val nodeC = Node( nodeCuK, node_cK.map( x => if ( x == 2 ) 0 else -1 ) )
    nodeA.setC()
    nodeB.setC()
    nodeC.setC()
    val nodeSwap = Node( nodeA.getUkNext(), node_cK1.map( x => if ( x == 0 ) 0 else -1 ) )
    val nodeOther = Node( nodeB.getUkNext() ++ nodeC.getUkNext(), node_cK1.map( x => if ( x < 1 ) -1 else x - 1 ) )
    nodeSwap.setB()
    nodeSwap.setL( Some(nodeA) )
    nodeSwap.setR( Some(nodeA) )
    nodeOther.setB()
    nodeOther.setL( Some(nodeB) )
    nodeOther.setR( Some(nodeC) )

    val nodeParUk = nodeSwap.getUkNext() ++ nodeOther.getUkNext()
    val nodeParCkComb = nodeSwap.getCkNext().zip( nodeOther.getCkNext() ).map( cks => {
      if ( cks._1 == -1 && cks._2 == -1 )
        -1
      else if ( cks._1 == -1 )
        cks._2 + 1
      else
        cks._1
    })
    val nodePar = Node( nodeParUk, nodeParCkComb )
    nodePar.setB()
    nodePar.setL( Some(nodeSwap) )
    nodePar.setR( Some(nodeOther) )

    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintC( nodeC ) )
    assert( Node.satisfiesConstraintB( nodeSwap ) )
    assert( Node.satisfiesConstraintB( nodeOther ) )
    assert( Node.satisfiesConstraintB( nodePar ) )

    val nodeList = Transforms.trySwap( nodePar, nodeSwap )._1

    assert( Node.satisfiesConstraintB( nodeList(0) ) && nodeList(0).isB() )
    assert( Node.satisfiesConstraintB( nodeList(1) ) && nodeList(1).isB() )
    assert( Node.satisfiesConstraintB( nodeList(2) ) && nodeList(2).isB() )
  }

  /* commented out as locking assertions fail for serial method
  @Test def simpleGraph {
    val cp = List(
      Set( Vector( 5, 0 ), Vector( 4, 1 ) ),
      Set( Vector( 5, 0 ), Vector( 4, 1 ), Vector( 3, 2 ) ),
      Set( Vector( 4, 1 ), Vector( 3, 2 ) )
    )
    var nodes = AnnealingSolver.init( List( cp ) )._1
    for ( i <- 0 until 100 ) {
      assert( testLinks( nodes ), "Node collection should be valid" )
      val dotFile = "nodeMap" + (i/10) + ".dot"
      println( "writing to " + "nodeMap" + (i/10) + ".dot")
      AnnealingSolver.toDot( nodes, dotFile )

      // verify all nodes
      for ( n <- nodes ) {
        val valid = Node.satisfiesConstraints(n)
        val lValid = { if ( n.getL().isDefined ) n.getL().get.hasParent(n) else true }
        val rValid = { if ( n.getR().isDefined ) n.getR().get.hasParent(n) else true }
        if ( !valid || !lValid || !rValid ) {
          println( "Node = " + n )
          println( "Node.getL() = " + n.getL() )
          println( "Node.getR() = " + n.getR() )
          if ( n.getL().isDefined )
            println( "Node.getL().parents = " + n.getL().get.getParents() )
          if ( n.getR().isDefined )
            println( "Node.getR().parents = " + n.getR().get.getParents() )
        }
        assert( valid && lValid && rValid, "Node " + n + " did not satisfy constraints" )
      }

      // overwrite each cycle to catch changes
      val applyIfIncrease = Random.nextDouble >= i.toDouble/100 
      nodes = AnnealingSolver.applyOperation( nodes, applyIfIncrease )
    }
  }

  @Test def conv3n5 {
    val imgSize = 5
    val filterSize = 3
    val cpCoords = getConvSums( imgSize, filterSize ).zipWithIndex.map( cSet => {
      cSet._1.map( v => Vector( cSet._2 - v(0)) ++ v.drop(1) )
    })
    val latAdd = AnnealingSolver.needLatency( List( cpCoords ) )
    println( "latAdd = " + latAdd )
    val cp = cpCoords.zipWithIndex.map( cSet => {
      cSet._1.map( v => { Vector( latAdd + v(0) ) ++ v.drop(1) })
    })
    var nodes = AnnealingSolver.init( List( cp ) )._1
    nodes = AnnealingSolver.run( nodes, 100000000 )
    assert( testLinks( nodes ), "Nodes must be connected properly" )
    for ( n <- nodes )
      assert( Node.satisfiesConstraints(n), "Nodes must satisfy constraints" )
    AnnealingSolver.toDot( nodes, "conv3n5.dot" )
    println( "cost = " + nodes.size )
  }
   */
  @Test def conv3n5Par {
    val imgSize = 5
    val filterSize = 3
    val cpCoords = getConvSums( imgSize, filterSize ).zipWithIndex.map( cSet => {
      cSet._1.map( v => { Vector( cSet._2 - v(0)) ++ v.drop(1) }.to[Seq] )
    }).toVector.to[Seq]
    val latAdd = AnnealingSolver.needLatency( Vector( cpCoords ).to[Seq] )
    println( "latAdd = " + latAdd )
    val cp = cpCoords.zipWithIndex.map( cSet => {
      cSet._1.map( v => { Vector( latAdd + v(0) ) ++ v.drop(1) }.to[Seq])
    })
    var nodes = AnnealingSolver.init( Vector( cp ).to[Seq] )._1
    nodes = AnnealingSolver.runPar( nodes, 100000000, 1000000 )
    assert( testLinks( nodes ), "Nodes must be connected properly" )
    for ( n <- nodes )
      assert( Node.satisfiesConstraints(n), "Nodes must satisfy constraints" )
    AnnealingSolver.toDot( nodes, "conv3n5.dot" )
    println( "cost = " + nodes.size )
    val outNodes = nodes.filter( _.parentsIsEmpty() ).toVector
    verifyHardware( nodes, outNodes )
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

  def genTrinary( filterSize : (Int, Int, Int, Int), imgSize : (Int, Int ), throughput : Int = 1 ) : Seq[Seq[Set[Seq[Int]]]] = {
    val convFilter = Vector.fill(filterSize._4) {
      ( 0 until filterSize._3).toVector.map( f3 => {
        ( 0 until filterSize._1).toVector.map( f1 => {
          ( 0 until filterSize._2).toVector.map( f2 => {
            val num = Random.nextInt(100)
            // cant deal with all 0 yet so just force not that case by putting 1 in mid
            val inMid = ( f1 == filterSize._1/2 && f2 == filterSize._2/2 && f3 == filterSize._3/2 )
            val trinary = {
              if ( num < 8  )
                1
              else if ( num < 16 )
                -1
              else
                0
            }
            trinary
          }).to[Seq]
        }).to[Seq]
      }).to[Seq]
    }.to[Seq]
    binFilterToCp( convFilter, imgSize, throughput )
  }

  @Test def trinaryLayer {
    val filterSize = ( 3, 3, 3, 128 )
    val imgSize = ( 32, 32 )
    val initNodes = AnnealingSolver.init( genTrinary( filterSize, imgSize, 2 ) )._1
    println( "created " + initNodes.size + " nodes")
    val nodes = AnnealingSolver.runPar( initNodes, 100000000, 1000000 )
    assert( testLinks( nodes ), "Nodes must be connected properly" )
    for ( n <- nodes )
      assert( Node.satisfiesConstraints(n), "Nodes must satisfy constraints" )
    AnnealingSolver.toDot( nodes, "trinary.dot" )
    println( "cost = " + nodes.size )
  }

  @Test def minimalTest {
    // Node@224625879(B)
    val node = Node( Seq(Set(Seq(11, 1), Seq(10, 1)), Set(Seq(11, 1)), Set(Seq(11, 1), Seq(6, 0))), Seq(-1, -1, -1, 2, -1, -1, -1, -1, 2, -1, -1, -1, -1, 2, -1, -1, -1, -1, 2, 0, 0, 0, 0, 1, -1) )
    node.setB()
    // Node@758935928(B)
    val nSwap = Node( Seq(Set(Seq(10, 1)), Set(Seq(10, 1), Seq(5, 0))), Seq(-1, -1, 1, -1, -1, -1, -1, 1, -1, -1, -1, -1, 1, -1, -1, -1, -1, 1, -1, -1, -1, -1, 0, -1, -1) )
    nSwap.setB()
    // Node@209082060(B)
    val nOther = Node( Seq(Set(Seq(10, 1), Seq(9, 1))), Seq(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, -1, -1, -1) )
    nOther.setB()
    // Node@1750422405(B)
    val nodePar = Node( Seq(Set(Seq(12, 1), Seq(7, 0), Seq(11, 1), Seq(6, 1)), Set(Seq(12, 1), Seq(7, 0)), Set(Seq(12, 1), Seq(11, 1)), Set(Seq(12, 1))), Seq(0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 2, 2, 2, 2, 3) )
    nodePar.setB()

    val nSwapPar = Seq( node )
    val nOtherPar = Seq( node )
    val nodeL = nOther
    val nodeR = nSwap
    // Node@1581619835(B)
    val nSwapL = Node( Seq(Set(Seq(9, 1), Seq(4, 0))), Seq(-1, 0, -1, -1, -1, -1, 0, -1, -1, -1, -1, 0, -1, -1, -1, -1, 0, -1, -1, -1, -1, -1, -1, -1, -1) )
    nSwapL.setB()
    // Node@821299933(B)
    val nSwapR = Node( Seq(Set(Seq(9, 1))), Seq(-1, 0, -1, -1, -1, -1, 0, -1, -1, -1, -1, 0, -1, -1, -1, -1, 0, -1, -1, -1, -1, 0, -1, -1, -1) )
    nSwapR.setB()
    // Node@103207052(A)
    val nOtherL = Node( Seq(Set(Seq(9, 1), Seq(8, 1))), Seq(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, -1, -1, -1, -1) )
    nOtherL.setA()
    // Node@103207052(A)
    val nOtherR = nOtherL

    nodePar.setL( Some(node) )
    node.setL( Some(nOther) )
    node.setR( Some(nSwap) )
    nSwap.setL( Some(nSwapL) )
    nSwap.setR( Some(nSwapR) )
    nOther.setL( Some(nOtherL) )
    nOther.setR( Some(nOtherR) )

    val nodeSeq = Seq( node, nSwap, nOther )
    for ( n <- nodeSeq )
      assert( Node.isMinimal( n ), "node " + n + " should be minimal" )

    val res = Transforms.trySwap( node, nSwap, true )

    // clean up parents of merged nodes
    nSwap.setL( None )
    nSwap.setR( None )
    nOther.setL( None )
    nOther.setR( None )

    res._1.foreach( n => assert( Node.isMinimal( n ), "node " + n + " should be minimal" ) )
  }

  @Test def minimalTest2 {
    // Node@1736012577(B)
    val node = Node( Seq(Set(Seq(10, 1), Seq(6, 1), Seq(5, 1)), Set(Seq(6, 1))), Seq(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, 1, -1, -1, -1, -1, -1, -1) )
    node.setB()
    // Node@929306827(B)
    val nSwap = Node( Seq(Set(Seq(5, 1), Seq(4, 1), Seq(9, 1)), Set(Seq(5, 1))), Seq(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, 1, -1, -1, -1, -1, -1, -1, -1) )
    nSwap.setB()
    // Node@929306827(B)
    val nOther = nSwap

    // Node@2069863558(B)
    val nodePar = Node( Seq(Set(Seq(11, 1), Seq(7, 1), Seq(6, 1)), Set(Seq(7, 1))), Seq(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, 1, -1, -1, -1, -1, -1) )
    nodePar.setB()
    // Node@1521934339(B)
    val nSwapL =  Node( Seq(Set(Seq(4, 1))), Seq(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1) )
    nSwapL.setB()
    // Node@2009145978(A)
    val nSwapR = Node( Seq(Set(Seq(4, 1), Seq(3, 1), Seq(8, 1))), Seq(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1) )
    nSwapR.setA()

    nodePar.setL( Some(node) )
    node.setL( Some( nSwap ) )
    node.setR( Some( nOther ) )
    nSwap.setL( Some(nSwapL) )
    nSwap.setR( Some(nSwapR) )

    val nodeSeq = Seq( node, nSwap, nOther )
    for ( n <- nodeSeq )
      assert( Node.isMinimal( n ), "node " + n + " should be minimal" )

    val res = Transforms.trySwap( node, nSwap, true )

    // clean up parents of merged nodes
    nSwap.setL( None )
    nSwap.setR( None )
    nOther.setL( None )
    nOther.setR( None )

    res._1.foreach( n => assert( Node.isMinimal( n ), "node " + n + " should be minimal" ) )

  }

  @Test def hardwareGen {
    val nodeA = Node( Seq( Set(Seq( 0, 0 )) ), Seq( 0, -1, 0, -1 ) )
    val nodeB = Node( Seq( Set(Seq( 0, 1 )) ), Seq( -1, 0, -1, 0 ) )
    val nodeC = Node( Seq( Set(Seq( 0, 2 )) ), Seq( 0, -1, -1, -1 ) )
    val nodeD = Node( Seq( Set(Seq( 0, 3 )) ), Seq( -1, -1, 0, -1 ) )

    val mux1 = Node( Seq( Set(Seq( 1, 0 )), Set(Seq( 1, 1 )) ), Seq( 1, 0, 1, 0 ) )
    val mux2 = Node( Seq( Set(Seq( 2, 0 )), Set(Seq( 2, 1 )), Set(Seq( 1, 2 )) ), Seq( 0, 2, -1, 1 ) )
    val mux3 = Node( Seq( Set(Seq( 3, 0 )), Set(Seq( 3, 1 )), Set(Seq( 2, 2 )), Set(Seq( 1, 3 )) ), Seq( 1, 0, 2, 3 ) )

    val reg1 = Node( Seq( Set(Seq( 2, 0 )), Set(Seq( 2, 1 )) ), Seq( 0, 1, 0, 1 ) )

    nodeA.setC()
    nodeB.setC()
    nodeC.setC()
    nodeD.setC()

    mux1.setB()
    mux2.setB()
    mux3.setB()

    reg1.setB()

    mux1.setL( nodeA )
    mux1.setR( nodeB )
    mux2.setL( mux1 )
    mux2.setR( nodeC )
    mux3.setL( mux2 )
    mux3.setR( nodeD )

    reg1.setL( mux1 )
    reg1.setR( mux1 )

    val nodes = Set( nodeA, nodeB, nodeC, nodeD, mux1, mux2, mux3, reg1 )

    // test constraints
    assert( testLinks( nodes ), "Nodes must be connected properly" )
    for ( n <- nodes )
      assert( Node.satisfiesConstraints(n), "Node " + n + " must satisfy constraints" )

    // test hardware
    verifyHardware( nodes, Vector( reg1, mux3 ) )
  }

  def readCsv( filename : String ) : Seq[Seq[Seq[Seq[Int]]]] = {
    val reader = CSVReader.open(new File(filename))
    val floatData = reader.all().map( x => x.map( _.toFloat.toInt ).to[Seq] )
    reader.close()
    // group into the convolutional filters
    floatData.toVector.to[Seq].grouped(3).toVector.to[Seq].grouped(3).toVector.to[Seq]
  }

  @Test def binarizedConv {

    // read in a conv filter
    val filename = "src/main/resources/conv1.csv"
    val conv = readCsv( filename )
    val ( initNodes, outNodes, x ) = AnnealingSolver.init( binFilterToCp( conv, ( 32, 32 ) ) )
    println( "created " + initNodes.size + " nodes")
    val nodes = AnnealingSolver.runPar( initNodes, 100000000, 1000000 )
    assert( testLinks( nodes ), "Nodes must be connected properly" )
    for ( n <- nodes )
      assert( Node.satisfiesConstraints(n), "Nodes must satisfy constraints" )
    AnnealingSolver.toDot( nodes, "binConv2.dot" )
    println( "cost = " + nodes.size )
    val parNodes = outNodes.toVector
    verifyHardware( nodes, parNodes )
  }

  @Test def saveAndLoad {
    val filterSize = ( 3, 3, 1, 1 )
    val imgSize = ( 5, 5 )
    val initNodes = AnnealingSolver.init( genTrinary( filterSize, imgSize ) )._1
    AnnealingSolver.save( initNodes, "testSave.obj" )
    val newNodes = AnnealingSolver.load( "testSave.obj" )
    assert( initNodes.size == newNodes.size )
    for ( n <- newNodes ) {
      assert( initNodes.find( nn =>
        nn.uk == n.uk && nn.ck == n.ck && n.letter() == nn.letter()
      ).isDefined )
    }
    // cant do this as hashcodes change ...
    //for ( n <- initNodes )
    // assert( newNodes.contains(n) )
  }

}
