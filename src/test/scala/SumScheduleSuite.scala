import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import chiselutils.algorithms.Node
import chiselutils.algorithms.Transforms
import chiselutils.algorithms.AnnealingSolver
import scala.collection.immutable.HashSet

class SumScheduleSuite extends TestSuite {

  val myRand = new Random
  val dim = 2
  val nodeSize = 20

  def decr( uk : List[Set[Vector[Int]]] ) : List[Set[Vector[Int]]] = {
    uk.map( s => s.map( v => { Vector( v(0) - 1 ) ++ v.drop(1) }))
  }

  def incr( uk : List[Set[Vector[Int]]] ) : List[Set[Vector[Int]]] = {
    uk.map( s => s.map( v => { Vector( v(0) + 1 ) ++ v.drop(1) }))
  }

  def testLinks( nodes : HashSet[Node] ) : Boolean = {
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

  /** Test the sum constraint
    */
  @Test def testConstraintA {
    val noUk = 5
    val setSize = 5
    val maxVal = 10
    val nodeAuK = List.fill( noUk ) { List.fill( setSize ) { Vector.fill( dim ) { myRand.nextInt( maxVal ) + 1 }}.toSet }
    val nodeAcK = List.fill( nodeSize ) { myRand.nextInt( noUk ) }
    val nodeA = Node( nodeAuK, nodeAcK )
    val nodeBCuK = nodeAuK.map( uki => {
      val listB = ArrayBuffer[Vector[Int]]()
      val listC = ArrayBuffer[Vector[Int]]()
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
    val noUk = 5
    val setSize = 5
    val maxVal = 10
    val nodeAuK = List.fill( noUk ) { List.fill( setSize ) { Vector.fill( dim ) { myRand.nextInt( maxVal ) + 1 }}.toSet }
    val nodeAcK = List.fill( nodeSize ) { myRand.nextInt( noUk ) }
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
    val nodeAuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 1 } ))
    val nodeAcK = List.fill( nodeSize ) { myRand.nextInt( 2 ) - 1 }
    val nodeA = Node( nodeAuK, nodeAcK )
    assert( Node.satisfiesConstraintC( nodeA ) )
  }

  @Test def testSwap1 {
    val nodeAuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 0 } )) // termination setA
    val nodeBuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 1 } )) // termination setB
    val node_cK = List.fill( nodeSize ) { myRand.nextInt( 2 ) - 1 }
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

    val nodeList = Transforms.trySwap( nodePar, nodeSwap )

    assert( Node.satisfiesConstraintA( nodeList(0) ) && nodeList(0).isA() )
    assert( Node.satisfiesConstraintB( nodeList(1) ) && nodeList(1).isB() )
    assert( Node.satisfiesConstraintB( nodeList(2) ) && nodeList(2).isB() )
  }

  @Test def testSwap2 {
    val nodeAuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 0 } )) // termination setA
    val nodeBuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 1 } )) // termination setB
    val node_cK = List.fill( nodeSize ) { myRand.nextInt( 2 ) }
    val nodeA = Node( nodeAuK, node_cK.map( -_ ) )
    nodeA.setC()
    val nodeB = Node( nodeBuK, node_cK.map( _ - 1 ) )
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

    val nodeList = Transforms.trySwap( nodePar, nodeSwap )

    assert( Node.satisfiesConstraintB( nodeList(0) ) && nodeList(0).isB() )
    assert( Node.satisfiesConstraintB( nodeList(1) ) && nodeList(1).isB() )
    assert( Node.satisfiesConstraintB( nodeList(2) ) && nodeList(2).isB() )
  }

  @Test def testSwap4 {
    val nodeAuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 0 } )) // termination setA
    val nodeBuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 1 } )) // termination setB
    val nodeCuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 2 } )) // termination setC
    val node_cK = List.fill( nodeSize ) { myRand.nextInt( 2 ) - 1 }
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

    val nodeList = Transforms.trySwap( nodePar, nodeSwap )

    assert( Node.satisfiesConstraintA( nodeList(0) ) && nodeList(0).isA() )
    assert( Node.satisfiesConstraintB( nodeList(1) ) && nodeList(1).isB() )
    assert( Node.satisfiesConstraintA( nodeList(2) ) && nodeList(2).isA() )
  }

  @Test def testSwap5 {
    val nodeAuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 0 } )) // termination setA
    val nodeBuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 1 } )) // termination setB
    val nodeCuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 2 } )) // termination setC
    val nodeDuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 3 } )) // termination setC
    val nodeParCk = List.fill( nodeSize ) { myRand.nextInt(2) - 1 }
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

    val nodeList = Transforms.trySwap( nodePar, nodeSwap )

    assert( Node.satisfiesConstraintA( nodeList(0) ) && nodeList(0).isA() )
    assert( Node.satisfiesConstraintA( nodeList(1) ) && nodeList(1).isA() )
    assert( Node.satisfiesConstraintA( nodeList(2) ) && nodeList(2).isA() )
  }

  @Test def testSwap6 {
    val nodeAuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 0 } )) // termination setA
    val nodeBuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 1 } )) // termination setB
    val node_cK = List.fill( nodeSize ) { myRand.nextInt( 2 ) - 1 }
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

    val nodeList = Transforms.trySwap( nodePar, nodeSwap )

    assert( Node.satisfiesConstraintB( nodeList(0) ) && nodeList(0).isB() )
    assert( Node.satisfiesConstraintA( nodeList(1) ) && nodeList(1).isA() )
  }

  @Test def testSwap7 {
    val nodeAuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 0 } )) // termination setA
    val nodeBuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 1 } )) // termination setB
    val nodeCuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 2 } )) // termination setC
    val node_cK = List.fill( nodeSize ) { myRand.nextInt( 2 ) - 1 }
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

    val nodeList = Transforms.trySwap( nodePar, nodeSwap )

    assert( Node.satisfiesConstraintB( nodeList(0) ) && nodeList(0).isB() )
    assert( Node.satisfiesConstraintA( nodeList(1) ) && nodeList(1).isA() )
    assert( Node.satisfiesConstraintA( nodeList(2) ) && nodeList(2).isA() )
  }

  @Test def testSwap10 {
    val nodeAuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 0 } )) // termination setA
    val nodeBuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 1 } )) // termination setB
    val nodeParCk = List.fill( nodeSize ) { myRand.nextInt( 3 ) - 1 }
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

    val nodeList = Transforms.trySwap( nodePar, nodeSwap )

    assert( Node.satisfiesConstraintB( nodeList(0) ) && nodeList(0).isB() )
    assert( Node.satisfiesConstraintB( nodeList(1) ) && nodeList(1).isB() )
  }

  @Test def testSwap11 {
    val nodeAuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 0 } )) // termination setA
    val nodeBuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 1 } )) // termination setB
    val nodeCuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 2 } )) // termination setC
    val nodeParCk = List.fill( nodeSize ) { myRand.nextInt( 3 ) - 1 }
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

    val nodeList = Transforms.trySwap( nodePar, nodeSwap )

    assert( Node.satisfiesConstraintA( nodeList(0) ) && nodeList(0).isA() )
    assert( Node.satisfiesConstraintB( nodeList(1) ) && nodeList(1).isB() )
    assert( Node.satisfiesConstraintB( nodeList(2) ) && nodeList(2).isB() )
  }

  @Test def testSwap12 {
    val nodeAuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 0 } )) // termination setA
    val nodeBuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 1 } )) // termination setB
    val nodeCuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 2 } )) // termination setC
    val nodeDuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 3 } )) // termination setC
    val nodeParCk = List.fill( nodeSize ) { myRand.nextInt( 5 ) - 1 }
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

    val nodeList = Transforms.trySwap( nodePar, nodeSwap )

    assert( Node.satisfiesConstraintB( nodeList(0) ) && nodeList(0).isB() )
    assert( Node.satisfiesConstraintB( nodeList(1) ) && nodeList(1).isB() )
    assert( Node.satisfiesConstraintB( nodeList(2) ) && nodeList(2).isB() )
  }

  @Test def testSwap13 {
    val nodeAuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 0 } )) // termination setA
    val nodeBuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 1 } )) // termination setB
    val nodeCuK = List( Set( Vector( 0 ) ++ Vector.fill( dim - 1 ) { 2 } )) // termination setC
    val nodeParCk = List.fill( nodeSize ) { myRand.nextInt( 4 ) - 1 }
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

    val nodeList = Transforms.trySwap( nodePar, nodeSwap )

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
      cSet._1.map( v => Vector( cSet._2 - v(0)) ++ v.drop(1) )
    })
    val latAdd = AnnealingSolver.needLatency( List( cpCoords ) )
    println( "latAdd = " + latAdd )
    val cp = cpCoords.zipWithIndex.map( cSet => {
      cSet._1.map( v => { Vector( latAdd + v(0) ) ++ v.drop(1) })
    })
    var nodes = AnnealingSolver.init( List( cp ) )._1
    nodes = AnnealingSolver.runPar( nodes, 10000000 )
    assert( testLinks( nodes ), "Nodes must be connected properly" )
    for ( n <- nodes )
      assert( Node.satisfiesConstraints(n), "Nodes must satisfy constraints" )
    AnnealingSolver.toDot( nodes, "conv3n5.dot" )
    println( "cost = " + nodes.size )
  }

  @Test def trinaryLayer {
    val filterSize = ( 3, 3, 3, 128 )
    val imgSize = ( 32, 32 )
    val convFilter = List.fill(filterSize._4) {
      ( 0 until filterSize._1).toList.map( f1 => {
        ( 0 until filterSize._2).toList.map( f2 => {
          ( 0 until filterSize._3).toList.map( f3 => {
            val num = Random.nextInt(100)
            // cant deal with all 0 yet so just force not that case by putting 1 in mid
            val inMid = ( f1 == filterSize._1/2 && f2 == filterSize._2/2 && f3 == filterSize._3/2 )
            val trinary = {
              if ( num < 16 || inMid  )
                1
              else if ( num < 16 + 22 )
                -1
              else
                0
            }
            trinary
          })
        })
      })
    }
    for ( convFilt <- convFilter )
      assert( convFilt(1)(1)(1) == 1, "Mid must be 1" )

    val cp = ( 0 until filterSize._4 ).map( convIdx => { // for each filter
      ( 0 until imgSize._2 ).map( y => { // for each column in the img
        ( 0 until imgSize._1 ).map( x => { // for each row in that column
          ( 0 until filterSize._1 ).map( px => {
            ( 0 until filterSize._2 ).map( py => {
              ( 0 until filterSize._3 ).map( d => {
                val xpx = x + px - (filterSize._1/2)
                val ypy = y + py - (filterSize._2/2)
                ( xpx, ypy, px, py, d )
              }).filter{ case ( xpx, ypy, px, py, d ) => { // filter out zeros and any edge parts
                val isZero = ( convFilter( convIdx )(px)(py)(d) == 0 )
                ( xpx >= 0 && xpx < imgSize._1 && ypy >= 0 && ypy < imgSize._2 && !isZero )
              }}.map{ case ( xpx, ypy, px, py, d ) => {
                val addIdx = if ( convFilter( convIdx )(px)(py)(d) == 1 ) 1 else 0
                Vector( ypy*imgSize._2 + xpx, (2*d) + addIdx )
              }}.toSet
            }).reduce( _ ++ _ ) // sum over filter cols
          }).reduce( _ ++ _ ) // sum over filter rows
        })
      }).reduce( _ ++ _ ).toList // collect image into a list
    })

    for ( convFilt <- cp ) {
      for ( cSet <- convFilt ){
        assert( cSet.size > 0, "can't deal with empty sets" )
      }
    }
    val cpCoords = cp.map( convFilt => {
      convFilt.zipWithIndex.map( cSet => {
        cSet._1.map( v => Vector( cSet._2 - v(0)) ++ v.drop(1) )
      })
    }).toList
    val latAdd = AnnealingSolver.needLatency( cpCoords )
    println( "latAdd = " + latAdd )
    val cpFinal = cpCoords.map( convFilt => {
      convFilt.zipWithIndex.map( cSet => {
        cSet._1.map( v => { Vector( latAdd + v(0) ) ++ v.drop(1) })
      })
    })
    var nodes = AnnealingSolver.init( cpFinal )._1
    println( "created " + nodes.size + " nodes")
    nodes = AnnealingSolver.runPar( nodes, 10000000 )
    assert( testLinks( nodes ), "Nodes must be connected properly" )
    for ( n <- nodes )
      assert( Node.satisfiesConstraints(n), "Nodes must satisfy constraints" )
    AnnealingSolver.toDot( nodes, "trinary.dot" )
    println( "cost = " + nodes.size )
  }

}
