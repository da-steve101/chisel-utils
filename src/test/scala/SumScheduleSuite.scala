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

  def genUk( noUk : Int, setSize : Int ) : Seq[Set[Seq[Int]]] = {
    Vector.fill( noUk ) { Vector.fill( setSize ) { Vector.fill( dim ) { myRand.nextInt( maxVal ) + 1 }.to[Seq]}.to[Set] }
  }

  def genTermUk( x : Int  ) : Seq[Set[Seq[Int]]] = {
    Vector(  Vector( (Vector( 0 ) ++ Vector.fill( dim - 1 ) { x }).to[Seq] ).to[Set] )
  }

  def genCk( noUk : Int ) : Seq[Int] = {
    while ( true ) {
      val newCk = Vector.fill( nodeSize ) { myRand.nextInt( noUk + 1 ) - 1 }
      if ( ( 0 until noUk ).map( i => newCk.contains( i ) ).reduce( _ && _ ) )
        return newCk
    }
    Vector.fill( nodeSize ) { myRand.nextInt( noUk + 1 ) - 1 }
  }

  def decr( uk : Seq[Set[Seq[Int]]] ) : Seq[Set[Seq[Int]]] = {
    uk.map( s => s.map( v => { Vector( v(0) - 1 ) ++ v.drop(1) }.to[Seq]))
  }

  def incr( uk : Seq[Set[Seq[Int]]] ) : Seq[Set[Seq[Int]]] = {
    uk.map( s => s.map( v => { Vector( v(0) + 1 ) ++ v.drop(1) }.to[Seq]))
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
    nodeA.addChild( nodeB )
    nodeA.addChild( nodeC )
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
    nodeA.addChild( nodeB )
    nodeA.addChild( nodeC )
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
    nodeSwap.addChild( nodeA )
    nodeSwap.addChild( nodeB )
    val nodePar = Node( nodeSwap.getUkNext, nodeSwap.getCkNext )
    nodePar.setB()
    nodePar.addChild( nodeSwap )
    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintA( nodeSwap ) )
    assert( Node.satisfiesConstraintB( nodePar ) )

    val nodeList = Transforms.trySwap( nodePar )._1

    assert( nodeList.size == 3 )
    assert( Node.satisfiesConstraintA( nodeList(0) ) && nodeList(0).isAdd2() )
    assert( Node.satisfiesConstraintB( nodeList(1) ) && nodeList(1).isReg() )
    assert( Node.satisfiesConstraintB( nodeList(2) ) && nodeList(2).isReg() )

    VerifyHardware( nodeList.toSet ++ Set( nodeA, nodeB ), List( nodeList(0) ) )
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
    nodeSwap.addChild( nodeA )
    nodeSwap.addChild( nodeB )
    val nodeParUk = incr( nodeSwapUk )
    val nodeParCk = nodeSwapCk.takeRight( 1 ) ++ nodeSwapCk.dropRight( 1 )
    val nodePar = Node( nodeParUk, nodeParCk )
    nodePar.setB()
    nodePar.addChild( nodeSwap )
    nodePar.addChild( nodeSwap )
    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintB( nodeSwap ) )
    assert( Node.satisfiesConstraintB( nodePar ) )

    val nodeList = Transforms.trySwap( nodePar )._1

    assert( nodeList.size == 3 )
    assert( Node.satisfiesConstraintB( nodeList(0) ) && nodeList(0).isMux() )
    assert( Node.satisfiesConstraintB( nodeList(1) ) && nodeList(1).isReg() )
    assert( Node.satisfiesConstraintB( nodeList(2) ) && nodeList(2).isReg() )

    VerifyHardware( nodeList.toSet ++ Set( nodeA, nodeB ), List( nodeList(0) ) )
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
    val nodeSwap = Node( nodeA.getUkNext, nodeA.getCkNext )
    nodeSwap.setB()
    val nodeOtherUk = incr( nodeBuK.zip( nodeCuK ).map( z => z._1 ++ z._2 ) )
    val nodeOtherCk = nodeB.getCkNext
    val nodeOther = Node( nodeOtherUk, nodeOtherCk )
    nodeOther.setA()

    nodeSwap.addChild( nodeA )
    nodeOther.addChild( nodeB )
    nodeOther.addChild( nodeC )

    val nodeParUk = incr( nodeSwap.uk.zip( nodeOther.uk ).map( z => z._1 ++ z._2 ) )
    val nodeParCk = nodeSwap.getCkNext
    val nodePar = Node( nodeParUk, nodeParCk )
    nodePar.setA()
    nodePar.addChild( nodeSwap )
    nodePar.addChild( nodeOther )

    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintC( nodeC ) )
    assert( Node.satisfiesConstraintB( nodeSwap ) )
    assert( Node.satisfiesConstraintA( nodeOther ) )
    assert( Node.satisfiesConstraintA( nodePar ) )

    val nodeList = Transforms.trySwap( nodePar )._1

    assert( nodeList.size == 2 )
    assert( Node.satisfiesConstraintB( nodeList(0) ) && nodeList(0).isReg() )
    assert( Node.satisfiesConstraintA( nodeList(1) ) && nodeList(1).isAdd3() )

    VerifyHardware( nodeList.toSet ++ Set( nodeA, nodeB, nodeC ), List( nodeList(0) ) )
  }

  @Test def testSwap4B {
    val nodeAuK = genTermUk( 0 ) // termination setA
    val nodeBuK = genTermUk( 1 ) // termination setB
    val nodeCuK = genTermUk( 2 ) // termination setC
    val nodeDuK = genTermUk( 3 ) // termination setD
    val node_cK = genCk( 1 )
    val nodeA = Node( nodeAuK, node_cK )
    val nodeB = Node( nodeBuK, node_cK )
    val nodeC = Node( nodeCuK, node_cK )
    val nodeD = Node( nodeDuK, node_cK )
    nodeA.setC()
    nodeB.setC()
    nodeC.setC()
    nodeD.setC()
    val nodeSwap = Node( nodeA.getUkNext, nodeA.getCkNext )
    nodeSwap.setB()
    val nodeOtherUk = incr( nodeBuK.zip( nodeCuK.zip( nodeDuK ) ).map( z => z._1 ++ z._2._1 ++ z._2._2 ) )
    val nodeOtherCk = nodeB.getCkNext
    val nodeOther = Node( nodeOtherUk, nodeOtherCk )
    nodeOther.setA()

    nodeSwap.addChild( nodeA )
    nodeOther.addChild( nodeB )
    nodeOther.addChild( nodeC )
    nodeOther.addChild( nodeD )

    val nodeParUk = incr( nodeSwap.uk.zip( nodeOther.uk ).map( z => z._1 ++ z._2 ) )
    val nodeParCk = nodeSwap.getCkNext
    val nodePar = Node( nodeParUk, nodeParCk )
    nodePar.setA()
    nodePar.addChild( nodeSwap )
    nodePar.addChild( nodeOther )

    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintC( nodeC ) )
    assert( Node.satisfiesConstraintC( nodeD ) )
    assert( Node.satisfiesConstraintB( nodeSwap ) )
    assert( Node.satisfiesConstraintA( nodeOther ) )
    assert( Node.satisfiesConstraintA( nodePar ) )

    val nodeList = Transforms.trySwap( nodePar )._1

    assert( nodeList.size == 3 )
    for ( n <- nodeList )
      assert( Node.satisfiesConstraints( n ) )

    VerifyHardware( nodeList.toSet ++ Set( nodeA, nodeB, nodeC, nodeD ), List( nodeList(0) ) )
  }

  @Test def testSwap5 {
    for ( numInputs <- 4 to 9 ) {
      val uksIn = ( 0 until numInputs ).map( i => genTermUk( i ) )
      val nodeParCk = genCk( 1 )
      val node_cK = nodeParCk.drop(2) ++ nodeParCk.take(2)
      val node_cK1 = nodeParCk.drop(1) ++ nodeParCk.take(1)
      val nodesIn = uksIn.map( uk => Node( uk, node_cK ) )
      for ( n <- nodesIn )
        n.setC()
      val groupingSize = {
        if ( numInputs == 4 )
          2
        else if ( numInputs == 5 )
          3
        else if ( numInputs == 6 )
          Random.nextInt(2) + 2
        else if ( numInputs > 7 )
          3
        else
          -1
      }
      val grps =  {
        if ( numInputs == 7 ) {
          val dblGrps = nodesIn.grouped(2).toList
          Vector( dblGrps(0), dblGrps(1), dblGrps(2) ++ dblGrps(3) )
        } else
          nodesIn.grouped( groupingSize )
      }
      val addNodes = grps.map( grp => {
        val uk = Vector( grp.map( _.getUkNext.head ).reduce( _ ++ _ ) )
        val n = Node( uk, node_cK1 )
        n.setA()
        n.addChildren( grp.toSet )
        n
      }).toSet

      val nodeParUk = Vector( addNodes.map( _.getUkNext.head ).reduce( _ ++ _ ) )
      val nodePar = Node( nodeParUk, nodeParCk )
      nodePar.setA()
      nodePar.addChildren( addNodes )

      for ( n <- nodesIn )
        assert( Node.satisfiesConstraintC( n ) )
      for ( n <- addNodes )
        assert( Node.satisfiesConstraintA( n ) )
      assert( Node.satisfiesConstraintA( nodePar ) )

      val nodeList = Transforms.trySwap( nodePar )._1

      assert( Node.satisfiesConstraintA( nodeList(0) ) && nodeList(0).isA() )
      assert( nodeList.tail.map( _.numChildren() ).sum == numInputs )
      for ( n <- nodeList.tail ) {
        if ( n.numChildren() > 1 )
          assert( Node.satisfiesConstraintA( n ) && n.isAdd() )
        else
          assert( Node.satisfiesConstraintB( n ) && n.isReg() )
      }

      VerifyHardware( nodeList.toSet ++ nodesIn.toSet, List( nodeList(0) ) )
    }
  }

  @Test def testSwap6 {
    val nodeAuK = genTermUk( 0 ) // termination setA
    val nodeBuK = genTermUk( 1 ) // termination setB
    val node_cK = genCk( 1 )
    val nodeA = Node( nodeAuK, node_cK )
    val nodeB = Node( nodeBuK, node_cK )
    nodeA.setC()
    nodeB.setC()
    val nodeSwap = Node( nodeA.getUkNext, nodeA.getCkNext )
    val nodeOther = Node( nodeB.getUkNext, nodeB.getCkNext )
    nodeSwap.setB()
    nodeOther.setB()
    nodeSwap.addChild( nodeA )
    nodeSwap.addChild( nodeA )
    nodeOther.addChild( nodeB )
    nodeOther.addChild( nodeB )

    val nodeParUk = incr( nodeSwap.uk.zip( nodeOther.uk ).map( z => z._1 ++ z._2 ) )
    val nodeParCk = nodeSwap.getCkNext
    val nodePar = Node( nodeParUk, nodeParCk )
    nodePar.setA()
    nodePar.addChild( nodeSwap )
    nodePar.addChild( nodeOther )

    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintB( nodeSwap ) )
    assert( Node.satisfiesConstraintB( nodeOther ) )
    assert( Node.satisfiesConstraintA( nodePar ) )

    val nodeList = Transforms.trySwap( nodePar )._1

    assert( nodeList.size == 2 )
    assert( Node.satisfiesConstraintB( nodeList(0) ) && nodeList(0).isReg() )
    assert( Node.satisfiesConstraintA( nodeList(1) ) && nodeList(1).isAdd2() )

    VerifyHardware( nodeList.toSet ++ Set( nodeA, nodeB ), List( nodeList(0) ) )
  }

  @Test def testSwap7 {
    val nodeAuK = genTermUk( 0 ) // termination setA
    val nodeBuK = genTermUk( 1 ) // termination setB
    val nodeCuK = genTermUk( 2 ) // termination setC
    val node_cK = genCk( 2 )
    val node_cK1 = node_cK.drop(1) ++ node_cK.take(1)
    val nodeA = Node( nodeAuK, node_cK1.map( x => if ( x == -1 ) -1 else 0 ) )
    val nodeB = Node( nodeBuK, node_cK1.map( x => if ( x == 0 ) 0 else -1 ) )
    val nodeC = Node( nodeCuK, node_cK1.map( x => if ( x == 1 ) 0 else -1 ) )
    nodeA.setC()
    nodeB.setC()
    nodeC.setC()
    val nodeOtherUk = nodeB.getUkNext ++ nodeC.getUkNext
    val nodeSwap = Node( nodeA.getUkNext, nodeA.getCkNext )
    val nodeOther = Node( nodeOtherUk, node_cK)
    nodeSwap.setB()
    nodeOther.setB()
    nodeSwap.addChild( nodeA )
    nodeOther.addChild( nodeB )
    nodeOther.addChild( nodeC )

    val nodeParUk = incr( nodeOther.uk.map( z => nodeSwap.uk.head ++ z ) )
    val nodeParCk = nodeOther.getCkNext
    val nodePar = Node( nodeParUk, nodeParCk )
    nodePar.setA()
    nodePar.addChild( nodeSwap )
    nodePar.addChild( nodeOther )

    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintC( nodeC ) )
    assert( Node.satisfiesConstraintB( nodeSwap ) )
    assert( Node.satisfiesConstraintB( nodeOther ) )
    assert( Node.satisfiesConstraintA( nodePar ) )

    val nodeList = Transforms.trySwap( nodePar )._1

    assert( nodeList.size == 3 )
    assert( Node.satisfiesConstraintB( nodeList(0) ) && nodeList(0).isMux() )
    assert( Node.satisfiesConstraintA( nodeList(1) ) && nodeList(1).isAdd2() )
    assert( Node.satisfiesConstraintA( nodeList(2) ) && nodeList(2).isAdd2() )

    VerifyHardware( nodeList.toSet ++ Set( nodeA, nodeB, nodeC ), List( nodeList(0) ) )

  }

  @Test def testSwap9 {
    val nodeAuK = genTermUk( 0 ) // termination setA
    val nodeBuK = genTermUk( 1 ) // termination setB
    val nodeCuK = genTermUk( 2 ) // termination setC
    val nodeDuK = genTermUk( 3 ) // termination setD
    val node_cK = genCk( 2 )
    val add_cK = node_cK.map( x => if (x == -1 ) -1 else 0 )
    val nodeA = Node( nodeAuK, add_cK )
    val nodeB = Node( nodeBuK, add_cK )
    val nodeC = Node( nodeCuK, node_cK.map( x => if ( x == 0 ) 0 else -1 ) )
    val nodeD = Node( nodeDuK, node_cK.map( x => if ( x == 1 ) 0 else -1 ) )
    nodeA.setC()
    nodeB.setC()
    nodeC.setC()
    nodeD.setC()
    val nAdd = Node( Vector( nodeA.getUkNext.head ++ nodeB.getUkNext.head ), nodeA.getCkNext )
    val nMux = Node( nodeC.getUkNext ++ nodeD.getUkNext, node_cK.takeRight(1) ++ node_cK.dropRight(1) )
    val nPar = Node( nMux.getUkNext.map( uki => uki ++ nAdd.getUkNext.head ), nMux.getCkNext )
    nAdd.setA()
    nMux.setB()
    nPar.setA()
    nAdd.addChild( nodeA )
    nAdd.addChild( nodeB )
    nMux.addChild( nodeC )
    nMux.addChild( nodeD )
    nPar.addChild( nAdd )
    nPar.addChild( nMux )

    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintC( nodeC ) )
    assert( Node.satisfiesConstraintC( nodeD ) )
    assert( Node.satisfiesConstraintA( nAdd ) )
    assert( Node.satisfiesConstraintB( nMux ) )
    assert( Node.satisfiesConstraintA( nPar ) )

    val nodeList = Transforms.trySwap( nPar )._1

    assert( nodeList.size == 3 )
    assert( Node.satisfiesConstraintB( nodeList(0) ) && nodeList(0).isMux() )
    assert( Node.satisfiesConstraintA( nodeList(1) ) && nodeList(1).isAdd3() )
    assert( Node.satisfiesConstraintA( nodeList(2) ) && nodeList(2).isAdd3() )

    VerifyHardware( nodeList.toSet ++ Set( nodeA, nodeB, nodeC, nodeD ), List( nodeList(0) ) )
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
    val nodeSwap = Node( nodeA.getUkNext, nodeA.getCkNext )
    val nodeOther = Node( nodeB.getUkNext, nodeB.getCkNext )
    nodeSwap.setB()
    nodeSwap.addChild( nodeA )
    nodeSwap.addChild( nodeA )
    nodeOther.setB()
    nodeOther.addChild( nodeB )
    nodeOther.addChild( nodeB )

    val nodeParUk = nodeSwap.getUkNext ++ nodeOther.getUkNext
    val nodePar = Node( nodeParUk, nodeParCk )
    nodePar.setB()
    nodePar.addChild( nodeSwap )
    nodePar.addChild( nodeOther )

    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintB( nodeSwap ) )
    assert( Node.satisfiesConstraintB( nodeOther ) )
    assert( Node.satisfiesConstraintB( nodePar ) )

    val nodeList = Transforms.trySwap( nodePar )._1

    assert( nodeList.size == 2 )
    assert( Node.satisfiesConstraintB( nodeList(0) ) && nodeList(0).isReg() )
    assert( Node.satisfiesConstraintB( nodeList(1) ) && nodeList(1).isMux() )

    VerifyHardware( nodeList.toSet ++ Set( nodeA, nodeB ), List( nodeList(0) ) )
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
    val nodeSwap = Node( nodeA.getUkNext.map( z => nodeB.getUkNext.head ++ z ), nodeA.getCkNext )
    val nodeOther = Node( nodeC.getUkNext.map( z => nodeB.getUkNext.head ++ z ), nodeC.getCkNext )
    nodeSwap.setA()
    nodeSwap.addChild( nodeA )
    nodeSwap.addChild( nodeB )
    nodeOther.setA()
    nodeOther.addChild( nodeB )
    nodeOther.addChild( nodeC )

    val nodeParUk = nodeSwap.getUkNext ++ nodeOther.getUkNext
    val nodePar = Node( nodeParUk, nodeParCk )
    nodePar.setB()
    nodePar.addChild( nodeSwap )
    nodePar.addChild( nodeOther )

    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintC( nodeC ) )
    assert( Node.satisfiesConstraintA( nodeSwap ) )
    assert( Node.satisfiesConstraintA( nodeOther ) )
    assert( Node.satisfiesConstraintB( nodePar ) )

    val nodeList = Transforms.trySwap( nodePar )._1

    assert( nodeList.size == 3 )
    assert( Node.satisfiesConstraintA( nodeList(0) ) && nodeList(0).isAdd2() )
    assert( Node.satisfiesConstraintB( nodeList(1) ) && nodeList(1).isReg() )
    assert( Node.satisfiesConstraintB( nodeList(2) ) && nodeList(2).isMux() )

    VerifyHardware( nodeList.toSet ++ Set( nodeA, nodeB, nodeC ), List( nodeList(0) ) )
  }

  @Test def testSwap11B {
    val nodeAuK = genTermUk( 0 ) // termination setA
    val nodeBuK = genTermUk( 1 ) // termination setB
    val nodeCuK = genTermUk( 2 ) // termination setC
    val nodeDuK = genTermUk( 3 ) // termination setD
    val node_cK = genCk( 2 )
    val nodeA_cK = node_cK.map( x => if ( x == 0 ) 0 else -1 )
    val nodeBC_cK = node_cK.map( x => if ( x == -1 ) -1 else 0 )
    val nodeD_cK = node_cK.map( x => if ( x == 1 ) 0 else -1 )
    val nodeA = Node( nodeAuK, nodeA_cK )
    val nodeB = Node( nodeBuK, nodeBC_cK )
    val nodeC = Node( nodeCuK, nodeBC_cK )
    val nodeD = Node( nodeDuK, nodeD_cK )
    nodeA.setC()
    nodeB.setC()
    nodeC.setC()
    nodeD.setC()
    val nAdd1 = Node( Vector( nodeA.getUkNext.head ++ nodeB.getUkNext.head ++ nodeC.getUkNext.head ), nodeA.getCkNext )
    val nAdd2 = Node( Vector( nodeD.getUkNext.head ++ nodeB.getUkNext.head ++ nodeC.getUkNext.head ), nodeD.getCkNext )
    val nPar = Node( nAdd1.getUkNext ++ nAdd2.getUkNext, node_cK.takeRight(2) ++ node_cK.dropRight(2) )
    nAdd1.addChildren( Set( nodeA, nodeB, nodeC ) )
    nAdd2.addChildren( Set( nodeB, nodeC, nodeD ) )
    nPar.addChildren( Set( nAdd1, nAdd2 ) )
    nPar.setB()
    nAdd1.setA()
    nAdd2.setA()

    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintC( nodeC ) )
    assert( Node.satisfiesConstraintC( nodeD ) )
    assert( Node.satisfiesConstraintA( nAdd1 ) )
    assert( Node.satisfiesConstraintA( nAdd2 ) )
    assert( Node.satisfiesConstraintB( nPar ) )

    val nodeList = Transforms.trySwap( nPar )._1

    for ( n <- nodeList )
      assert( Node.satisfiesConstraints( n ) )

    VerifyHardware( nodeList.toSet ++ Set( nodeA, nodeB, nodeC, nodeD ), List( nodeList(0) ) )
  }

  @Test def testSwap11C {
    val nPar = Node( Vector(Set(Vector(15, 0), Vector(14, 1)), Set(Vector(4, 7), Vector(9, 4), Vector(8, 5))), Vector(1, -1, -1, -1, -1, -1, 0, -1, -1, -1, -1, 0, -1, -1, -1, -1, 0, -1, -1, -1, -1, -1, -1, 1, 1) )
    nPar.setB()
    val n1 = Node( Vector(Set(Vector(14, 0), Vector(13, 1))), Vector(-1, -1, -1, -1, -1, 0, -1, -1, -1, -1, 0, -1, -1, -1, -1, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1) )
    n1.setA()
    val n2 = Node( Vector(Set(Vector(3, 7), Vector(8, 4), Vector(7, 5))), Vector(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0) )
    n2.setA()

    val nodeA = Node( Vector(Set(Vector(13, 0)), Set(Vector(2, 7))), Vector(-1, -1, -1, -1, 0, -1, -1, -1, -1, 0, -1, -1, -1, -1, 0, -1, -1, -1, -1, -1, -1, 1, 1, 1, -1) )
    nodeA.setB()
    val nodeB = Node( Vector(Set(Vector(12, 1)), Set(Vector(7, 4), Vector(6, 5))), Vector(-1, -1, -1, -1, 0, -1, -1, -1, -1, 0, -1, -1, -1, -1, 0, -1, -1, -1, -1, -1, -1, 1, 1, 1, -1) )
    nodeB.setB()
    nPar.addChildren( Set( n1, n2 ) )
    n1.addChildren( Set( nodeA, nodeB ) )
    n2.addChildren( Set( nodeA, nodeB ) )

    assert( Node.satisfiesConstraintA( n1 ) )
    assert( Node.satisfiesConstraintA( n2 ) )
    assert( Node.satisfiesConstraintB( nPar ) )

    val nodeList = Transforms.trySwap( nPar )._1

    for ( n <- nodeList ) {
      assert( Node.satisfiesConstraints( n ), "Node " + n + " must satisfy constraints" )
      assert( Node.isMinimal( n ), "Node must be minimal" )
    }
  }

  @Test def swapTest11D {
    // Node@1117645218(A) {
    val nAdd1 = Node( Vector(Set(Vector(11, 0), Vector(9, 2), Vector(10, 1), Vector(5, 4), Vector(4, 5), Vector(6, 3)), Set(Vector(10, 1), Vector(9, 2), Vector(5, 4), Vector(4, 5))), Vector(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1) )
    nAdd1.setA()
    // nAdd2 = Node@223718617(A) {
    val nAdd2 = Node( Vector(Set(Vector(4, 5), Vector(5, 4), Vector(10, 1), Vector(9, 2)), Set(Vector(11, 0), Vector(9, 2), Vector(10, 1), Vector(5, 4), Vector(4, 5), Vector(6, 3))), Vector(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1, 1, 1, -1, -1, -1, -1, -1, -1, -1, -1) )
    nAdd2.setA()
    // nPar = Node@2135949754(B) {
    val nPar = Node( Vector(Set(Vector(11, 1), Vector(10, 2), Vector(5, 5), Vector(6, 4)), Set(Vector(10, 2), Vector(5, 5), Vector(7, 3), Vector(11, 1), Vector(12, 0), Vector(6, 4))), Vector(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1, 1, 1, -1, -1, -1, -1, -1, -1, -1) )
    nPar.setB()
    // nAdd1.getChildren() = Set(Node@1683082425(B) {
    val nA1_0 = Node( Vector(Set(Vector(3, 5)), Set(Vector(3, 5), Vector(4, 4))), Vector(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1) )
    nA1_0.setB()
    // Node@467494393(B) {
    val nA1_1 = Node( Vector(Set(Vector(4, 4), Vector(10, 0), Vector(9, 1), Vector(8, 2), Vector(5, 3)), Set(Vector(8, 2), Vector(9, 1))), Vector(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1) )
    nA1_1.setB()
    // nAdd2.getChildren() = Set(Node@1285229702(B) {
    val nA2_0 = Node( Vector(Set(Vector(3, 5)), Set(Vector(4, 4), Vector(9, 1), Vector(8, 2))), Vector(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1) )
    nA2_0.setB()
    // Node@1465079988(B) {
    val nA2_1 = Node( Vector(Set(Vector(4, 4), Vector(10, 0), Vector(9, 1), Vector(8, 2), Vector(5, 3)), Set(Vector(3, 5))), Vector(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1) )
    nA2_1.setB()
    nAdd1.addChild( nA1_0 )
    nAdd1.addChild( nA1_1 )
    nAdd2.addChild( nA2_0 )
    nAdd2.addChild( nA2_1 )
    nPar.addChild( nAdd1 )
    nPar.addChild( nAdd2 )

    for ( n <- Set( nPar, nAdd1, nAdd2 ) ) {
      assert( Node.satisfiesConstraints( n ) )
      assert( Node.isMinimal( n ) )
    }

    val nodeList = Transforms.trySwap( nPar )._1

    for ( n <- nodeList ) {
      assert( Node.satisfiesConstraints(n), "Nodes must satisfy constraints" )
      assert( Node.isMinimal( n ), "Node must be minimal: " + n )
    }
  }

  @Test def swapTest11E {
    // nAdd1 = Node@1932039124(A) {
    val nAdd1 = Node( Vector(Set(Vector(8, 4), Vector(7, 5), Vector(3, 7), Vector(2, 8)), Set(Vector(13, 1), Vector(12, 2), Vector(8, 4), Vector(7, 5)), Set(Vector(7, 5), Vector(14, 0), Vector(12, 2), Vector(9, 3), Vector(13, 1), Vector(8, 4)), Set(Vector(14, 0), Vector(13, 1), Vector(9, 3), Vector(8, 4))), Vector(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, 2, 2, 2, 3, 0, -1, -1, -1) )
    nAdd1.setA()
    // nAdd2 = Node@1583809588(A) {
    val nAdd2 = Node( Vector(Set(Vector(13, 1), Vector(12, 2), Vector(8, 4), Vector(7, 5)), Set(Vector(7, 5), Vector(14, 0), Vector(12, 2), Vector(9, 3), Vector(13, 1), Vector(8, 4)), Set(Vector(14, 0), Vector(13, 1), Vector(9, 3), Vector(8, 4)), Set(Vector(9, 3), Vector(8, 4), Vector(4, 6), Vector(3, 7))), Vector(3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1, 1, 1, 2, -1, -1, -1, -1) )
    nAdd2.setA()
    // nPar = Node@1240197764(B) {
    val nPar = Node( Vector(Set(Vector(9, 4), Vector(8, 5), Vector(4, 7), Vector(3, 8)), Set(Vector(14, 1), Vector(13, 2), Vector(9, 4), Vector(8, 5)), Set(Vector(15, 0), Vector(13, 2), Vector(9, 4), Vector(14, 1), Vector(8, 5), Vector(10, 3)), Set(Vector(15, 0), Vector(14, 1), Vector(10, 3), Vector(9, 4)), Set(Vector(10, 3), Vector(9, 4), Vector(5, 6), Vector(4, 7))), Vector(-1, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, 2, 2, 2, 3, 0, -1, -1) )
    nPar.setB()
    // nAdd1.getChildren() = Set(Node@222667687(B) {
    val nA1_0 = Node( Vector(Set(Vector(1, 8)), Set(Vector(6, 5)), Set(Vector(13, 0), Vector(12, 1), Vector(8, 3))), Vector(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, 1, 1, 1, 2, 0, -1, -1, -1, -1) )
    nA1_0.setB()
    // Node@1544331690(B) {
    val nA1_1 = Node( Vector(Set(Vector(7, 4), Vector(6, 5), Vector(2, 7)), Set(Vector(11, 2), Vector(12, 1), Vector(8, 3), Vector(7, 4), Vector(13, 0)), Set(Vector(7, 4)), Set(Vector(11, 2), Vector(12, 1), Vector(7, 4))), Vector(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 3, 1, 1, 1, 2, 0, -1, -1, -1, -1) )
    nA1_1.setB()
    // nAdd2.getChildren() = Set(Node@1287081551(B) {
    val nA2_0 = Node( Vector(Set(Vector(2, 7)), Set(Vector(11, 2), Vector(12, 1), Vector(8, 3), Vector(7, 4), Vector(13, 0)), Set(Vector(7, 4)), Set(Vector(11, 2), Vector(12, 1), Vector(7, 4))), Vector(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 3, 1, 1, 1, 2, -1, -1, -1, -1, 0) )
    nA2_0.setB()
    // Node@376851426(B) {
    val nA2_1 = Node( Vector(Set(Vector(8, 3), Vector(7, 4), Vector(3, 6)), Set(Vector(6, 5)), Set(Vector(13, 0), Vector(12, 1), Vector(8, 3))), Vector(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, 1, 1, 1, 2, -1, -1, -1, -1, 0) )
    nA2_1.setB()
    nPar.addChildren( Set( nAdd1, nAdd2 ) )
    nAdd1.addChildren( Set( nA1_0, nA1_1 ) )
    nAdd2.addChildren( Set( nA2_0, nA2_1 ) )

    for ( n <- Set( nPar, nAdd1, nAdd2 ) ) {
      assert( Node.satisfiesConstraints( n ) )
      assert( Node.isMinimal( n ) )
    }

    val nodeList = Transforms.trySwap( nPar )._1

    for ( n <- nodeList ) {
      assert( Node.satisfiesConstraints(n), "Nodes must satisfy constraints" )
      assert( Node.isMinimal( n ), "Node must be minimal: " + n )
    }
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
    val nodeSwap = Node( nodeA.getUkNext ++ nodeB.getUkNext, node_cK1.map( x => if ( x != -1 && x < 2 ) x else -1 ) )
    val nodeOther = Node( nodeC.getUkNext ++ nodeD.getUkNext, node_cK1.map( x => if ( x < 2 ) -1 else x - 2 ) )
    nodeSwap.setB()
    nodeSwap.addChild( nodeA )
    nodeSwap.addChild( nodeB )
    nodeOther.setB()
    nodeOther.addChild( nodeC )
    nodeOther.addChild( nodeD )

    val nodeParUk = nodeSwap.getUkNext ++ nodeOther.getUkNext
    val nodePar = Node( nodeParUk, nodeParCk )
    nodePar.setB()
    nodePar.addChild( nodeSwap )
    nodePar.addChild( nodeOther )

    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintC( nodeC ) )
    assert( Node.satisfiesConstraintC( nodeD ) )
    assert( Node.satisfiesConstraintB( nodeSwap ) )
    assert( Node.satisfiesConstraintB( nodeOther ) )
    assert( Node.satisfiesConstraintB( nodePar ) )

    val nodeList = Transforms.trySwap( nodePar )._1

    assert( nodeList.size == 3 )
    assert( Node.satisfiesConstraintB( nodeList(0) ) && nodeList(0).isMux() )
    assert( Node.satisfiesConstraintB( nodeList(1) ) && nodeList(1).isMux() )
    assert( Node.satisfiesConstraintB( nodeList(2) ) && nodeList(2).isMux() )

    VerifyHardware( nodeList.toSet ++ Set( nodeA, nodeB, nodeC, nodeD ), List( nodeList(0) ) )
  }

  @Test def testSwap12B {
    val nodeAuK = genTermUk( 0 ) // termination setA
    val nodeBuK = genTermUk( 1 ) // termination setB
    val nodeCuK = genTermUk( 2 ) // termination setC
    val node_cK = genCk( 3 )
    val node_cK1 = node_cK.takeRight(1) ++ node_cK.dropRight(1)
    val node_cK2 = node_cK1.takeRight(1) ++ node_cK1.dropRight(1)
    val nodeA = Node( nodeAuK, node_cK.map( x => if ( x == 0 ) 0 else -1 ) )
    val nodeB = Node( nodeBuK, node_cK.map( x => if ( x == 1 ) 0 else -1 ) )
    val nodeC = Node( nodeCuK, node_cK.map( x => if ( x == 2 ) 0 else -1 ) )
    nodeA.setC()
    nodeB.setC()
    nodeC.setC()
    val nMux1 = Node( nodeA.getUkNext ++ nodeB.getUkNext, node_cK1.map( x => if ( x == 2 ) -1 else x ) )
    val nMux2 = Node( nodeB.getUkNext ++ nodeC.getUkNext, node_cK1.map( x => if ( x < 1 ) -1 else x - 1 ) )
    val nPar = Node( nMux1.getUkNext ++ Vector( nMux2.getUkNext(1) ), node_cK2 )
    nMux1.addChildren( Set( nodeA, nodeB ) )
    nMux2.addChildren( Set( nodeB, nodeC ) )
    nPar.addChildren( Set( nMux1, nMux2 ) )
    nMux1.setB()
    nMux2.setB()
    nPar.setB()

    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintC( nodeC ) )
    assert( Node.satisfiesConstraintB( nMux1 ) )
    assert( Node.satisfiesConstraintB( nMux2 ) )
    assert( Node.satisfiesConstraintB( nPar ) )

    val nodeList = Transforms.trySwap( nPar )._1

    assert( nodeList.size == 3 )
    for ( n <- nodeList )
      assert( Node.satisfiesConstraints( n ) )

    VerifyHardware( nodeList.toSet ++ Set( nodeA, nodeB, nodeC ), List( nodeList(0) ) )
  }

  @Test def testSwap12C {
    val nodeAuK = genTermUk( 0 ) // termination setA
    val nodeBuK = genTermUk( 1 ) // termination setB
    val node_cK = genCk( 2 )
    val node_cK1 = node_cK.takeRight(1) ++ node_cK.dropRight(1)
    val nodeA = Node( nodeAuK, node_cK.map( x => if ( x == 0 ) 0 else -1 ) )
    val nodeB = Node( nodeBuK, node_cK.map( x => if ( x == 1 ) 0 else -1 ) )
    nodeA.setC()
    nodeB.setC()
    val nMux1 = Node( nodeA.getUkNext ++ nodeB.getUkNext, node_cK1 )
    val nMux2 = Node( nodeA.getUkNext ++ nodeB.getUkNext, node_cK1 )
    val nPar = Node( nMux1.getUkNext, nMux1.getCkNext )
    nMux1.setB()
    nMux2.setB()
    nPar.setB()
    nMux1.addChild( nodeA )
    nMux1.addChild( nodeB )
    nMux2.addChild( nodeA )
    nMux2.addChild( nodeB )
    nPar.addChild( nMux1 )
    nPar.addChild( nMux2 )

    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintB( nMux1 ) )
    assert( Node.satisfiesConstraintB( nMux2 ) )
    assert( Node.satisfiesConstraintB( nPar ) )

    val nodeList = Transforms.trySwap( nPar )._1

    assert( nodeList.size == 3 )
    for ( n <- nodeList )
      assert( Node.satisfiesConstraints( n ) )

    VerifyHardware( nodeList.toSet ++ Set( nodeA, nodeB ), List( nodeList(0) ) )
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
    val nodeSwap = Node( nodeA.getUkNext, node_cK1.map( x => if ( x == 0 ) 0 else -1 ) )
    val nodeOther = Node( nodeB.getUkNext ++ nodeC.getUkNext, node_cK1.map( x => if ( x < 1 ) -1 else x - 1 ) )
    nodeSwap.setB()
    nodeSwap.addChild( nodeA )
    nodeSwap.addChild( nodeA )
    nodeOther.setB()
    nodeOther.addChild( nodeB )
    nodeOther.addChild( nodeC )

    val nodeParUk = nodeSwap.getUkNext ++ nodeOther.getUkNext
    val nodeParCkComb = nodeSwap.getCkNext.zip( nodeOther.getCkNext ).map( cks => {
      if ( cks._1 == -1 && cks._2 == -1 )
        -1
      else if ( cks._1 == -1 )
        cks._2 + 1
      else
        cks._1
    })
    val nodePar = Node( nodeParUk, nodeParCkComb )
    nodePar.setB()
    nodePar.addChild( nodeSwap )
    nodePar.addChild( nodeOther )

    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintC( nodeC ) )
    assert( Node.satisfiesConstraintB( nodeSwap ) )
    assert( Node.satisfiesConstraintB( nodeOther ) )
    assert( Node.satisfiesConstraintB( nodePar ) )

    val nodeList = Transforms.trySwap( nodePar )._1

    assert( nodeList.size == 3 )
    for ( n <- nodeList )
      assert( Node.satisfiesConstraints( n ) )

    VerifyHardware( nodeList.toSet ++ Set( nodeA, nodeB, nodeC ), List( nodeList(0) ) )
  }

  @Test def testSwap16 {
    val nodeAuK = genTermUk( 0 ) // termination setA
    val nodeBuK = genTermUk( 1 ) // termination setB
    val nodeCuK = genTermUk( 2 ) // termination setC
    val nodeDuK = genTermUk( 3 ) // termination setD
    val node_cK = genCk( 2 )
    val node_cK1 = node_cK.drop(1) ++ node_cK.take(1)
    val nodeA = Node( nodeAuK, node_cK1.map( x => if ( x == -1 ) -1 else 0 ) )
    val nodeB = Node( nodeBuK, node_cK1.map( x => if ( x == -1 ) -1 else 0 ) )
    val nodeC = Node( nodeCuK, node_cK1.map( x => if ( x == 0 ) 0 else -1 ) )
    val nodeD = Node( nodeDuK, node_cK1.map( x => if ( x == 1 ) 0 else -1 ) )
    nodeA.setC()
    nodeB.setC()
    nodeC.setC()
    nodeD.setC()
    val nReg1 = Node( nodeA.getUkNext, nodeA.getCkNext )
    val nReg2 = Node( nodeB.getUkNext, nodeB.getCkNext )
    val nMux = Node( nodeC.getUkNext ++ nodeD.getUkNext, node_cK )
    nReg1.addChild( nodeA )
    nReg2.addChild( nodeB )
    nMux.addChild( nodeC )
    nMux.addChild( nodeD )
    nReg1.setB()
    nReg2.setB()
    nMux.setB()
    val nPar = Node( nMux.getUkNext.map( uki => uki ++ nReg1.getUkNext.head ++ nReg2.getUkNext.head ), nMux.getCkNext )
    nPar.setA()
    nPar.addChild( nMux )
    nPar.addChild( nReg1 )
    nPar.addChild( nReg2 )

    assert( Node.satisfiesConstraintC( nodeA ) )
    assert( Node.satisfiesConstraintC( nodeB ) )
    assert( Node.satisfiesConstraintC( nodeC ) )
    assert( Node.satisfiesConstraintC( nodeD ) )
    assert( Node.satisfiesConstraintB( nReg1 ) )
    assert( Node.satisfiesConstraintB( nReg2 ) )
    assert( Node.satisfiesConstraintB( nMux ) )
    assert( Node.satisfiesConstraintA( nPar ) )

    val nodeList = Transforms.trySwap( nPar )._1

    assert( nodeList.size == 3 )
    assert( Node.satisfiesConstraintB( nodeList(0) ) && nodeList(0).isMux() )
    assert( Node.satisfiesConstraintA( nodeList(1) ) && nodeList(1).isAdd3() )
    assert( Node.satisfiesConstraintA( nodeList(2) ) && nodeList(2).isAdd3() )

    VerifyHardware( nodeList.toSet ++ Set( nodeA, nodeB, nodeC, nodeD ), List( nodeList(0) ) )
  }

  @Test def testSwap18 {
    val nodesInUk = ( 0 until 5 ).map( i => genTermUk( i ) ).toList
    val node_cK = genCk( 2 )
    val node_cK1 = node_cK.drop(1) ++ node_cK.take(1)
    val nodesABC = nodesInUk.take(3).map( uk => {
      val n = Node( uk, node_cK1.map( x => if ( x == -1 ) -1 else 0 ) )
      n.setC()
      n
    })
    val nodesDE = nodesInUk.drop(3).zipWithIndex.map( uk => {
      val n = Node( uk._1, node_cK1.map( x => if ( x == uk._2 ) 0 else -1 ) )
      n.setC()
      n
    })
    val nMux = Node( nodesDE.map( _.getUkNext ).reduce( _++_ ), node_cK )
    nMux.addChildren( nodesDE )
    nMux.setB()
    val nReg = Node( nodesABC(0).getUkNext, nodesABC(0).getCkNext )
    nReg.addChild( nodesABC(0) )
    nReg.setB()
    val nAdd = Node( Vector( nodesABC.drop(1).map( _.getUkNext.head ).reduce( _ ++ _ ) ),
      nodesABC(1).getCkNext )
    nAdd.addChildren( nodesABC.drop(1) )
    nAdd.setA()
    val nPar = Node( nMux.getUkNext.map( uk => uk ++ nAdd.getUkNext.head ++ nReg.getUkNext.head ),
      nMux.getCkNext )
    nPar.addChildren( Set( nMux, nReg, nAdd ) )
    nPar.setA()

    for ( n <- nodesABC ++ nodesDE )
      assert( Node.satisfiesConstraintC( n ) )
    assert( Node.satisfiesConstraintB( nReg ) )
    assert( Node.satisfiesConstraintA( nAdd ) )
    assert( Node.satisfiesConstraintB( nMux ) )
    assert( Node.satisfiesConstraintA( nPar ) )

    val nodeList = Transforms.trySwap( nPar )._1

    assert( nodeList.size == 3 )
    assert( Node.satisfiesConstraintA( nodeList(0) ) && nodeList(0).isAdd2() )
    assert( Node.satisfiesConstraintA( nodeList(1) ) && nodeList(1).isAdd3() )
    assert( Node.satisfiesConstraintB( nodeList(2) ) && nodeList(2).isMux() )

    VerifyHardware( nodeList.toSet ++ nodesABC.toSet ++ nodesDE.toSet, List( nodeList(0) ) )
  }

  @Test def testSwap18B {
    val nodesInUk = ( 0 until 6 ).map( i => genTermUk( i ) ).toList
    val node_cK = genCk( 2 )
    val node_cK1 = node_cK.drop(1) ++ node_cK.take(1)
    val nodesABCD = nodesInUk.take(4).map( uk => {
      val n = Node( uk, node_cK1.map( x => if ( x == -1 ) -1 else 0 ) )
      n.setC()
      n
    })
    val nodesEF = nodesInUk.drop(4).zipWithIndex.map( uk => {
      val n = Node( uk._1, node_cK1.map( x => if ( x == uk._2 ) 0 else -1 ) )
      n.setC()
      n
    })
    val nMux = Node( nodesEF.map( _.getUkNext ).reduce( _++_ ), node_cK )
    nMux.addChildren( nodesEF )
    nMux.setB()
    val nReg = Node( nodesABCD(0).getUkNext, nodesABCD(0).getCkNext )
    nReg.addChild( nodesABCD(0) )
    nReg.setB()
    val nAdd = Node( Vector( nodesABCD.drop(1).map( _.getUkNext.head ).reduce( _ ++ _ ) ),
      nodesABCD(1).getCkNext )
    nAdd.addChildren( nodesABCD.drop(1) )
    nAdd.setA()
    val nPar = Node( nMux.getUkNext.map( uk => uk ++ nAdd.getUkNext.head ++ nReg.getUkNext.head ),
      nMux.getCkNext )
    nPar.addChildren( Set( nMux, nReg, nAdd ) )
    nPar.setA()

    for ( n <- nodesABCD ++ nodesEF )
      assert( Node.satisfiesConstraintC( n ) )
    assert( Node.satisfiesConstraintB( nReg ) )
    assert( Node.satisfiesConstraintA( nAdd ) )
    assert( Node.satisfiesConstraintB( nMux ) )
    assert( Node.satisfiesConstraintA( nPar ) )

    val nodeList = Transforms.trySwap( nPar )._1

    assert( nodeList.size == 4 )
    assert( Node.satisfiesConstraintA( nodeList(0) ) && nodeList(0).isAdd3() )
    assert( Node.satisfiesConstraintA( nodeList(1) ) && nodeList(1).isAdd2() )
    assert( Node.satisfiesConstraintA( nodeList(2) ) && nodeList(2).isAdd2() )
    assert( Node.satisfiesConstraintB( nodeList(3) ) && nodeList(3).isMux() )

    VerifyHardware( nodeList.toSet ++ nodesABCD.toSet ++ nodesEF.toSet, List( nodeList(0) ) )
  }

  @Test def testSwap19 {
    val nodesInUk = ( 0 until 7 ).map( i => genTermUk( i ) ).toList
    val node_cK = genCk( 2 )
    val node_cK1 = node_cK.drop(1) ++ node_cK.take(1)
    val nodesAdds = nodesInUk.take(5).map( uk => {
      val n = Node( uk, node_cK1.map( x => if ( x == -1 ) -1 else 0 ) )
      n.setC()
      n
    })
    val nodesMux = nodesInUk.drop(5).zipWithIndex.map( uk => {
      val n = Node( uk._1, node_cK1.map( x => if ( x == uk._2 ) 0 else -1 ) )
      n.setC()
      n
    })
    val nMux = Node( nodesMux.map( _.getUkNext ).reduce( _++_ ), node_cK )
    nMux.addChildren( nodesMux )
    nMux.setB()
    val nAdd1 = Node( Vector( nodesAdds.take(3).map( _.getUkNext.head ).reduce( _ ++ _ ) ),
      nodesAdds(0).getCkNext )
    nAdd1.addChildren( nodesAdds.take(3) )
    nAdd1.setA()
    val nAdd2 = Node( Vector( nodesAdds.drop(3).map( _.getUkNext.head ).reduce( _ ++ _ ) ),
      nodesAdds(3).getCkNext )
    nAdd2.addChildren( nodesAdds.drop(3) )
    nAdd2.setA()
    val nPar = Node( nMux.getUkNext.map( uk => uk ++ nAdd1.getUkNext.head ++ nAdd2.getUkNext.head ),
      nMux.getCkNext )
    nPar.addChildren( Set( nMux, nAdd1, nAdd2 ) )
    nPar.setA()

    for ( n <- nodesAdds ++ nodesMux )
      assert( Node.satisfiesConstraintC( n ) )
    assert( Node.satisfiesConstraintA( nAdd2 ) )
    assert( Node.satisfiesConstraintA( nAdd1 ) )
    assert( Node.satisfiesConstraintB( nMux ) )
    assert( Node.satisfiesConstraintA( nPar ) )

    val nodeList = Transforms.trySwap( nPar )._1

    assert( nodeList.size == 4 )
    assert( Node.satisfiesConstraintA( nodeList(0) ) && nodeList(0).isAdd3() )
    assert( Node.satisfiesConstraintB( nodeList(1) ) && nodeList(1).isMux() )
    assert( Node.satisfiesConstraintA( nodeList(2) ) && nodeList(2).isAdd3() )
    assert( Node.satisfiesConstraintA( nodeList(3) ) && nodeList(3).isAdd2() )

    VerifyHardware( nodeList.toSet ++ nodesAdds.toSet ++ nodesMux.toSet, List( nodeList(0) ) )
  }

  @Test def testSwap19B {
    val nodesInUk = ( 0 until 6 ).map( i => genTermUk( i ) ).toList
    val node_cK = genCk( 2 )
    val node_cK1 = node_cK.drop(1) ++ node_cK.take(1)
    val nodesAdds = nodesInUk.take(4).map( uk => {
      val n = Node( uk, node_cK1.map( x => if ( x == -1 ) -1 else 0 ) )
      n.setC()
      n
    })
    val nodesMux = nodesInUk.drop(4).zipWithIndex.map( uk => {
      val n = Node( uk._1, node_cK1.map( x => if ( x == uk._2 ) 0 else -1 ) )
      n.setC()
      n
    })
    val nMux = Node( nodesMux.map( _.getUkNext ).reduce( _++_ ), node_cK )
    nMux.addChildren( nodesMux )
    nMux.setB()
    val nAdd1 = Node( Vector( nodesAdds.take(2).map( _.getUkNext.head ).reduce( _ ++ _ ) ),
      nodesAdds(0).getCkNext )
    nAdd1.addChildren( nodesAdds.take(2) )
    nAdd1.setA()
    val nAdd2 = Node( Vector( nodesAdds.drop(2).map( _.getUkNext.head ).reduce( _ ++ _ ) ),
      nodesAdds(3).getCkNext )
    nAdd2.addChildren( nodesAdds.drop(2) )
    nAdd2.setA()
    val nPar = Node( nMux.getUkNext.map( uk => uk ++ nAdd1.getUkNext.head ++ nAdd2.getUkNext.head ),
      nMux.getCkNext )
    nPar.addChildren( Set( nMux, nAdd1, nAdd2 ) )
    nPar.setA()

    for ( n <- nodesAdds ++ nodesMux )
      assert( Node.satisfiesConstraintC( n ) )
    assert( Node.satisfiesConstraintA( nAdd2 ) )
    assert( Node.satisfiesConstraintA( nAdd1 ) )
    assert( Node.satisfiesConstraintB( nMux ) )
    assert( Node.satisfiesConstraintA( nPar ) )

    val nodeList = Transforms.trySwap( nPar )._1

    assert( nodeList.size == 4 )
    assert( Node.satisfiesConstraintA( nodeList(0) ) && nodeList(0).isAdd3() )
    assert( Node.satisfiesConstraintB( nodeList(1) ) && nodeList(1).isMux() )
    assert( Node.satisfiesConstraintB( nodeList(2) ) && nodeList(2).isReg() )
    assert( Node.satisfiesConstraintA( nodeList(3) ) && nodeList(3).isAdd3() )

    VerifyHardware( nodeList.toSet ++ nodesAdds.toSet ++ nodesMux.toSet, List( nodeList(0) ) )
  }

  @Test def testSwap20 {
    val nodesInUk = ( 0 until 6 ).map( i => genTermUk( i ) ).toList
    val node_cK = genCk( 1 )
    val nodesIn = nodesInUk.map( uk => {
      val n = Node( uk, node_cK )
      n.setC()
      n
    })
    val nReg = Node( nodesIn.head.getUkNext, nodesIn.head.getCkNext )
    nReg.addChild( nodesIn.head )
    nReg.setB()
    val nAdd1 = Node( Vector( nodesIn.tail.take(2).map( _.getUkNext.head ).reduce( _ ++ _ ) ),
      nodesIn.tail(0).getCkNext )
    nAdd1.addChildren( nodesIn.tail.take(2) )
    nAdd1.setA()
    val nAdd2 = Node( Vector( nodesIn.tail.drop(2).map( _.getUkNext.head ).reduce( _ ++ _ ) ),
      nodesIn.tail(3).getCkNext )
    nAdd2.addChildren( nodesIn.tail.drop(2) )
    nAdd2.setA()
    val nPar = Node( Vector( nReg.getUkNext.head ++ nAdd1.getUkNext.head ++ nAdd2.getUkNext.head ),
      nReg.getCkNext )
    nPar.addChildren( Set( nReg, nAdd1, nAdd2 ) )
    nPar.setA()

    for ( n <- nodesIn )
      assert( Node.satisfiesConstraintC( n ) )
    assert( Node.satisfiesConstraintA( nAdd2 ) )
    assert( Node.satisfiesConstraintA( nAdd1 ) )
    assert( Node.satisfiesConstraintB( nReg ) )
    assert( Node.satisfiesConstraintA( nPar ) )

    val nodeList = Transforms.trySwap( nPar )._1

    assert( nodeList.size == 3 )
    assert( Node.satisfiesConstraintA( nodeList(0) ) && nodeList(0).isAdd2() )
    assert( Node.satisfiesConstraintA( nodeList(1) ) && nodeList(1).isAdd3() )
    assert( Node.satisfiesConstraintA( nodeList(2) ) && nodeList(2).isAdd3() )

    VerifyHardware( nodeList.toSet ++ nodesIn.toSet, List( nodeList(0) ) )
  }

  @Test def testSwap20B {
    val nodesInUk = ( 0 until 7 ).map( i => genTermUk( i ) ).toList
    val node_cK = genCk( 1 )
    val nodesIn = nodesInUk.map( uk => {
      val n = Node( uk, node_cK )
      n.setC()
      n
    })
    val nReg = Node( nodesIn.head.getUkNext, nodesIn.head.getCkNext )
    nReg.addChild( nodesIn.head )
    nReg.setB()
    val nAdd1 = Node( Vector( nodesIn.tail.take(3).map( _.getUkNext.head ).reduce( _ ++ _ ) ),
      nodesIn.tail(0).getCkNext )
    nAdd1.addChildren( nodesIn.tail.take(3) )
    nAdd1.setA()
    val nAdd2 = Node( Vector( nodesIn.tail.drop(3).map( _.getUkNext.head ).reduce( _ ++ _ ) ),
      nodesIn.tail(3).getCkNext )
    nAdd2.addChildren( nodesIn.tail.drop(3) )
    nAdd2.setA()
    val nPar = Node( Vector( nReg.getUkNext.head ++ nAdd1.getUkNext.head ++ nAdd2.getUkNext.head ),
      nReg.getCkNext )
    nPar.addChildren( Set( nReg, nAdd1, nAdd2 ) )
    nPar.setA()

    for ( n <- nodesIn )
      assert( Node.satisfiesConstraintC( n ) )
    assert( Node.satisfiesConstraintA( nAdd2 ) )
    assert( Node.satisfiesConstraintA( nAdd1 ) )
    assert( Node.satisfiesConstraintB( nReg ) )
    assert( Node.satisfiesConstraintA( nPar ) )

    val nodeList = Transforms.trySwap( nPar )._1

    assert( nodeList.size == 4 )
    assert( Node.satisfiesConstraintA( nodeList(0) ) && nodeList(0).isAdd3() )
    assert( Node.satisfiesConstraintA( nodeList(1) ) && nodeList(1).isAdd2() )
    assert( Node.satisfiesConstraintA( nodeList(2) ) && nodeList(2).isAdd2() )
    assert( Node.satisfiesConstraintA( nodeList(3) ) && nodeList(3).isAdd3() )

    VerifyHardware( nodeList.toSet ++ nodesIn.toSet, List( nodeList(0) ) )
  }

  @Test def testSwap21 {
    val nodesInUk = ( 0 until 5 ).map( i => genTermUk( i ) ).toList
    val node_cK = genCk( 1 )
    val nodesIn = nodesInUk.map( uk => {
      val n = Node( uk, node_cK )
      n.setC()
      n
    })
    val nReg = Node( nodesIn(0).getUkNext, nodesIn(0).getCkNext )
    nReg.addChild( nodesIn(0) )
    nReg.setB()
    val nReg2 = Node( nodesIn(1).getUkNext, nodesIn(1).getCkNext )
    nReg2.addChild( nodesIn(1) )
    nReg2.setB()
    val nAdd = Node( Vector( nodesIn.drop(2).map( _.getUkNext.head ).reduce( _ ++ _ ) ),
      nodesIn.tail(2).getCkNext )
    nAdd.addChildren( nodesIn.drop(2) )
    nAdd.setA()
    val nPar = Node( Vector( nReg.getUkNext.head ++ nReg2.getUkNext.head ++ nAdd.getUkNext.head ),
      nReg.getCkNext )
    nPar.addChildren( Set( nReg, nReg2, nAdd ) )
    nPar.setA()

    for ( n <- nodesIn )
      assert( Node.satisfiesConstraintC( n ) )
    assert( Node.satisfiesConstraintB( nReg2 ) )
    assert( Node.satisfiesConstraintA( nAdd ) )
    assert( Node.satisfiesConstraintB( nReg ) )
    assert( Node.satisfiesConstraintA( nPar ) )

    val nodeList = Transforms.trySwap( nPar )._1

    assert( nodeList.size == 3 )
    assert( Node.satisfiesConstraintA( nodeList(0) ) && nodeList(0).isAdd2() )
    assert( Node.satisfiesConstraintA( nodeList(1) ) && nodeList(1).isAdd3() )
    assert( Node.satisfiesConstraintA( nodeList(2) ) && nodeList(2).isAdd2() )

    VerifyHardware( nodeList.toSet ++ nodesIn.toSet, List( nodeList(0) ) )
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
              if ( num < 45  )
                1
              else if ( num < 90 )
                -1
              else
                0
            }
            trinary
          }).to[Seq]
        }).to[Seq]
      }).to[Seq]
    }.to[Seq]
    AnnealingSolver.binFilterToCp( convFilter, imgSize, throughput )
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

    nodePar.addChild( node )
    node.addChild( nOther )
    node.addChild( nSwap )
    nSwap.addChild( nSwapL )
    nSwap.addChild( nSwapR )
    nOther.addChild( nOtherL )
    nOther.addChild( nOtherR )

    val nodeSeq = Seq( node, nSwap, nOther )
    for ( n <- nodeSeq )
      assert( Node.isMinimal( n ), "node " + n + " should be minimal" )

    val res = Transforms.trySwap( node, true )

    // clean up parents of merged nodes
    nSwap.removeChildren()
    nOther.removeChildren()

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

    nodePar.addChild( node )
    node.addChild(  nSwap  )
    node.addChild(  nOther  )
    nSwap.addChild( nSwapL )
    nSwap.addChild( nSwapR )

    val nodeSeq = Seq( node, nSwap, nOther )
    for ( n <- nodeSeq )
      assert( Node.isMinimal( n ), "node " + n + " should be minimal" )

    val res = Transforms.trySwap( node, true )

    // clean up parents of merged nodes
    nSwap.removeChildren()
    nOther.removeChildren()

    res._1.foreach( n => assert( Node.isMinimal( n ), "node " + n + " should be minimal" ) )

  }

  @Test def minimalTest3 {
    val nodeA_uk = Vector(Set(Vector(2, 7)))
    val nodeB_uk = Vector(Set(Vector(6, 5), Vector(11, 2), Vector(12, 1), Vector(8, 3), Vector(7, 4), Vector(13, 0), Vector(3, 6)))
    val nodeC_uk = Vector( Set(Vector(12, 2), Vector(7, 5), Vector(13, 1), Vector(8, 4) ))
    val node_cK = Vector(-1, 2, 1, 1, 1, -1, 2, 1, 1, 1, -1, 2, 1, 1, 1, -1, -1, -1, -1, -1, -1, 0, -1, -1, -1)
    val node_cK1 = node_cK.drop(1) ++ node_cK.take(1)
    val node_cK2 = node_cK1.drop(1) ++ node_cK1.take(1)

    val nodeA = Node( nodeA_uk, node_cK2.map( x => if ( x == 0 ) 0 else -1 ) )
    val nodeB = Node( nodeB_uk, node_cK2.map( x => if ( x == 1 ) 0 else -1 ) )
    val nodeC = Node( nodeC_uk, node_cK2.map( x => if ( x == 2 ) 0 else -1 ) )
    val nReg = Node( nodeA.getUkNext, nodeA.getCkNext )
    nReg.addChild( nodeA )
    nReg.setB()
    val nMux = Node( nodeB.getUkNext ++ nodeC.getUkNext, node_cK1.map( x => if ( x < 1 ) -1 else x - 1 ) )
    nMux.addChild( nodeB )
    nMux.addChild( nodeC )
    nMux.setB()
    val nPar = Node( nReg.getUkNext ++ nMux.getUkNext, node_cK )
    nPar.addChild( nReg )
    nPar.addChild( nMux )
    nPar.setB()

    assert( Node.satisfiesConstraintB( nReg ) )
    assert( Node.satisfiesConstraintB( nMux ) )
    assert( Node.satisfiesConstraintB( nPar ) )

    val nodeList = Transforms.trySwap( nPar )._1

    assert( nodeList.size == 3 )
    for ( n <- nodeList )
      assert( Node.satisfiesConstraints(n), "Nodes must satisfy constraints" )
    for ( n <- nodeList )
      assert( Node.isMinimal( n ), "Node must be minimal: " + n )
  }

  @Test def minimalTest4 {
    val nPar = Node( Vector(Set(Vector(9, 4)), Set(Vector(15, 0), Vector(5, 6), Vector(13, 2), Vector(9, 4), Vector(10, 3))), Vector(-1, -1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, -1, 0, 0, 0, -1, -1, -1, -1) )
    val n1Child = Node( Vector(Set(Vector(8, 4))), Vector(-1, 0, -1, -1, -1, 0, 0, -1, -1, -1, 0, 0, -1, -1, -1, 0, -1, 0, 0, 0, -1, -1, -1, -1, -1) )
    val n2Child = Node( Vector(Set(Vector(14, 0), Vector(12, 2), Vector(9, 3), Vector(4, 6), Vector(8, 4))), Vector(-1, -1, 0, 0, 0, -1, -1, 0, 0, 0, -1, -1, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1) )
    val nodeA = Node( Vector(Set(Vector(7, 4))), Vector(0, -1, -1, -1, -1, 0, -1, -1, -1, -1, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1) )
    val nodeB = Node( Vector(Set(Vector(2, 7)), Set(Vector(7, 4))), Vector(0, -1, -1, -1, 1, 0, -1, -1, -1, 1, 0, -1, -1, -1, 1, -1, 1, 1, 1, -1, -1, -1, -1, -1, -1) )
    val nodeC = Node( Vector(Set(Vector(11, 2), Vector(8, 3), Vector(7, 4), Vector(13, 0), Vector(3, 6))), Vector(-1, 0, 0, 0, -1, -1, 0, 0, 0, -1, -1, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1) )
    nodeA.setB()
    nodeB.setB()
    nodeC.setA()
    n1Child.setB()
    n2Child.setB()
    nPar.setB()
    n1Child.addChild( nodeA )
    n1Child.addChild( nodeB )
    n2Child.addChild( nodeC )
    nPar.addChild( n1Child )
    nPar.addChild( n2Child )

    assert( Node.satisfiesConstraintB( n1Child ) )
    assert( Node.satisfiesConstraintB( n2Child ) )
    assert( Node.satisfiesConstraintB( nPar ) )

    val nodeList = Transforms.trySwap( nPar )._1

    assert( nodeList.size == 3 )
    assert( nodeList(0).isMux() )
    for ( n <- nodeList )
      assert( Node.satisfiesConstraints(n), "Nodes must satisfy constraints" )
    for ( n <- nodeList.drop(1) )
      assert( Node.isMinimal( n ), "Node must be minimal: " + n )

  }

  @Test def minimalTest5 {
    val nPar = Node( Vector(Set(Vector(7, 6), Vector(10, 5), Vector(11, 4), Vector(12, 3)), Set(Vector(6, 7)), Set(Vector(10, 5), Vector(12, 3), Vector(11, 4)), Set(Vector(11, 4)), Set(Vector(12, 3), Vector(17, 0)), Set(Vector(17, 0)), Set(Vector(16, 1), Vector(11, 4))), Vector(0, 0, 0, 1, 1, 2, 2, 2, 1, 1, 2, 2, 2, 1, 1, 2, 2, 2, 1, 6, 4, 4, 4, 5, 3) )
    val n1 = Node( Vector(Set(Vector(9, 5), Vector(10, 4), Vector(11, 3)), Set(Vector(16, 0), Vector(11, 3)), Set(Vector(6, 6), Vector(9, 5), Vector(11, 3), Vector(10, 4))), Vector(2, 2, -1, -1, 0, 0, 0, -1, -1, 0, 0, 0, -1, -1, 0, 0, 0, -1, -1, 1, 1, 1, -1, -1, 2) )
    val n2 = Node( Vector(Set(Vector(10, 4)), Set(Vector(5, 7)), Set(Vector(16, 0)), Set(Vector(10, 4), Vector(15, 1))), Vector(-1, -1, 1, 1, -1, -1, -1, 1, 1, -1, -1, -1, 1, 1, -1, -1, -1, 1, 3, -1, -1, -1, 2, 0, -1) )
    n1.setB()
    n2.setB()
    nPar.setB()
    nPar.addChildren( Set( n1, n2 ) )

    val nodeA = Node( Vector(Set(Vector(8, 5), Vector(9, 4), Vector(10, 3)), Set(Vector(15, 0), Vector(10, 3)), Set(Vector(5, 6), Vector(8, 5), Vector(10, 3), Vector(9, 4))), Vector(2, -1, -1, 0, 0, 0, -1, -1, 0, 0, 0, -1, -1, 0, 0, 0, -1, -1, 1, 1, 1, -1, -1, 2, 2) )
    val nodeB = Node( Vector(Set(Vector(9, 4)), Set(Vector(4, 7)), Set(Vector(8, 5))), Vector(-1, 0, 1, -1, -1, -1, 1, 1, -1, -1, -1, 1, 1, -1, -1, -1, 1, 2, -1, -1, -1, 0, 0, -1, -1) )
    val nodeC = Node( Vector(Set(Vector(15, 0)), Set(Vector(9, 4), Vector(14, 1)), Set(Vector(4, 7))), Vector(-1, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, -1, -1, -1, 0, -1, -1, -1) )
    nodeA.setB()
    nodeB.setB()
    nodeC.setB()
    n2.addChildren( Set( nodeB, nodeC ) )
    n1.addChild( nodeA )

    assert( Node.satisfiesConstraintB( n1 ) )
    assert( Node.satisfiesConstraintB( n2 ) )
    assert( Node.satisfiesConstraintB( nPar ) )

    val nodeList = Transforms.trySwap( nPar )._1

    assert( nodeList.size == 3 )
    assert( nodeList(0).isMux() )
    for ( n <- nodeList )
      assert( Node.satisfiesConstraints(n), "Nodes must satisfy constraints" )
    for ( n <- nodeList.drop(1) )
      assert( Node.isMinimal( n ), "Node must be minimal: " + n )
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

    mux1.addChild( nodeA )
    mux1.addChild( nodeB )
    mux2.addChild( mux1 )
    mux2.addChild( nodeC )
    mux3.addChild( mux2 )
    mux3.addChild( nodeD )

    reg1.addChild( mux1 )
    reg1.addChild( mux1 )

    val nodes = Set( nodeA, nodeB, nodeC, nodeD, mux1, mux2, mux3, reg1 )

    // test constraints
    assert( VerifyHardware.testLinks( nodes ), "Nodes must be connected properly" )
    for ( n <- nodes )
      assert( Node.satisfiesConstraints(n), "Node " + n + " must satisfy constraints" )

    // test hardware
    VerifyHardware( nodes, Vector( reg1, mux3 ) )
  }

  @Test def testVerifyHardware {
    val nPar = Node( Vector(Set(Vector(2, 0), Vector(2, 1), Vector(2, 2), Vector(2, 3))), Vector(-1, -1, 0, 0, -1, -1, 0, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, -1, 0) )
    nPar.setA()
    val nAdd = Node( Vector(Set(Vector(1, 1), Vector(1, 2), Vector(1, 3))), Vector(-1, 0, 0, -1, -1, 0, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, -1, 0, -1) )
    nAdd.setA()
    val nReg = Node( Vector(Set(Vector(1, 0))), Vector(-1, 0, 0, -1, -1, 0, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, -1, 0, -1) )
    nReg.setB()
    nPar.addChild( nAdd )
    nPar.addChild( nReg )
    val nIn = ( 0 until 4 ).toList.map( i =>
      Node( Vector(Set(Vector(0, i))), Vector( 0, 0, -1, -1, 0, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, -1, 0, -1, -1 ) )
    )
    for ( n <- nIn )
      n.setC()
    nReg.addChild( nIn.head )
    nAdd.addChildren( nIn.tail )

    VerifyHardware( Set( nPar, nAdd, nReg ) ++ nIn.toSet, List( nPar ) )
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

  @Test def mergeTest {
    // Node Node@959512813(B) { Vector(Set(Vector(6, 4))) } { Vector(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, -1, -1, -1, -1, -1) } should satisfy constraints after merge of
    // Node@1506986179(B) {
    val node = Node( Vector(Set(Vector(2, 7))), Vector(0, -1, -1, -1, -1, 0, -1, -1, -1, -1, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0) )
    node.setB()
    // and Node@1881595643(B) {
    val selNode = Node( Vector(Set(Vector(7, 4)), Set(Vector(2, 7))), Vector(-1, -1, -1, -1, 1, -1, -1, -1, -1, 1, -1, -1, -1, -1, 1, -1, -1, -1, -1, -1, 0, -1, -1, -1, -1) )
    selNode.setB()
    // has parents Set(Node@482083279(B) { Vector(Set(Vector(2, 7)), Set(Vector(7, 4))) } { Vector(0, -1, -1, -1, 0, 0, -1, -1, -1, 0, 0, -1, -1, -1, 0, -1, -1, -1, -1, -1, 1, -1, -1, -1, 0) })
    // node children Set(Node@1860458806(B) {
    val nodeA = Node( Vector(Set(Vector(1, 7))), Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 0) )
    nodeA.setB()
    // node parents Vector(Node@1250103303(B) {
    val nPar1 = Node( Vector(Set(Vector(3, 7))), Vector(0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1) )
    nPar1.setB()
    // Node@1729078159(B) {
    val nPar2 = Node( Vector(Set(Vector(8, 4)), Set(Vector(7, 5)), Set(Vector(3, 7))), Vector(-1, 2, -1, -1, -1, -1, 2, -1, -1, -1, -1, 2, -1, -1, -1, -1, 0, 0, 0, 0, 0, 1, -1, -1, -1) )
    nPar2.setB()
    // sel children Set(Node@959512813(B) {
    val nodeB = Node( Vector(Set(Vector(6, 4))), Vector(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, -1, -1, -1, -1, -1) )
    nodeB.setB()
    // Node@1860458806(B) { == nodeA Vector(Set(Vector(1, 7))) } { Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 0) })
    // sel parents Vector(Node@1447738371(B) {
    val selPar = Node( Vector(Set(Vector(8, 4)), Set(Vector(3, 7))), Vector(-1, -1, -1, -1, -1, 1, -1, -1, -1, -1, 1, -1, -1, -1, -1, 1, -1, -1, -1, -1, -1, 0, -1, -1, -1) )
    selPar.setB()

    node.addChild( nodeA )
    selNode.addChild( nodeA )
    selNode.addChild( nodeB )
    nPar1.addChild( node )
    nPar2.addChild( node )
    selPar.addChild( selNode )

    assert( Node.satisfiesConstraints( node ) )
    assert( Node.satisfiesConstraints( selNode ) )

    val nodeList = Transforms.tryMerge( node, selNode )

    assert( nodeList.isDefined )
    for ( n <- List( nodeList.get ) )
      assert( Node.satisfiesConstraints( n ), "Node " + n )
  }

  @Test def conv3n5 {
    val imgSize = 5
    val filterSize = 3
    val cpCoords = VerifyHardware.getConvSums( imgSize, filterSize ).zipWithIndex.map( cSet => {
      cSet._1.map( v => { Vector( cSet._2 - v(0)) ++ v.drop(1) }.to[Seq] )
    }).toVector.to[Seq]
    val latAdd = AnnealingSolver.needLatency( Vector( cpCoords ).to[Seq] )
    println( "latAdd = " + latAdd )
    val cp = cpCoords.map( cSet => {
      cSet.map( v => { Vector( latAdd + v(0) ) ++ v.drop(1) }.to[Seq])
    })
    var nodes = AnnealingSolver.init( Vector( cp ).to[Seq] )._1
    nodes = AnnealingSolver.runPar( nodes, 100, 1000000, true )
    AnnealingSolver.toDot( nodes, "conv3n5.dot" )
    println( "cost = " + nodes.size )
    val outNodes = nodes.filter( _.parentsIsEmpty() ).toVector
    VerifyHardware( nodes, outNodes )
  }

  /*
  @Test def trinaryLayer {
    val filterSize = ( 3, 3, 3, 128 )
    val imgSize = ( 32, 32 )
    val initNodes = AnnealingSolver.init( genTrinary( filterSize, imgSize, 2 ) )._1
    println( "created " + initNodes.size + " nodes")
    val nodes = AnnealingSolver.runPar( initNodes, 100000000, 1000000 )
    assert( VerifyHardware.testLinks( nodes ), "Nodes must be connected properly" )
    for ( n <- nodes )
      assert( Node.satisfiesConstraints(n), "Nodes must satisfy constraints" )
    AnnealingSolver.toDot( nodes, "trinary.dot" )
    println( "cost = " + nodes.size )
  }

  @Test def binarizedConv {

    // read in a conv filter
    val filename = "src/main/resources/conv1.csv"
    val conv = AnnealingSolver.readCsv( filename )
    val ( initNodes, outNodes, x ) = AnnealingSolver.init( AnnealingSolver.binFilterToCp( conv, ( 32, 32 ) ) )
    println( "created " + initNodes.size + " nodes")
    val nodes = AnnealingSolver.runPar( initNodes, 1000, 1000000 )
    assert( VerifyHardware.testLinks( nodes ), "Nodes must be connected properly" )
    for ( n <- nodes )
      assert( Node.satisfiesConstraints(n), "Nodes must satisfy constraints" )
    AnnealingSolver.toDot( nodes, "binConv2.dot" )
    println( "cost = " + nodes.size )
    val parNodes = outNodes.toVector
    VerifyHardware( nodes, parNodes )
  }
   */
}
