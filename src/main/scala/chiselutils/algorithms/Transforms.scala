/** This file implements valid transformations on
  * a different numbers of Nodes
  */
package chiselutils.algorithms

import collection.mutable.ArrayBuffer
import scala.util.Random

object Transforms {

  /** Combine two uks into 1 and return the index mapping to them
    */
  def commonMapping( uKa : Seq[Set[Seq[Int]]], uKb : Seq[Set[Seq[Int]]]) :
      ( Seq[Set[Seq[Int]]], Seq[Int], Seq[Int] ) = {
    var aIdx = 0
    var bIdx = 0
    val aIdxMap = ArrayBuffer[Int]()
    val bIdxMap = ArrayBuffer[Int]()
    val uKc = ArrayBuffer[Set[Seq[Int]]]()
    while ( aIdx < uKa.size || bIdx < uKb.size ) {
      if ( bIdx == uKb.size || ( aIdx < uKa.size && uKa(aIdx).hashCode < uKb(bIdx).hashCode ) ) {
        aIdxMap += uKc.size
        uKc += uKa( aIdx )
        aIdx += 1
      } else {
        bIdxMap += uKc.size
        if ( aIdx < uKa.size && uKa( aIdx ) == uKb( bIdx ) ) {
          aIdx += 1
          aIdxMap += uKc.size 
        }
        uKc += uKb( bIdx )
        bIdx += 1
      }
    }
    ( uKc.toVector, aIdxMap.toVector, bIdxMap.toVector )
  }

  /** Look at two nodes and try to merge them
    * return new node if merge was done
    * seach parents for common l/r
    * check overlap and that constraint type the same
    * create new merged node
    * look if all L or all R for mux. if so set l/r
    */
  def tryMerge( nA : Node , nB : Node ) : Option[Node] = {

    // checkType
    if ( nA.isA() != nB.isA() || nA.isB() != nB.isB() || nA.isC() != nB.isC() )
      return None

    // check number of distinct inputs
    val sameIn = nA.getChild( 0 ) == nB.getChild( 0 ) && nA.getChild( 1 ) == nB.getChild( 1 )
    val oppositeIn = nA.getChild( 0 ) == nB.getChild( 1 ) && nA.getChild( 1 ) == nB.getChild( 0 )
    if ( sameIn || oppositeIn ) {
      val mapping = commonMapping( nA.uk, nB.uk )
      val ck = ( nA.ck zip nB.ck ).map( cki => {
        if ( cki._1 == -1 && cki._2 == -1 )
          -1
        else if ( cki._2 == -1 )
          mapping._2( cki._1 )
        else if ( cki._1 == -1 )
          mapping._3( cki._2 )
        else {
          if ( mapping._2( cki._1 ) != mapping._3( cki._2 ) )
            return None // as cannot merge under this condition
          mapping._2( cki._1 )
        }
      })
      // create the new node
      val nC = Node( mapping._1, ck )
      if ( nA.isA() )
        nC.setA()
      if ( nA.isB() )
        nC.setB()
      if ( nA.isC() )
        nC.setC()

      // set l/r
      nC.setChild( nA.getChild( 0 ), 0 )
      nC.setChild( nA.getChild( 1 ), 1 )

      // fix parent links
      for ( p <- nA.getParents() ++ nB.getParents() ) {
        if ( p.getChild( 0 ).isDefined && ( p.getChild( 0 ).get == nA || p.getChild( 0 ).get == nB ) )
          p.setChild( nC, 0 )
        if ( p.getChild( 1 ).isDefined && ( p.getChild( 1 ).get == nA || p.getChild( 1 ).get == nB ) )
          p.setChild( nC, 1 )
      }

      return Some( nC )
    }

    None
  }

  /** Increment the integers in position 1 of vectors
    */
  private def incr( uk : Seq[Set[Seq[Int]]] ) : Seq[Set[Seq[Int]]] = {
    uk.map( s => s.map( v => { List( v(0) + 1 ) ++ v.drop(1) }.to[Seq]))
  }

  /** Combine two uk/cks in an add
    */
  private def combineAdd( uk1 : Seq[Set[Seq[Int]]], ck1 : Seq[Int],
    uk2 : Seq[Set[Seq[Int]]], ck2 : Seq[Int] ) : ( Seq[Set[Seq[Int]]], Seq[Int] ) = {
    val ckCombined = ck1.zip( ck2 )
    val uKidx = ckCombined.distinct.filter( _ != ( -1, -1 ) )
    val ckNew = ckCombined.map( uKidx.indexOf( _ ) )
    // should never have one as -1 and not the other
    val ukNew = uKidx.map( uki => uk1( uki._1 ) ++ uk2( uki._2 ) )
    ( ukNew, ckNew )
  }

  /** Combine two uk/cks in a mux
    */
  private def combineMux( uk1 : Seq[Set[Seq[Int]]], ck1 : Seq[Int],
    uk2 : Seq[Set[Seq[Int]]], ck2 : Seq[Int] ) : ( Seq[Set[Seq[Int]]], Seq[Int] ) = {
    val ukNew = (uk1 ++ uk2).distinct
    val uk1Idx = uk1.map( uki => ukNew.indexOf(uki) )
    val uk2Idx = uk2.map( uki => ukNew.indexOf(uki) )
    val ckNew = ck1.zip( ck2 ).map( cks => {
      if ( cks._1 == -1 && cks._2 == -1 )
        -1
      else if ( cks._1 == -1 )
        uk2Idx( cks._2 )
      else if ( cks._2 == -1 )
        uk1Idx( cks._1 )
      else {
        assert( uk1Idx( cks._1 ) == uk2Idx( cks._2 ), "Invalid mux combine of {" + uk1 + ", " + ck1 + "} and {" + uk2 + ", " + ck2 + "}" )
        uk1Idx( cks._1 )
      }
    })
    ( ukNew, ckNew )
  }

  /** pass ck through the ckFilter so that cks with other parents aren't included
    */
  private def filterCk( ck : Seq[Int], uk : Seq[Set[Seq[Int]]], ckFilter : Seq[Int], ukFilter : Seq[Set[Seq[Int]]] ) : Seq[Int] = {
    val mapping = ukFilter.map( uki => if ( uki != -1 ) uk.indexOf( uki ) else -1 )
    ckFilter.map( cki => if ( cki != -1 ) mapping( cki ) else -1 ).zip( ck ).map( cks => if ( cks._1 == -1 ) -1 else cks._2 )
  }

  /** ignore uks, useful for adds with no other parents
    */
  private def filterCk( ck : Seq[Int], ckFilter : Seq[Int] ) : Seq[Int] = {
    ckFilter.zip( ck ).map( cks => if ( cks._1 == -1 ) -1 else cks._2 )
  }


  /** nSwap satisfies constraintA, nPar satisfies constraintB
    * nPar.lNode = nSwap
    * nPar.rNode = nSwap
    * transformed to (6):
    * nPar.lNode = nodeA
    * nPar.rNode = nodeB
    * nodeA.lNode = nSwap.lNode
    * nodeA.rNode = nSwap.lNode
    * nodeB.lNode = nSwap.rNode
    * nodeB.rNode = nSwap.rNode
    */
  private def swapCase1( nPar : Node, nSwap : Node ) : Seq[Node] = {
    val nodeAuK = nSwap.getChild( 0 ).get.getUkNext()
    val nodeAcKFiltered = filterCk( nSwap.getChild( 0 ).get.getCkNext(), nSwap.ck )

    val nodeA = Node( nodeAuK, nodeAcKFiltered )
    nodeA.setChild( nSwap.getChild( 0 ), 0 )
    nodeA.setChild( nSwap.getChild( 0 ), 1 )
    nodeA.setB()
    val nodeBuK = nSwap.getChild( 1 ).get.getUkNext()
    val nodeBcKFiltered = filterCk( nSwap.getChild( 1 ).get.getCkNext(), nSwap.ck )

    val nodeB = Node( nodeBuK, nodeBcKFiltered )
    nodeB.setChild( nSwap.getChild( 1 ), 0 )
    nodeB.setChild( nSwap.getChild( 1 ), 1 )
    nodeB.setB()

    nPar.setChild( nodeA, 0 )
    nPar.setChild( nodeB, 1 )
    nPar.setA()
    List( nPar, nodeA, nodeB )
  }

  /** nSwap satisfies constraintB, nPar satisfies constraintB
    * nPar.lNode = nSwap
    * nPar.rNode = nSwap
    * transformed to (10):
    * nPar.lNode = nodeA
    * nPar.rNode = nodeB
    * nodeA.lNode = nSwap.lNode
    * nodeA.rNode = nSwap.lNode
    * nodeB.lNode = nSwap.rNode
    * nodeB.rNode = nSwap.rNode
    */
  private def swapCase2( nPar : Node, nSwap : Node ) : Seq[Node] = {
    val nodeAuK = nSwap.getChild( 0 ).get.getUkNext()
    val nodeAcKFiltered = filterCk( nSwap.getChild( 0 ).get.getCkNext(), nodeAuK, nSwap.ck, nSwap.uk )

    val nodeA = Node( nodeAuK, nodeAcKFiltered )
    nodeA.setChild( nSwap.getChild( 0 ), 0 )
    nodeA.setChild( nSwap.getChild( 0 ), 1 )
    nodeA.setB()
    val nodeBuK = nSwap.getChild( 1 ).get.getUkNext()
    val nodeBcKFiltered = filterCk( nSwap.getChild( 1 ).get.getCkNext(), nodeBuK, nSwap.ck, nSwap.uk )

    val nodeB = Node( nodeBuK, nodeBcKFiltered )
    nodeB.setChild( nSwap.getChild( 1 ), 0 )
    nodeB.setChild( nSwap.getChild( 1 ), 1 )
    nodeB.setB()

    nPar.setChild( nodeA, 0 )
    nPar.setChild( nodeB, 1 )
    nPar.setB()
    List( nPar, nodeA, nodeB )
  }

  /** nSwap satisfies constraintB, nPar satisfies constraintA, nOther satisfies constraintA
    * nPar.lNode = nSwap
    * nPar.rNode = nOther
    * nSwap.lNode = nSwap.rNode
    * transformed to (10):
    * nPar.lNode = nodeA
    * nPar.rNode = nodeB
    * nodeA.lNode = nSwap.lNode
    * nodeA.rNode = nOther.lNode
    * nodeB.lNode = nOther.rNode
    * nodeB.rNode = nOther.rNode
    */
  private def swapCase4( nPar : Node, nSwap : Node, nOther : Node ) : Seq[Node] = {

    // nSwap must be the reg by itself
    // rotate so that nOther.rNode + nSwap.lNode and nOther.lNode is reg

    val otherLuK = nOther.getChild( 0 ).get.getUkNext()
    val otherLcK = filterCk( nOther.getChild( 0 ).get.getCkNext(), nOther.ck )

    val nodeA = Node( otherLuK, otherLcK )
    nodeA.setChild( nOther.getChild( 0 ), 0 )
    nodeA.setChild( nOther.getChild( 0 ), 1 )
    nodeA.setB()

    // find distinct ck combinations
    val otherRuK = nOther.getChild( 1 ).get.getUkNext()
    val otherRcK = filterCk( nOther.getChild( 1 ).get.getCkNext(), nOther.ck )
    val swapuK = nSwap.uk
    val swapcK = nSwap.ck

    // combine two as union
    val combAdd = combineAdd( swapuK, swapcK, otherRuK, otherRcK )
    val nodeB = Node( combAdd._1, combAdd._2 )
    nodeB.setA()
    nodeB.setChild( nOther.getChild( 1 ), 0 )
    nodeB.setChild( nSwap.getChild( 0 ), 1 )

    nPar.setChild( nodeA, 0 )
    nPar.setChild( nodeB, 1 )
    nPar.setA()
    List( nPar, nodeA, nodeB )
  }

  /** nSwap satisfies constraintA, nPar satisfies constraintA, nOther satisfies constraintA
    * nPar.lNode = nSwap
    * nPar.rNode = nOther
    * transformed to (5):
    * nPar.lNode = nodeA
    * nPar.rNode = nodeB
    * nodeA.lNode = nOther.lNode
    * nodeA.rNode = nSwap.lNode
    * nodeB.lNode = nSwap.rNode
    * nodeB.rNode = nOther.rNode
    */
  private def swapCase5( nPar : Node, nSwap : Node, nOther : Node ) : Seq[Node] = {
    val otherLFiltered = filterCk( nOther.getChild( 0 ).get.getCkNext(), nOther.ck )
    val otherRFiltered = filterCk( nOther.getChild( 1 ).get.getCkNext(), nOther.ck )
    val swapLFiltered = filterCk( nSwap.getChild( 0 ).get.getCkNext(), nSwap.ck )
    val swapRFiltered = filterCk( nSwap.getChild( 1 ).get.getCkNext(), nSwap.ck )
    val nodeAComb = combineAdd( nOther.getChild( 0 ).get.getUkNext(), otherLFiltered,
      nSwap.getChild( 0 ).get.getUkNext(), swapLFiltered )
    val nodeBComb = combineAdd( nOther.getChild( 1 ).get.getUkNext(), otherRFiltered,
      nSwap.getChild( 1 ).get.getUkNext(), swapRFiltered )

    val nodeA = Node( nodeAComb._1, nodeAComb._2 )
    val nodeB = Node( nodeBComb._1, nodeBComb._2 )
    nodeA.setA()
    nodeA.setChild( nOther.getChild( 0 ), 0 )
    nodeA.setChild( nSwap.getChild( 0 ), 1 )
    nodeB.setChild( nSwap.getChild( 1 ), 0 )
    nodeB.setChild( nOther.getChild( 1 ), 1 )
    nodeB.setA()

    nPar.setChild( nodeA, 0 )
    nPar.setChild( nodeB, 1 )
    nPar.setA()
    List( nPar, nodeA, nodeB )
  }

  /** nSwap satisfies constraintB, nPar satisfies constraintA, nOther satisfies constraintB
    * nPar.lNode = nSwap
    * nPar.rNode = nOther
    * nSwap.lNode = nSwap.rNode
    * nOther.lNode = nOther.rNode
    * transformed to (1):
    * nPar.lNode = nodeA
    * nPar.rNode = nodeA
    * nodeA.lNode = nSwap.lNode
    * nodeA.rNode = nOther.lNode
    */
  private def swapCase6( nPar : Node, nSwap : Node, nOther : Node ) : Seq[Node] = {

    val othercK = filterCk( nOther.getChild( 0 ).get.getCkNext(), nOther.getChild( 0 ).get.getUkNext(), nOther.ck, nOther.uk )
    val otheruK = nOther.getChild( 0 ).get.getUkNext()
    val swapcK = filterCk( nSwap.getChild( 0 ).get.getCkNext(), nSwap.getChild( 0 ).get.getUkNext(), nSwap.ck, nSwap.uk )
    val swapuK = nSwap.getChild( 0 ).get.getUkNext()
    val combAdd = combineAdd( otheruK, othercK, swapuK, swapcK )
    val nodeA = Node( combAdd._1, combAdd._2 )
    nodeA.setA()
    nodeA.setChild( nSwap.getChild( 0 ), 0 )
    nodeA.setChild( nOther.getChild( 0 ), 1 )

    nPar.setChild( nodeA, 0 )
    nPar.setChild( nodeA, 1 )
    nPar.setB()
    List( nPar, nodeA )
  }

  /** nSwap satisfies constraintB, nPar satisfies constraintA, nOther satisfies constraintB
    * nPar.lNode = nSwap
    * nPar.rNode = nOther
    * nSwap.lNode = nSwap.rNode
    * transformed to (11):
    * nPar.lNode = nodeA
    * nPar.rNode = nodeB
    * nodeA.lNode = nSwap.lNode
    * nodeA.rNode = nOther.lNode
    * nodeB.lNode = nSwap.lNode
    * nodeB.rNode = nOther.rNode
    */
  private def swapCase7( nPar : Node, nSwap : Node, nOther : Node ) : Seq[Node] = {

    val otherLcK = filterCk( nOther.getChild( 0 ).get.getCkNext(), nOther.getChild( 0 ).get.getUkNext(), nOther.ck, nOther.uk )
    val otherLuK = nOther.getChild( 0 ).get.getUkNext()
    val otherRcK = filterCk( nOther.getChild( 1 ).get.getCkNext(), nOther.getChild( 1 ).get.getUkNext(), nOther.ck, nOther.uk )
    val otherRuK = nOther.getChild( 1 ).get.getUkNext()
    val swapcKL = filterCk( nSwap.ck, otherLcK )
    val swapcKR = filterCk( nSwap.ck, otherRcK )
    val combAddL = combineAdd( otherLuK, otherLcK, nSwap.uk, swapcKL )
    val combAddR = combineAdd( otherRuK, otherRcK, nSwap.uk, swapcKR )
    val nodeA = Node( combAddL._1, combAddL._2 )
    nodeA.setChild( nOther.getChild( 0 ), 0 )
    nodeA.setChild( nSwap.getChild( 0 ), 1 )
    nodeA.setA()
    val nodeB = Node( combAddR._1, combAddR._2 )
    nodeB.setChild( nSwap.getChild( 0 ), 0 )
    nodeB.setChild( nOther.getChild( 1 ), 1 )
    nodeB.setA()
    nPar.setChild( nodeA, 0 )
    nPar.setChild( nodeB, 1 )
    nPar.setB()
    List( nPar, nodeA, nodeB )
  }

  /** nSwap satisfies constraintB, nPar satisfies constraintA, nOther satisfies constraintB
    * nPar.lNode = nSwap
    * nPar.rNode = nOther
    * nSwap.lNode = nSwap.rNode
    * nOther.lNode = nOther.rNode
    * transformed to (2):
    * nPar.lNode = nodeA
    * nPar.rNode = nodeA
    * nodeA.lNode = nSwap.lNode
    * nodeA.rNode = nOther.lNode
    */
  private def swapCase10( nPar : Node, nSwap : Node, nOther : Node ) : Seq[Node] = {

    val othercK = filterCk( nOther.getChild( 0 ).get.getCkNext(), nOther.getChild( 0 ).get.getUkNext(), nOther.ck, nOther.uk )
    val otheruK = nOther.getChild( 0 ).get.getUkNext()
    val swapcK = filterCk( nSwap.getChild( 0 ).get.getCkNext(), nSwap.getChild( 0 ).get.getUkNext(), nSwap.ck, nSwap.uk )
    val swapuK = nSwap.getChild( 0 ).get.getUkNext()
    val combMux = combineMux( otheruK, othercK, swapuK, swapcK )
    val nodeA = Node( combMux._1, combMux._2 )
    nodeA.setChild( nSwap.getChild( 0 ), 0 )
    nodeA.setChild( nOther.getChild( 0 ), 1 )
    nodeA.setB()
    nPar.setChild( nodeA, 0 )
    nPar.setChild( nodeA, 1 )
    nPar.setB()
    List( nPar, nodeA )
  }

  /** nSwap satisfies constraintA, nPar satisfies constraintB, nOther satisfies constraintA
    * nPar.lNode = nSwap
    * nPar.rNode = nOther
    * iff nSwap and nOther have lNode or rNode in common
    * transformed to (7):
    * nPar.lNode = nodeA
    * nPar.rNode = nodeB
    * nodeA.lNode = nSwap.lNode
    * nodeA.rNode = nSwap.lNode
    * nodeB.lNode = nOther.lNode
    * nodeB.lNode = nOther.rNode
    */
  private def swapCase11( nPar : Node, nSwap : Node, nOther : Node ) : Seq[Node] = {

    // find common
    val swapLOtherL = nSwap.getChild( 0 ).get == nOther.getChild( 0 ).get
    val swapLOtherR = nSwap.getChild( 0 ).get == nOther.getChild( 1 ).get
    val swapROtherL = nSwap.getChild( 1 ).get == nOther.getChild( 0 ).get
    val swapROtherR = nSwap.getChild( 1 ).get == nOther.getChild( 1 ).get
    val swapL = swapLOtherL || swapLOtherR
    val swapR = swapROtherL || swapROtherR
    val otherL = swapLOtherL || swapROtherL
    if ( swapL || swapR ) {
      val commonNode = { if ( swapL ) nSwap.getChild( 0 ).get else nSwap.getChild( 1 ).get }
      val swapSpare = { if ( swapL ) nSwap.getChild( 1 ).get else nSwap.getChild( 0 ).get }
      val otherSpare = { if ( otherL ) nOther.getChild( 1 ).get else nOther.getChild( 0 ).get }

      val nodeAuK = commonNode.getUkNext()
      val nodeAcKUp = commonNode.getCkNext()
      val ckComb = nSwap.ck.zip( nOther.ck ).map( cks => {
        if ( cks._1 == -1 )
          cks._2
        else
          cks._1
      })
      val nodeAcK = filterCk( nodeAcKUp, ckComb )
      val nodeA = Node( nodeAuK, nodeAcK )
      nodeA.setB()
      nodeA.setChild( commonNode, 0 )
      nodeA.setChild( commonNode, 1 )

      val swapCkFiltered = filterCk( swapSpare.getCkNext(), nSwap.ck )
      val otherCkFiltered = filterCk( otherSpare.getCkNext(), nOther.ck )
      val combMux = combineMux( swapSpare.getUkNext(), swapCkFiltered,
        otherSpare.getUkNext(), otherCkFiltered )
      val nodeB = Node( combMux._1, combMux._2 )
      nodeB.setB()
      nodeB.setChild( swapSpare, 0 )
      nodeB.setChild( otherSpare, 1 )

      nPar.setChild( nodeA, 0 )
      nPar.setChild( nodeB, 1 )
      nPar.setA()
      return List( nPar, nodeA, nodeB )
    }
    List[Node]()
  }

  /** nSwap satisfies constraintB, nPar satisfies constraintB, nOther satisfies constraintB
    * nPar.lNode = nSwap
    * nPar.rNode = nOther
    * transformed to (12):
    * nPar.lNode = nodeA
    * nPar.rNode = nodeB
    * nodeA.lNode = nOther.lNode
    * nodeA.rNode = nSwap.lNode
    * nodeB.lNode = nSwap.rNode
    * nodeB.rNode = nOther.rNode
    */
  private def swapCase12( nPar : Node, nSwap : Node, nOther : Node ) : Seq[Node] = {

    val otherLFiltered = filterCk( nOther.getChild( 0 ).get.getCkNext(), nOther.getChild( 0 ).get.getUkNext(), nOther.ck, nOther.uk )
    val otherRFiltered = filterCk( nOther.getChild( 1 ).get.getCkNext(), nOther.getChild( 1 ).get.getUkNext(), nOther.ck, nOther.uk )
    val swapLFiltered = filterCk( nSwap.getChild( 0 ).get.getCkNext(), nSwap.getChild( 0 ).get.getUkNext(), nSwap.ck, nSwap.uk )
    val swapRFiltered = filterCk( nSwap.getChild( 1 ).get.getCkNext(), nSwap.getChild( 1 ).get.getUkNext(), nSwap.ck, nSwap.uk )
    val nodeAComb = combineMux( nOther.getChild( 0 ).get.getUkNext(), otherLFiltered,
      nSwap.getChild( 0 ).get.getUkNext(), swapLFiltered )
    val nodeBComb = combineMux( nOther.getChild( 1 ).get.getUkNext(), otherRFiltered,
      nSwap.getChild( 1 ).get.getUkNext(), swapRFiltered )

    val nodeA = Node( nodeAComb._1, nodeAComb._2 )
    val nodeB = Node( nodeBComb._1, nodeBComb._2 )
    nodeA.setB()
    nodeA.setChild( nOther.getChild( 0 ), 0 )
    nodeA.setChild( nSwap.getChild( 0 ), 1 )
    nodeB.setChild( nSwap.getChild( 1 ), 0 )
    nodeB.setChild( nOther.getChild( 1 ), 1 )
    nodeB.setB()
    nPar.setChild( nodeA, 0 )
    nPar.setChild( nodeB, 1 )
    nPar.setB()
    List( nPar, nodeA, nodeB )
  }

  /** nSwap satisfies constraintB, nPar satisfies constraintB, nOther satisfies constraintB
    * nPar.lNode = nSwap
    * nPar.rNode = nOther
    * transformed to (13):
    * nPar.lNode = nodeA
    * nPar.rNode = nodeB
    * nodeA.lNode = nOther.lNode
    * nodeA.rNode = nOther.lNode
    * nodeB.lNode = nOther.rNode
    * nodeB.rNode = nSwap.lNode
    */
  private def swapCase13( nPar : Node, nSwap : Node, nOther : Node ) : Seq[Node] = {

    val otherLuK = nOther.getChild( 0 ).get.getUkNext()
    val otherLcK = filterCk( nOther.getChild( 0 ).get.getCkNext(), otherLuK, nOther.ck, nOther.uk )
    val nodeA = Node( otherLuK, otherLcK )
    nodeA.setChild( nOther.getChild( 0 ), 0 )
    nodeA.setChild( nOther.getChild( 0 ), 1 )
    nodeA.setB()
    // find distinct ck combinations
    val otherRuK = nOther.getChild( 1 ).get.getUkNext()
    val otherRcK = filterCk( nOther.getChild( 1 ).get.getCkNext(), otherRuK, nOther.ck, nOther.uk )
    val swapuK = nSwap.uk
    val swapcK = nSwap.ck

    // combine two as union
    val combMux = combineMux( swapuK, swapcK, otherRuK, otherRcK )
    val nodeB = Node( combMux._1, combMux._2 )
    nodeB.setB()
    nodeB.setChild( nOther.getChild( 1 ), 0 )
    nodeB.setChild( nSwap.getChild( 0 ), 1 )

    nPar.setChild( nodeA, 0 )
    nPar.setChild( nodeB, 1 )
    nPar.setB()
    List( nPar, nodeA, nodeB )
  }

  /** Look at two nodes and try to swap them
    */
  def trySwap( nPar : Node, nSwap : Node, applyIfIncrease : Boolean = true ) : (Seq[Node], Int) = {

    assert( nSwap.hasParent( nPar ), "For swap must have swap and parent node" )

    if ( nSwap.isC() )
      return (List[Node](), 0)

    // work out how nodes are connected ( should be directly )
    if ( nPar.isB() && nPar.getChild( 0 ) == nPar.getChild( 1 ) ) {
      if ( nSwap.isA() )
        return { if ( applyIfIncrease ) ( swapCase1( nPar, nSwap ), 1 ) else ( List[Node](), 0 ) }
      if ( nSwap.isB() && nSwap.getChild( 0 ) != nSwap.getChild( 1 ) )
        return { if ( applyIfIncrease ) ( swapCase2( nPar, nSwap ), 2 ) else ( List[Node](), 0) }
      return (List[Node](), 0) // case 3 which no point as changes nothing
    }
    val nOther = { if ( nPar.getChild( 0 ).get == nSwap ) nPar.getChild( 1 ).get else nPar.getChild( 0 ).get }

    if ( nPar.isA() ) {
      if ( nSwap.isB() && nSwap.getChild( 0 ) == nSwap.getChild( 1 ) ) {
        if ( nOther.isA() )
          return ( swapCase4( nPar, nSwap, nOther ), 4 )
        if ( nOther.isB() && nOther.getChild( 0 ) == nOther.getChild( 1 ) )
          return ( swapCase6( nPar, nSwap, nOther ), 6 )
        if ( nOther.isB() )
          return ( swapCase7( nPar, nSwap, nOther ), 7 )
      }

      if ( nOther.isB() && nOther.getChild( 0 ) == nOther.getChild( 1 ) ) {
        if ( nSwap.isA() )
          return ( swapCase4( nPar, nOther, nSwap ), 4 )
        if ( nSwap.isB() )
          return ( swapCase7( nPar, nOther, nSwap ), 7 )
      }

      if ( nSwap.isA() && nOther.isA() )
        return ( swapCase5( nPar, nSwap, nOther ), 5 )
    }

    if ( nPar.isB() ) {
      if ( nSwap.isB() && nSwap.getChild( 0 ) == nSwap.getChild( 1 ) ) {
        if ( nOther.isB() && nOther.getChild( 0 ) == nOther.getChild( 1 ) )
          return ( swapCase10( nPar, nSwap, nOther ), 10 )
        if ( nOther.isB() )
          return ( swapCase13( nPar, nSwap, nOther ), 13 )
        return ( List[Node](), 0 )
      }

      if ( nOther.isB() && nOther.getChild( 0 ) == nOther.getChild( 1 ) ) {
        if ( nSwap.isB() )
          return ( swapCase13( nPar, nOther, nSwap ), 13 )
      }

      if ( nSwap.isB() && nOther.isB() )
        return ( swapCase12( nPar, nSwap, nOther ), 12 )

      if ( nSwap.isA() && nOther.isA() )
        return ( swapCase11( nPar, nSwap, nOther ), 11 )
    }
    ( List[Node](), 0 )
  }

  /** Split a node and randomally assign its parents to each
    */
  def trySplit( nA : Node ) : Seq[Node] = {
    if ( nA.getParents().size <= 1 || nA.isC() )
      return List[Node]()

    val shuffledPar = Random.shuffle( nA.getParents() )
    val splitIdx = Random.nextInt( shuffledPar.size - 1 )

    val ckNeeded = shuffledPar.map( n => {
      if ( n.isA() || n.getChild( 0 ) == n.getChild( 1 ) ) {
        // adds or reg are easy as all are needed
        n.getCkPrev().zip( nA.ck ).map( ck => {
          if ( ck._1 == -1 )
            -1
          else
            ck._2
        })
      } else {
        // mux
        val prevCk = n.getCkPrev()
        val prevUk = n.getUkPrev()
        // determine for each ck if uk is provided by split
        prevCk.zip( nA.ck ).map( ck => {
          if ( ck._1 == -1 )
            -1
          else {
            if ( ck._2 != -1 && prevUk( ck._1 ) == nA.uk( ck._2 ) )
              ck._2
            else
              -1
          }
        })
      }
    })

    val ( n1Par, n2Par ) = shuffledPar.splitAt( splitIdx + 1 )
    val ( ck1, ck2 ) = ckNeeded.splitAt( splitIdx + 1 )
    val n1Ck = ( 0 until nA.nodeSize ).map( idx => ck1.map( cks => cks( idx ) ).reduce( (x,y) => {
      if ( x == -1 )
        y
      else {
        assert( y == x || y == -1, "Invalid combining of parents" )
        x
      }
    })).toVector
    val n2Ck = ( 0 until nA.nodeSize ).map( idx => ck2.map( cks => cks( idx ) ).reduce( (x,y) => {
      if ( x == -1 )
        y
      else {
        assert( y == x || y == -1, "Invalid combining of parents" )
        x
      }
    })).toVector
    val n1 = Node( nA.uk, n1Ck )
    val n2 = Node( nA.uk, n2Ck )
    if ( nA.getChild( 0 ).isDefined ) {
      n1.setChild( nA.getChild( 0 ), 0 )
      n2.setChild( nA.getChild( 0 ), 0 )
    }
    if ( nA.getChild( 1 ).isDefined ) {
      n1.setChild( nA.getChild( 1 ), 1 )
      n2.setChild( nA.getChild( 1 ), 1 )
    }
    if ( nA.isA() ) {
      n1.setA()
      n2.setA()
    }
    if ( nA.isB() ) {
      n1.setB()
      n2.setB()
    }
    for ( p <- n1Par ) {
      if ( p.getChild( 0 ).isDefined && p.getChild( 0 ).get == nA )
        p.setChild( n1, 0 )
      if ( p.getChild( 1 ).isDefined && p.getChild( 1 ).get == nA )
        p.setChild( n1, 1 )
    }
    for ( p <- n2Par ) {
      if ( p.getChild( 0 ).isDefined && p.getChild( 0 ).get == nA )
        p.setChild( n2, 0 )
      if ( p.getChild( 1 ).isDefined && p.getChild( 1 ).get == nA )
        p.setChild( n2, 1 )
    }

    return List( n1, n2 )
  }

}
