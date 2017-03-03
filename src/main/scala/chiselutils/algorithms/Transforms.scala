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
    if ( nA.getChildren() == nB.getChildren() ) {
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
      nC.addChildren( nA.getChildren() )

      // fix parent links
      for ( p <- nA.getParents() ++ nB.getParents() ) {
        p.replaceIfChild( nA, nC )
        p.replaceIfChild( nB, nC )
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

  private def filterCk( child : Node, par : Node ) : Seq[Int] = {
    if ( par.isB() )
      return filterCk( child.getCkNext(), child.getUkNext(), par.ck, par.uk )
    filterCk( child.getCkNext(), par.ck ) // for adds just filter -1's
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
    * nSwap adds inputs followed by a register, change to registers then adds
    */
  private def swapCase1( nPar : Node ) : Seq[Node] = {
    assert( nPar.numChildren() == 1, "Node must have one child" )
    val nAdd23 = nPar.getOnlyChild()
    nPar.removeChildren()
    val newNodes = nAdd23.getChildren().map( child => {
      val ck = filterCk( child, nAdd23 )
      println( "ck = " + ck )
      val newNode = Node( child.getUkNext(), ck )
      newNode.addChild( child )
      newNode.addChild( child )
      newNode.setB()
      nPar.addChild( newNode )
      newNode
    })
    nAdd23.removeChildren()
    nPar.setA()
    List( nPar ) ++ newNodes.toList
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
    * nSwap is a mux followed by a register, change to registers followed by mux
    */
  private def swapCase2( nPar : Node ) : Seq[Node] = {
    assert( nPar.numChildren() == 1, "Node must have one child" )
    val nMux = nPar.getOnlyChild()
    nPar.removeChildren()
    val newNodes = nMux.getChildren().map( child => {
      val ck = filterCk( child, nMux )

      val newNode = Node( child.getUkNext(), ck )
      newNode.addChild( child )
      newNode.setB()
      nPar.addChild( newNode )
      newNode
    })
    nMux.removeChildren()
    nPar.setB()
    List( nPar ) ++ newNodes.toList
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
    * nSwap is a register, nOther is a add, nPar is an add
    * if nOther is 2 input, combine with nSwap to make 3 input and make nPar a reg
    * if nOther is 3 input, split to 2X two inputs adds
    */
  private def swapCase4( nPar : Node, nReg : Node, nAdd23 : Node ) : Seq[Node] = {

    if ( nAdd23.numChildren() == 2 ) {
      val ukck = combineAdd( nReg.uk, nReg.ck, nAdd23.uk, nAdd23.ck )
      val nodeA = Node( ukck._1, ukck._2 )
      for ( c <- nReg.getChildren() ++ nAdd23.getChildren() )
        nodeA.addChild( c )
      nodeA.setA()
      nPar.removeChildren()
      nPar.addChild( nodeA )
      nPar.setB()
      nReg.removeChildren()
      nAdd23.removeChildren()
      return List( nPar, nodeA )
    }
    // else nAdd23 is 3 input
    val randChild = nAdd23.getRandomChild()

    val randuK = randChild._1.getUkNext()
    val randcK = filterCk( randChild._1, nAdd23 )
    val combAdd = combineAdd( nReg.uk, nReg.ck, randuK, randcK )
    val nodeA = Node( combAdd._1, combAdd._2 )
    nodeA.addChild( randChild._1 )
    nodeA.addChild( nReg.getOnlyChild() )
    nodeA.setA()

    val ukck = randChild._2.map( child => {
      val uk = child.getUkNext()
      val ck = filterCk( child, nAdd23 )
      ( uk, ck )
    }).reduce( ( x, y ) => combineAdd( x._1, x._2, y._1, y._2 ) )

    val nodeB = Node( ukck._1, ukck._2 )
    nodeB.setA()
    for ( c <- randChild._2 )
      nodeB.addChild( c )

    nPar.removeChildren()
    nPar.addChild( nodeA )
    nPar.addChild( nodeB )
    nPar.setA()
    nReg.removeChildren()
    nAdd23.removeChildren()
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
  private def swapCase5( nPar : Node ) : Seq[Node] = {

    val allGrandList = nPar.getChildren().map( c => c.getChildren().map( gc => ( gc, c ) ) ).reduce( _ ++ _ ).toList
    val allGc = Random.shuffle( allGrandList )
    val nParChildren = nPar.getChildren()
    nPar.removeChildren()
    val groupNum = {
      if ( allGc.size < 7 )
        Random.nextInt( 2 ) + 2
      else
        3
    }
    val addNodes = allGc.grouped(groupNum).map( addGroup => {
      val ukck = addGroup.map( gcc => {
        ( gcc._1.getUkNext(), filterCk( gcc._1, gcc._2 ) )
      }).reduce( ( x, y ) => combineAdd( x._1, x._2, y._1, y._2 ) )
      val newNode = Node( ukck._1, ukck._2 )
      newNode.addChildren( addGroup.map( _._1 ).toSet )
      if ( addGroup.size > 1 )
        newNode.setA()
      else
        newNode.setB()
      newNode
    }).toSet
    nPar.addChildren( addNodes )
    for ( c <- nParChildren )
      c.removeChildren()
    List( nPar ) ++ addNodes.toList
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
  private def swapCase6( nPar : Node ) : Seq[Node] = {
    val newCkUk = nPar.getChildren().map( child => {
      val grandchild = child.getOnlyChild()
      val newcK = filterCk( grandchild, child )
      ( grandchild.getUkNext(), newcK )
    }).reduce( ( x, y ) => combineAdd( x._1, x._2, y._1, y._2 ) )

    val nodeA = Node( newCkUk._1, newCkUk._2 )
    nodeA.setA()
    nodeA.addChildren( nPar.getChildren().map( _.getOnlyChild() ) )

    for ( c <- nPar.getChildren() )
      c.removeChildren()

    nPar.removeChildren()
    nPar.addChild( nodeA )
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
  private def swapCase7( nPar : Node, nReg : Node, nMux : Node ) : Seq[Node] = {

    val nMuxUkCk = nMux.getChildren().map( child => {
      val newcK = filterCk( child, nMux )
      val newuK = child.getUkNext()
      ( newuK, newcK, child )
    })
    val newNodes = nMuxUkCk.map( ukck => {
      val swapCk = filterCk( nReg.ck, ukck._2 )
      val combAdd = combineAdd( ukck._1, ukck._2, nReg.uk, swapCk )
      val nodeA = Node( combAdd._1, combAdd._2 )
      nodeA.addChild( ukck._3 )
      nodeA.addChild( nReg.getOnlyChild() )
      nodeA.setA()
      nodeA
    })
    nReg.removeChildren()
    nMux.removeChildren()
    nPar.removeChildren()
    nPar.addChildren( newNodes )
    nPar.setB()
    List( nPar ) ++ newNodes.toList
  }

  /** nSwap is 2 input add, nOther is mux nPar is add
    */
  private def swapCase9( nPar : Node, nAdd : Node, nMux : Node ) : Seq[Node] = {
    val ukcks = nMux.getChildren().map( child => {
      val newCk = filterCk( child, nMux )
      val ukck = combineAdd( child.getUkNext(), newCk, nAdd.uk, nAdd.ck )
      ( ukck._1, ukck._2, child )
    })
    val newNodes = ukcks.map( ukck => {
      val n = Node( ukck._1, ukck._2 )
      n.addChildren( nAdd.getChildren() )
      n.addChild( ukck._3 )
      n
    })
    nAdd.removeChildren()
    nMux.removeChildren()
    nPar.removeChildren()
    nPar.addChildren( newNodes )
    List( nPar ) ++ newNodes.toList
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
  private def swapCase10( nPar : Node ) : Seq[Node] = {

    val ukck = nPar.getChildren().map( child => {
      val grandchild = child.getOnlyChild()
      val cK = filterCk( grandchild, child )
      val uK = grandchild.getUkNext()
      ( uK, cK )
    })
    val combMux = ukck.reduce( ( x, y ) => combineMux( x._1, x._2, y._1, y._2 ) )
    val nodeA = Node( combMux._1, combMux._2 )
    nodeA.addChildren( nPar.getChildren().map( _.getOnlyChild() ) )
    nodeA.setB()
    for ( n <- nPar.getChildren() )
      n.removeChildren()
    nPar.removeChildren()
    nPar.addChild( nodeA )
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
  private def swapCase11( nPar : Node ) : Seq[Node] = {
    val nParChildren = nPar.getChildren().toList
    val nAdd1 = nParChildren(0)
    val nAdd2 = nParChildren(1)
    // find common
    val commonChildren = nAdd1.getChildren() intersect nAdd2.getChildren()
    // can be 2 and 2 or 3 and 3 but not 2 and 3
    if ( commonChildren.size == 1 && nAdd1.numChildren() == nAdd2.numChildren() ) {
      val commonNode = commonChildren.head
      val nodeAuK = commonNode.getUkNext()
      val nodeAcKUp = commonNode.getCkNext()
      val ckComb = nAdd1.ck.zip( nAdd2.ck ).map( cks => {
        if ( cks._1 == -1 )
          cks._2
        else
          cks._1
      })
      val nodeAcK = filterCk( nodeAcKUp, ckComb )
      val nodeA = Node( nodeAuK, nodeAcK )
      nodeA.setB()
      nodeA.addChild( commonNode )
      val otherChildS = nAdd1.getChildren() - commonNode
      val otherChildO = nAdd2.getChildren() - commonNode
      val muxCombNodes = otherChildS.zip( otherChildO ).map( x => {
        val sck = filterCk( x._1, nAdd1 )
        val ock = filterCk( x._2, nAdd2 )
        val muxUkCk = combineMux( x._1.getUkNext(), sck, x._2.getUkNext(), ock )
        val newNode = Node( muxUkCk._1, muxUkCk._2 )
        newNode.addChild( x._1 )
        newNode.addChild( x._2 )
        newNode.setB()
        newNode
      })
      nAdd1.removeChildren()
      nAdd2.removeChildren()
      nPar.removeChildren()
      nPar.addChildren( muxCombNodes )
      nPar.addChild( nodeA )
      nPar.setA()
      return List( nPar, nodeA ) ++ muxCombNodes.toList
    }
    if ( commonChildren.size == 2 && nAdd1.numChildren() == 3 && nAdd2.numChildren() == 3 ) {
      val regNodes = commonChildren.map( child => {
        val ckS = filterCk( child, nAdd1 )
        val ckO = filterCk( child, nAdd2 )
        val muxComb = combineMux( child.getUkNext(), ckS, child.getUkNext(), ckO )
        val n = Node( muxComb._1, muxComb._2 )
        n.addChild( child )
        n
      })
      val remainAdd1Set = nAdd1.getChildren -- commonChildren
      val remainAdd1 = remainAdd1Set.head
      val remainAdd2Set = nAdd2.getChildren -- commonChildren
      val remainAdd2 = remainAdd2Set.head
      val ckS = filterCk( remainAdd1, nAdd1 )
      val ckO = filterCk( remainAdd2, nAdd2 )
      val muxComb = combineMux( remainAdd1.getUkNext(), ckS, remainAdd2.getUkNext(), ckO )
      val muxNode = Node( muxComb._1, muxComb._2 )
      muxNode.addChild( remainAdd1 )
      muxNode.addChild( remainAdd2 )
      nAdd1.removeChildren()
      nAdd2.removeChildren()
      nPar.removeChildren()
      nPar.addChild( muxNode )
      nPar.addChildren( regNodes )
      nPar.setA()
      return List( nPar, muxNode ) ++ regNodes.toList
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
  private def swapCase12( nPar : Node ) : Seq[Node] = {
    val nParChildren = nPar.getChildren().toList
    val nMux1 = nParChildren(0)
    val nMux2 = nParChildren(1)
    val commonChildren = nMux1.getChildren() intersect nMux2.getChildren()
    if ( commonChildren.size == 0 ) {
      val cks = List( nMux1, nMux2 ).map( c => c.getChildren().map( child => {
        val ck = filterCk( child, c )
        ( ck, child )
      }))
      val newMux = cks(0).zip( cks(1) ).map( x => {
        val ukck = combineMux( x._1._2.getUkNext(), x._1._1, x._2._2.getUkNext(), x._2._1 )
        val newNode = Node( ukck._1, ukck._2 )
        newNode.addChild( x._1._2 )
        newNode.addChild( x._2._2 )
        newNode.setB()
        newNode
      })
      nMux1.removeChildren()
      nMux2.removeChildren()
      nPar.removeChildren()
      nPar.addChildren( newMux )
      nPar.setB()
      return List( nPar ) ++ newMux.toList
    }
    if ( commonChildren.size == 1 ) {
      val commonChild = commonChildren.head
      val remainMux1 = ( nMux1.getChildren() - commonChild ).head
      val remainMux2 = ( nMux2.getChildren() - commonChild ).head
      val ckS = filterCk( remainMux1, nMux1 )
      val ckO = filterCk( remainMux2, nMux2 )
      val ukckMux = combineMux( remainMux1.getUkNext(), ckS, remainMux2.getUkNext(), ckO )
      val muxNode = Node( ukckMux._1, ukckMux._2 )
      muxNode.addChild( remainMux1 )
      muxNode.addChild( remainMux2 )
      muxNode.setB()
      val ckA = filterCk( commonChild, nMux1 )
      val ckB = filterCk( commonChild, nMux2 )
      val ukckReg = combineMux( commonChild.getUkNext(), ckA, commonChild.getUkNext(), ckB )
      val regNode = Node( ukckReg._1, ukckReg._2 )
      regNode.addChild( commonChild )
      regNode.setB()
      nMux1.removeChildren()
      nMux2.removeChildren()
      nPar.removeChildren()
      nPar.addChild( muxNode )
      nPar.addChild( regNode )
      nPar.setB()
      return List( nPar, muxNode, regNode )
    }
    // else commonChildren.size == 2
    val regNodes = commonChildren.map( child => {
      val ckA = filterCk( child, nMux1 )
      val ckB = filterCk( child, nMux2 )
      val ukck = combineMux( child.getUkNext(), ckA, child.getUkNext(), ckB )
      val n = Node( ukck._1, ukck._2 )
      n.addChild( child )
      n.setB()
      n
    })
    nMux1.removeChildren()
    nMux2.removeChildren()
    nPar.removeChildren()
    nPar.addChildren( regNodes )
    nPar.setB()
    List( nPar ) ++ regNodes.toList
  }

  /** nSwap satisfies constraintB, nPar satisfies constraintB, nOther satisfies constraintB
    * nPar.lNode = nSwap
    * nPar.rNode = nOther
    * nSwap.lNode = nSwap.rNode
    * transformed to (13):
    * nPar.lNode = nodeA
    * nPar.rNode = nodeB
    * nodeA.lNode = nOther.lNode
    * nodeA.rNode = nOther.lNode
    * nodeB.lNode = nOther.rNode
    * nodeB.rNode = nSwap.lNode
    */
  private def swapCase13( nPar : Node, nReg : Node, nMux : Node ) : Seq[Node] = {

    val nMuxChildren = nMux.getRandomChild()
    val nMuxChild = nMuxChildren._1
    val swapChild = nReg.getOnlyChild()
    val ckO = filterCk( nMuxChild, nMux )
    val ukck = combineMux( nMuxChild.getUkNext(), ckO, swapChild.getUkNext(), swapChild.getCkNext() )
    val muxNode = Node( ukck._1, ukck._2 )
    muxNode.addChild( nMuxChild )
    muxNode.addChild( swapChild )
    muxNode.setB()
    val remainNode = nMuxChildren._2.head
    val ckNew = filterCk( remainNode, nMux )
    val regNode = Node( remainNode.getUkNext(), ckNew )
    regNode.addChild( remainNode )
    regNode.setB()
    nReg.removeChildren()
    nMux.removeChildren()
    nPar.removeChildren()
    nPar.addChild( muxNode )
    nPar.addChild( regNode )
    nPar.setB()
    List( nPar, muxNode, regNode )
  }

  /** 3 input add parent, reg, reg, mux childs
    */
  private def swapCase16( nPar : Node ) : Seq[Node] = {
    val commonNodes = nPar.getChildren().filter( _.isReg() )
    val muxNode = ( nPar.getChildren() -- commonNodes ).head
    val ukckReg = commonNodes.map( child => {
      ( child.uk, child.ck )
    }).reduce( (x,y) => combineAdd( x._1, x._2, y._1, y._2 ) )
    val newAdds = muxNode.getChildren().map( child => {
      val ck = filterCk( child, muxNode )
      val ukck = combineAdd( child.getUkNext(), ck, ukckReg._1, ukckReg._2 )
      val addNode = Node( ukck._1, ukck._2 )
      addNode.addChild( child )
      for ( n <- commonNodes )
        addNode.addChild( n.getOnlyChild() )
      addNode.setA()
      addNode
    })
    muxNode.removeChildren()
    for ( c <- commonNodes )
      c.removeChildren()
    nPar.removeChildren()
    nPar.addChildren( newAdds )
    nPar.setB()
    List( nPar ) ++ newAdds.toList
  }

  private def swapCase18( nPar : Node, nReg : Node, nMux : Node, nAdd : Node ) : Seq[Node] = {
    if ( nAdd.numChildren() == 2 ) {
      // combine the reg and 2 input add to a 3 input add
      val ukck = combineAdd( nReg.uk, nReg.ck, nAdd.uk, nAdd.ck )
      val newAdd = Node( ukck._1, ukck._2 )
      newAdd.setA()
      newAdd.addChild( nReg.getOnlyChild() )
      newAdd.addChildren( nAdd.getChildren() )
      nReg.removeChildren()
      nAdd.removeChildren()
      nPar.replaceIfChild( nReg, newAdd )
      nPar.replaceIfChild( nAdd, newAdd )
      return List( nPar, newAdd, nMux )
    }
    // else 3 input, turn into 2 X 2 input
    val addSplit = nAdd.getRandomChild()
    val ukck = combineAdd( addSplit._1.getUkNext(), addSplit._1.getCkNext(), nReg.uk, nReg.ck )
    val newAddA = Node( ukck._1, ukck._2 )
    newAddA.addChild( addSplit._1 )
    newAddA.addChild( nReg.getOnlyChild() )
    newAddA.setA()
    val remainChildA = addSplit._2.head
    val remainChildB = addSplit._2.tail.head
    val ukckB = combineAdd( remainChildA.getUkNext(), remainChildA.getCkNext(), remainChildB.getUkNext(), remainChildB.getCkNext() )
    val newAddB = Node( ukckB._1, ukckB._2 )
    newAddB.addChild( remainChildA )
    newAddB.addChild( remainChildB )
    newAddB.setA()
    nReg.removeChildren()
    nAdd.removeChildren()
    nPar.replaceIfChild( nReg, newAddA )
    nPar.replaceIfChild( nAdd, newAddB )
    List( nPar, newAddA, newAddB, nMux )
  }

  private def swapCase19( nPar : Node, nAdd1 : Node, nAdd2 : Node, nMux : Node ) : Seq[Node] = {
    assert( nAdd1.numChildren() >= nAdd2.numChildren(), "nAdd1 should have more children" )
    val chosenChild = nAdd1.getRandomChild()
    if ( nAdd1.numChildren() == 3 ) {
      // take a random child from nAdd1 and put with nAdd2
      val ukck = combineAdd( chosenChild._1.getUkNext(), chosenChild._1.getCkNext(), nAdd2.uk, nAdd2.ck )
      val newAdd1 = Node( ukck._1, ukck._2 )
      newAdd1.addChild( chosenChild._1 )
      newAdd1.addChildren( nAdd2.getChildren() )
      newAdd1.setA()
      val nAdd1_A = chosenChild._2.head
      val nAdd1_B = chosenChild._2.tail.head
      val ukck2 = combineAdd( nAdd1_A.getUkNext(), nAdd1_A.getCkNext(), nAdd1_B.getUkNext(), nAdd1_B.getCkNext() )
      val newAdd2 = Node( ukck2._1, ukck2._2 )
      newAdd2.addChild( nAdd1_A )
      newAdd2.addChild( nAdd1_B )
      newAdd2.setA()
      nAdd1.removeChildren()
      nAdd2.removeChildren()
      nPar.replaceIfChild( nAdd1, newAdd1 )
      nPar.replaceIfChild( nAdd2, newAdd2 )
      return List( nPar, newAdd1, newAdd2, nMux )
    }
    // else both 2 input adds
    val ukck = combineAdd( nAdd2.uk, nAdd2.ck, chosenChild._1.getUkNext(), chosenChild._1.getCkNext() )
    val newAdd2 = Node( ukck._1, ukck._2 )
    newAdd2.addChildren( nAdd2.getChildren() )
    newAdd2.addChild( chosenChild._1 )
    newAdd2.setA()
    val regNode = chosenChild._2.head
    val newReg = Node( regNode.getUkNext(), regNode.getCkNext() )
    newReg.addChild( regNode )
    newReg.setB()
    nAdd2.removeChildren()
    nAdd1.removeChildren()
    nPar.replaceIfChild( nAdd1, newReg )
    nPar.replaceIfChild( nAdd2, newAdd2 )
    List( nPar, newReg, newAdd2, nMux )
  }

  private def swapCase20( nPar : Node, nAdd1 : Node, nAdd2 : Node, nReg : Node ) : Seq[Node] = {
    assert( nAdd1.numChildren() >= nAdd2.numChildren(), "nAdd1 should have more children" )
    if ( nAdd2.numChildren() == 2 ) {
      // combine the reg into nAdd2
      val ukck = combineAdd( nAdd2.uk, nAdd2.ck, nReg.uk, nReg.ck )
      val newAdd2 = Node( ukck._1, ukck._2 )
      newAdd2.addChildren( nAdd2.getChildren() )
      newAdd2.addChild( nReg.getOnlyChild() )
      newAdd2.setA()
      nReg.removeChildren()
      nAdd2.removeChildren()
      nPar.replaceIfChild( nAdd2, newAdd2 )
      nPar.replaceIfChild( nReg, newAdd2 )
      return List( nPar, nAdd1, newAdd2 )
    }
    // else 3 and 3, take one from each and put with reg
    val chosenChild1 = nAdd1.getRandomChild()
    val chosenChild2 = nAdd2.getRandomChild()
    val ukckTmp = combineAdd( chosenChild1._1.getUkNext(), chosenChild1._1.getCkNext(),
      chosenChild2._1.getUkNext(), chosenChild2._1.getCkNext() )
    val ukck3 = combineAdd( ukckTmp._1, ukckTmp._2, nReg.uk, nReg.ck )
    val newAdd3 = Node( ukck3._1, ukck3._2 )
    newAdd3.addChild( chosenChild1._1 )
    newAdd3.addChild( chosenChild2._1 )
    newAdd3.addChild( nReg.getOnlyChild() )
    val otherChild1 = chosenChild1._2.toSet
    val otherChild2 = chosenChild2._2.toSet
    val ukck1 = otherChild1.map( x => ( x.uk, x.ck ) ).reduce( ( x, y ) => combineAdd( x._1, x._2, y._1, y._2 ) )
    val ukck2 = otherChild2.map( x => ( x.uk, x.ck ) ).reduce( ( x, y ) => combineAdd( x._1, x._2, y._1, y._2 ) )
    val newAdd1 = Node( ukck1._1, ukck1._2 )
    val newAdd2 = Node( ukck2._1, ukck2._2 )
    newAdd1.addChildren( otherChild1 )
    newAdd2.addChildren( otherChild2 )
    newAdd1.setA()
    newAdd2.setA()
    newAdd3.setA()
    nReg.removeChildren()
    nAdd1.removeChildren()
    nAdd2.removeChildren()
    nPar.removeChildren()
    nPar.addChildren( Set( newAdd1, newAdd2, newAdd3 ) )
    List( nPar ) ++ nPar.getChildren().toList
  }

  private def swapCase21( nPar : Node ) : Seq[Node] = {
    val nRegs = nPar.getChildren().filter( _.isReg() )
    val nAdd = nPar.getChildren().find( _.isAdd() ).get
    val ukck = nRegs.map( x => ( x.uk, x.ck ) ).reduce( ( x, y ) => combineAdd( x._1, x._2, y._1, y._2 ) )
    val newAdd = Node( ukck._1, ukck._2 )
    newAdd.addChildren( nRegs.map( _.getOnlyChild() ) )
    newAdd.setA()
    for ( n <- nRegs ) {
      n.removeChildren()
      nPar.replaceIfChild( n, newAdd )
    }
    List( nPar, nAdd, newAdd )
  }

  /** Look at two nodes and try to swap them
    */
  def trySwap( nPar : Node, applyIfIncrease : Boolean = true ) : (Seq[Node], Int) = {

    if ( nPar.getChildren().find( _.isC() ).isDefined )
      return (List[Node](), 0)

    // work out how nodes are connected ( should be directly )
    if ( nPar.isReg() ) {
      if ( nPar.getOnlyChild().isAdd() )
        return { if ( applyIfIncrease ) ( swapCase1( nPar ), 1 ) else ( List[Node](), 0 ) }
      if ( nPar.getOnlyChild().isMux() )
        return { if ( applyIfIncrease ) ( swapCase2( nPar ), 2 ) else ( List[Node](), 0) }
      // case 3: two regs so no point as changes nothing
      return (List[Node](), 0)
    }

    val nReg = nPar.getChildren().find( _.isReg() )
    val nAdd = nPar.getChildren().find( _.isAdd() )
    val nMux = nPar.getChildren().find( _.isMux() )

    if ( nPar.isAdd2() ) {
      if ( nReg.isDefined && nAdd.isDefined )
        return ( swapCase4( nPar, nReg.get, nAdd.get ), 4 )
      if ( nAdd.isDefined && !nMux.isDefined && !nReg.isDefined) // must be 2 adds
        return ( swapCase5( nPar ), 5 )
      if ( nReg.isDefined && !nMux.isDefined && !nAdd.isDefined ) // must be 2 regs
        return ( swapCase6( nPar ), 6 )
      if ( nReg.isDefined && nMux.isDefined )
        return ( swapCase7( nPar, nReg.get, nMux.get ), 7 )
      if ( nAdd.isDefined && nMux.isDefined )
        return ( swapCase9( nPar, nAdd.get, nMux.get ), 9 )
      // case 8: cant do 2 muxes so return nothing
      return ( List[Node](), 0 )
    }

    if ( nPar.isMux() ) {
      if ( nReg.isDefined && !nMux.isDefined && !nAdd.isDefined )
          return ( swapCase10( nPar ), 10 )
      if ( nAdd.isDefined && !nMux.isDefined && !nReg.isDefined )
        return ( swapCase11( nPar ), 11 )
      if ( nMux.isDefined && !nReg.isDefined && !nAdd.isDefined )
        return ( swapCase12( nPar ), 12 )
      if ( nReg.isDefined && nMux.isDefined )
        return ( swapCase13( nPar, nReg.get, nMux.get ), 13 )
      // case 14: reg and add, cant do anything
      // case 15: mux and add, cant do anything
      return ( List[Node](), 0 )
    }

    if ( nPar.isAdd3() ) {
      if ( nAdd.isDefined && !nMux.isDefined && !nReg.isDefined )
        return ( swapCase5( nPar ), 5 )
      if ( nReg.isDefined && !nMux.isDefined && !nAdd.isDefined )
        return ( swapCase6( nPar ), 6 )
      if ( nReg.isDefined && nMux.isDefined && nAdd.isDefined )
        return ( swapCase18( nPar, nReg.get, nMux.get, nAdd.get ), 18 )
      if ( nReg.isDefined && nMux.isDefined ) {
        if ( nPar.getChildren().filter( _.isReg() ).size == 2 ) // if 2 reg then case 16
          return ( swapCase16( nPar ), 16 )
        // case 17: 2 mux and reg, cant do anything
        return ( List[Node](), 0 )
      }
      val adds = nPar.getChildren().filter( _.isAdd() ).toList.sortBy( x => -x.numChildren() )
      if ( nAdd.isDefined && nMux.isDefined ) {
        if ( adds.size == 2 ) // if 2 add then case 19
          return ( swapCase19( nPar, adds(0), adds(1), nMux.get ), 19 )
        // case 22: 2 mux and add, cant do anything
        return ( List[Node](), 0 )
      }
      if ( nReg.isDefined && nAdd.isDefined ) {
        if ( adds.size == 2 ) // if 2 add then case 20, if 2 reg case 21
          return ( swapCase20( nPar, adds(0), adds(1), nReg.get ), 20 )
        return ( swapCase21( nPar ), 21 )
      }
      // case 23: if 3 muxes can't do anything
      return ( List[Node](), 0 )
    }

    // should never reach here
    assert( false, "Parent:" + nPar + ", children = " + nPar.getChildren() )

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
      if ( n.isA() || n.isReg() ) {
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
    n1.addChildren( nA.getChildren() )
    n2.addChildren( nA.getChildren() )
    if ( nA.isA() ) {
      n1.setA()
      n2.setA()
    }
    if ( nA.isB() ) {
      n1.setB()
      n2.setB()
    }
    for ( p <- n1Par )
      p.replaceIfChild( nA, n1 )
    for ( p <- n2Par )
      p.replaceIfChild( nA, n2 )

    return List( n1, n2 )
  }

}
