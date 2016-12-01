
package chiselutils.algorithms

/** Data structure for holding nodes
  * Allows constant random selection and log insert/remove
  */
class NodeSet( nodesIn : Set[Node] ) {
  private val nodes = new collection.mutable.HashMap[Node, Int]() ++ nodesIn.zip( 0 until nodesIn.size )
  private val nodesInv = nodes.map(_.swap)
  private val myTRand = java.util.concurrent.ThreadLocalRandom.current()

  // an unsorted stack of index
  private var idxAvailable = List[Int]()
  // a sorted vector
  private var idxUsed = Vector[Int]() ++ ( 0 until nodesIn.size )

  private def getIdx( i : Int ) : Int = {
    var min = 0
    var max = idxUsed.size - 1
    while ( min <= max ) {
      val mid = ( min + max ) >> 1
      if ( idxUsed( mid ) == i )
        return mid
      if ( i < idxUsed( mid ) )
        max = mid - 1
      else
        min = mid + 1
    }
    min
  }

  private def verifySorted() = {
    for ( i <- 1 until idxUsed.size )
      assert( idxUsed( i - 1 ) < idxUsed( i ),  "Not sorted" )
  }


  def randomNode() : ( Node, Boolean ) = synchronized {
    val nRand = myTRand.nextInt(0, idxUsed.size)
    val idx = idxUsed( nRand )
    val node = nodesInv( idx )
    assert( nodes.contains( node ) && nodes( node ) == idx, "Must contain node" )
    val nodeLock = node.lockNode()
    ( node, nodeLock )
  }

  def remove( n : Node ) : Unit = synchronized {
    // get its index
    val idx = nodes( n )
    // put on stack of available
    idxAvailable = idx :: idxAvailable
    // remove from used ( bin search log(n) )
    val vecIdx = getIdx( idx )
    assert( idxUsed( vecIdx ) == idx, "Must remove correct idx" )
    idxUsed = idxUsed.patch( vecIdx, Nil, 1 ) // this is constant right???
    // remove from maps
    assert( nodes.contains( n ) && nodesInv( idx ) == n, "Must contain node" )
    nodes -= n
    nodesInv -= idx
    // verifySorted()
  }

  def add( n : Node ) : Unit = synchronized {
    // assign it an index
    val idx = {
      if ( idxAvailable.isEmpty )
        nodes.size
      else {
        val tmp = idxAvailable.head
        idxAvailable = idxAvailable.tail
        tmp
      }
    }
    // find where it should be put
    val vecIdx = getIdx( idx )
    idxUsed = idxUsed.patch( vecIdx, Vector(idx), 0 )
    // add to maps
    assert( !nodes.contains( n ) && !nodesInv.contains( idx ), "Must not contain node" )
    nodes.put( n, idx )
    nodesInv.put( idx, n )
    // verifySorted()
  }

  def contains( n : Node ) = { nodes.contains( n ) }
  def contains( i : Int ) = { nodesInv.contains( i ) }

  def size() = { nodes.size }

  def -=( n : Node ) = { remove(n) }
  def +=( n : Node ) = { add(n) }
  def ++=( nodeList : Seq[Node] ) = synchronized {
    for( n <- nodeList )
      add( n )
  }

  def foreach[U](f: Node => U) = nodes.keys.foreach( x => f(x) )

  def toSet : Set[Node] = {
    nodes.keys.toSet
  }
}
