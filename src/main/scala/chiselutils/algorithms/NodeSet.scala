
package chiselutils.algorithms

/** Data structure for holding nodes
  * Allows constant random selection and log insert/remove
  */
class NodeSet( nodesIn : Set[Node] ) {
  private val myTRand = java.util.concurrent.ThreadLocalRandom.current()

  private var idxAvailable = List[Int]()
  // a sorted vector
  private var nodes = nodesIn.zipWithIndex.map( n => {
    n._1.hashIdx = n._2
    n._1
  }).toVector

  private def getIdx( n : Node ) : Int = synchronized {
    var min = 0
    var max = nodes.size - 1
    val nHash = n.hashIdx
    while ( min <= max ) {
      val mid = ( min + max ) >> 1
      if ( nodes( mid ).hashIdx == nHash )
        return mid
      if ( nHash < nodes( mid ).hashIdx )
        max = mid - 1
      else
        min = mid + 1
    }
    min
  }

  private def verifySorted( s : String ) = synchronized {
    for ( i <- 1 until nodes.size )
      assert( nodes( i - 1 ).hashIdx < nodes( i ).hashIdx,  "Not sorted: " + s )
  }


  def randomNode() : ( Node, Boolean ) = synchronized {
    val nRand = myTRand.nextInt(0, nodes.size)
    val node = nodes( nRand )
    val nodeLock = node.lockNode()
    ( node, nodeLock )
  }

  def remove( n : Node ) : Unit = synchronized {
    // get its index
    val idx = getIdx( n )
    assert( n == nodes( idx ), "Trying to remove bad node" )
    nodes = nodes.patch( idx, Nil, 1 ) // this is constant right???
    idxAvailable = n.hashIdx :: idxAvailable
  }

  def add( n : Node ) : Unit = synchronized {
    // find where it should be put
    if ( idxAvailable.isEmpty )
      n.hashIdx = nodes.size
    else {
      n.hashIdx = idxAvailable.head
      idxAvailable = idxAvailable.tail
    }
    val idx = getIdx( n )
    assert( n != nodes( math.min( idx, nodes.size - 1 ) ), "Trying to add dup node" )
    nodes = nodes.patch( idx, Vector(n), 0 )
  }

  def size() = synchronized { nodes.size }

  def contains( n : Node ) : Boolean = synchronized {
    val idx = getIdx( n )
    nodes( idx ) == n
  }

  def -=( n : Node ) = { remove(n) }
  def +=( n : Node ) = { add(n) }
  def ++=( nodeList : Iterable[Node] ) = synchronized {
    for( n <- nodeList )
      add( n )
  }

  def foreach[U](f: Node => U) = nodes.foreach( x => f(x) )

  def toSet : Set[Node] = {
    nodes.toSet
  }
}
