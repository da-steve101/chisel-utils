
package chiselutils.algorithms

/** Data structure for holding nodes
  * Allows constant random selection and log insert/remove
  */
class NodeSet( nodesIn : Set[Node] ) {
  private val myTRand = java.util.concurrent.ThreadLocalRandom.current()

  private val nodes = scala.collection.mutable.Set[Node]() ++ nodesIn

  def randomNode() : ( Node, Boolean ) = synchronized {
    val nRand = myTRand.nextInt(0, nodes.size)
    val node = nodes.iterator.drop( nRand ).next
    val nodeLock = node.lockNode()
    ( node, nodeLock )
  }

  def remove( n : Node ) : Unit = synchronized {
    // get its index
    nodes -= n
  }

  def add( n : Node ) : Unit = synchronized {
    // find where it should be put
    nodes += n
  }

  def size() = synchronized { nodes.size }

  def contains( n : Node ) : Boolean = synchronized {
    nodes.contains( n )
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
