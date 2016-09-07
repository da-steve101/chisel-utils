import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import chiselutils.algorithms.PmcmILP
import chiselutils.exceptions.NoResultException

class PmcmILPSuite extends TestSuite {

  @Test def testSolve {
    val numsIn = List( 53, 89, 111 ).map( x => BigInt(x) )

    println( PmcmILP.solveILP( numsIn ) )
  }

}
