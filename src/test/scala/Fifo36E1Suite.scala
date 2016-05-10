import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

import Chisel._
import chiselutils.xilinx._

class Fifo36E1TestSuite extends TestSuite {

  @Test def fifo36E1() {

    class UserTop extends Module {
      val io = new QueueIO( UInt( width = 72 ), 500 )
      val enqClk = Clock()
      val deqClk = Clock()
      val testFifo = Module( new Fifo36E1( 72, 100, 500, enqClk, deqClk ) )
      testFifo.io.din := io.enq.bits( 71, 8 )
      testFifo.io.dip := io.enq.bits( 7, 0 )
      testFifo.io.wren := io.enq.valid
      io.enq.ready := !testFifo.io.full
      io.deq.valid := !testFifo.io.empty
      testFifo.io.rden := io.deq.ready
      io.deq.bits := testFifo.io.dout ## testFifo.io.dop
    }

    chiselMain( Array("--backend", "v", "--targetDir", dir.getPath.toString()), () => Module( new UserTop ) )
    assertFile( "Fifo36E1TestSuite_UserTop_1.v" )
  }

}
