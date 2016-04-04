import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import Chisel._
import chiselutils.interfaces.exanicx4._

class SimpleAdderSuite extends TestSuite {

  @Test def simpleAdder {

    class SimpleAdder extends TimeSeriesInterface( UInt(width = 8), 15 ) {
      val addLayer1 = (0 until 4).map( x => { RegNext(io.dataIn.bits(2*x) + io.dataIn.bits(2*x + 1)) } )
      val addLayer2 = (0 until 2).map( x => { RegNext(addLayer1(2*x) + addLayer1(2*x + 1)) } )
      val fifo = Module( new Queue( UInt( width = 8 ), 32 ) )
      fifo.io.enq.bits := RegNext(addLayer2(0) + addLayer2(1))
      fifo.io.enq.valid := ShiftRegister( io.dataIn.valid, 3 )
      io.dataOut <> fifo.io.deq
      io.regOut(0) := fifo.io.count
        (1 until 9).foreach( idx => { io.regOut(idx) := io.dataIn.bits(idx - 1) } )
        (9 until 15).foreach( idx => { io.regOut(idx) := UInt( 0 ) } )
      io.memAddr := UInt( 0 )
      io.regOutEn := fifo.io.deq.valid
      io.dataIn.ready := Bool(true)
      io.error := !fifo.io.enq.ready || !io.dataOut.ready
    }

    def simpleAddFunct() = new SimpleAdder

    class ExanicX4Test extends ExanicX4TimeSeries( UInt( width = 8 ), 32, 128, simpleAddFunct, 100 )

    chiselMain(Array("--backend", "v", "--wio", "--targetDir", "test-outputs/"),
      () => Module( new ExanicX4Test ) )

    assertFile("SimpleAdderSuite_ExanicX4Test_1.v")
  }
}
