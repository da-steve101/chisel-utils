/** A fully pipelined reduction circuit based on
  * "Modular Design of fully pipelined reduction circuits" Huang, Andrews
  */

package chiselutils.math

import Chisel._

private class DataAndLast [ T <: Data ] ( genType : T ) extends Bundle {
  val data = genType.cloneType
  val last = Bool()
  override def cloneType() = {
    new DataAndLast(genType.cloneType).asInstanceOf[this.type]
  }
}

private class DualFifoBlock[ T <: Data]( genType : T, depth : Int , opLatency : Int) extends Module {
  val dataAndLastType = new DataAndLast(genType)

  val io = new Bundle {
    val dataIn = dataAndLastType.cloneType.asInput
    val valid = Bool(INPUT)
    val deq = Bool(INPUT)
    val dataOutA = dataAndLastType.cloneType.asOutput
    val dataOutB = dataAndLastType.cloneType.asOutput
    val srOut = dataAndLastType.cloneType.asOutput
  }

  val fifoSelReg = RegInit(Bool(false))
  val fifoA = Module(new Queue(dataAndLastType.cloneType, depth, true))
  val fifoB = Module(new Queue(dataAndLastType.cloneType, depth, true))
  val srData = dataAndLastType.cloneType
  srData.data := fifoA.io.deq.bits.data
  srData.last := fifoA.io.deq.bits.last && fifoA.io.deq.valid
  val shiftReg = ShiftRegister(srData, opLatency)
  when ( io.valid ) {
    fifoSelReg := !fifoSelReg
    when ( io.dataIn.last ) {
      fifoSelReg := Bool(false)
    }
    fifoA.io.enq.valid := !fifoSelReg && io.valid
    fifoB.io.enq.valid := fifoSelReg && io.valid
  }
  io.dataOutA := fifoA.io.deq.bits
  io.dataOutB := fifoB.io.deq.bits
  io.srOut := shiftReg
  fifoA.io.deq.ready := io.deq | fifoA.io.deq.bits.last
  fifoB.io.deq.ready := io.deq
}

private class AccBlock[ T <: Data ] ( genType : T ) extends Module {
  val dataAndLastType = new DataAndLast(genType)
  val io = new Bundle {
    val toFifo = dataAndLastType.cloneType.asInput
    val adderRes = dataAndLastType.cloneType.asInput
    val adderResValid = Bool(INPUT)
    val result = genType.cloneType
    val resultValid = Bool(OUTPUT)
  }
}

class Accumulator[T <: Data] ( genType : T, reductionOp : ( T, T) => T, opLatency : Int) extends Module {

  Predef.assert(opLatency > 0, "opLatency must be atleast 1")

  val io = new Bundle {
    val dataIn = genType.cloneType.asInput
    val valid = Bool(INPUT)
    val last = Bool(INPUT)
    val result = genType.cloneType.asOutput
    val resultValid = Bool(OUTPUT)
  }
  val stages = log2Ceil(opLatency)
  private val dataAndLastType = new DataAndLast(genType)

  private val inputA = dataAndLastType.cloneType
  private val inputB = dataAndLastType.cloneType
  val opRes = reductionOp(inputA.data, inputB.data)
  val submitLvl = UInt( width = stages + 1 )
  val lvlSR = ShiftRegister( submitLvl, opLatency )
  val lastSR = ShiftRegister( inputB.last, opLatency )

  // create fifos according to depth bound in paper
  private val fifos = (0 until stages).map( x => Module(new DualFifoBlock( genType , stages - x, opLatency)) )

  // final queue
  private val accFifo = Module( new Queue( dataAndLastType.cloneType, opLatency, true ) )
  // final sr
  val accSR = ShiftRegister(accFifo.io.deq.bits.data, opLatency)
  val validSR = ShiftRegister(accFifo.io.deq.bits.last && accFifo.io.deq.valid, opLatency)

  val initFifo =  if ( stages > 0 ) {
    fifos(0).io.valid := io.valid
    fifos(0).io.dataIn.data := io.dataIn
    fifos(0).io.dataIn.last := io.last
  } else {
    accFifo.io.enq.valid := io.valid
    accFifo.io.enq.bits.data := io.dataIn
    accFifo.io.enq.bits.last := io.last
  }

  (1 until stages).foreach( x => {
    fifos(x).io.dataIn.data := opRes
    fifos(x).io.dataIn.last := lastSR
    fifos(x).io.valid := lvlSR(x - 1)
  })

  val prevWasLast = RegInit(Bool(true))
  prevWasLast := accFifo.io.deq.bits.last && accFifo.io.deq.ready

  val accValue = Reg(genType.cloneType)
  val accReadyReg = RegInit(Bool(false))
  when ( prevWasLast && accFifo.io.deq.valid ) {
    accValue := accFifo.io.deq.bits.data
    accReadyReg := Bool(true)
  }

  accFifo.io.deq.ready := ( prevWasLast && accFifo.io.deq.valid ) || ( accFifo.io.deq.bits.last && accFifo.io.deq.valid )


}
