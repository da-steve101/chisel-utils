package ChiselUtils.Interfaces

import Chisel._

class RIFFAModule(val userDataWidth : Int) extends Module {
  val io = new Bundle {
    val rx_data  = UInt(INPUT, width=userDataWidth)
    val rx_ready = Bool(INPUT)
    val rx_done  = Bool(OUTPUT)

    val tx_data  = UInt(OUTPUT, width=userDataWidth)
    val tx_ready = Bool(OUTPUT)
    val tx_done  = Bool(INPUT)
  }
}

class RIFFA[T <: RIFFAModule](val C_PCI_DATA_WIDTH : Int, val numInputs : Int,
  val numOutputs : Int, val userConstructor : () => T) extends Module {
  val io = new Bundle {
    // RX
    val CHNL_RX_ACK         = Bool(OUTPUT)
    val CHNL_RX_DATA_REN    = Bool(OUTPUT)
    val CHNL_RX             = Bool(INPUT)
    val CHNL_RX_LAST        = Bool(INPUT)
    val CHNL_RX_LEN         = UInt(INPUT, width=32)
    val CHNL_RX_OFF         = UInt(INPUT, width=31)
    val CHNL_RX_DATA        = UInt(INPUT, width=(32*C_PCI_DATA_WIDTH))
    val CHNL_RX_DATA_VALID  = Bool(INPUT)

    // TX
    val CHNL_TX_ACK         = Bool(INPUT)
    val CHNL_TX_DATA_REN    = Bool(INPUT)
    val CHNL_TX             = Bool(OUTPUT)
    val CHNL_TX_LAST        = Bool(OUTPUT)
    val CHNL_TX_LEN         = UInt(OUTPUT, width=32)
    val CHNL_TX_OFF         = UInt(OUTPUT, width=31)
    val CHNL_TX_DATA        = UInt(OUTPUT, width=(32*C_PCI_DATA_WIDTH))
    val CHNL_TX_DATA_VALID  = Bool(OUTPUT)
  }

  val userModule = Module(userConstructor())

  val rxIdle :: rxActive :: Nil = Enum(UInt(), 2)
  val rxState = Reg(init=rxIdle)
  val rxLen   = Reg(init=UInt(numInputs, width=32))
  val rxCount = Reg(init=UInt(0, width=32))
  val rxData  = Reg(init=UInt(0, width=(32*C_PCI_DATA_WIDTH)))

  val txIdle :: txActive :: Nil = Enum(UInt(), 2)
  val txState = Reg(init = txIdle)
  val txLen   = Reg(init=UInt(numOutputs, width=32))
  val txCount = Reg(init=UInt(0, width=32))
  val txData  = Reg(init=UInt(0, width=(32*C_PCI_DATA_WIDTH)))

  // Insert User Module
  /* Insert User Module
   * User Module need Ready and Done Signals
   */
  val userRX    = Reg(init=Bool(false))
  val userTX    = Reg(init=Bool(false))
  // This will sum all of the values set down and then send back a single result
  userTX := (rxState === rxActive)
  userRX := (rxState === rxIdle)

  // RX State Machine
  when (rxState === rxIdle && userRX) {
    when (io.CHNL_RX) {
      //rxLen := io.CHNL_RX_LEN
      rxCount := UInt(0)
      rxState := rxActive
    }
  }
  when (rxState === rxActive) {
    when (io.CHNL_RX_DATA_VALID) {
      rxData := io.CHNL_RX_DATA
      rxCount := rxCount + UInt(C_PCI_DATA_WIDTH)
    }
    when (rxCount >= rxLen) {
      rxState := rxIdle
    }
  }

  // RX Logic
  io.CHNL_RX_ACK := (rxState === rxActive)
  io.CHNL_RX_DATA_REN := (rxState === rxActive)


  // TX State Machine
  when (txState === txIdle && userTX) {
    txCount := UInt(C_PCI_DATA_WIDTH)
    txState := txActive
  }
  when (txState === txActive) {
    txCount := txCount + UInt(C_PCI_DATA_WIDTH)
    when (txCount >= txLen) {
      txState := txIdle
    }
  }

  // TX Logic
  io.CHNL_TX := (txState === txActive)
  io.CHNL_TX_DATA_VALID := (txState === txActive)
  io.CHNL_TX_DATA := txData
  // These will need to be modified if you want to transfer multiple experiments
  io.CHNL_TX_OFF := UInt(0, width=31)
  io.CHNL_TX_LAST := Bool(true)
}
