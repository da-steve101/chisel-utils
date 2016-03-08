package chiselutils.interfaces

import Chisel._

class RIFFA[Tx <: Data, Rx <: Data, T <: Interfaceable[Tx, Rx]](val C_PCI_DATA_WIDTH : Int,
  val txFifoEntries : Int, val rxFifoEntries : Int, val userConstructor : () => T) extends Module {

  Predef.assert(C_PCI_DATA_WIDTH == 32 || C_PCI_DATA_WIDTH == 64 || C_PCI_DATA_WIDTH == 128,
    "C_PCI_DATA_WIDTH must be 32, 64 or 128")

  val io = new Bundle {
    // RX
    val rxAck       = Bool(OUTPUT);                            rxAck.setName("CHNL_RX_ACK")
    val rxReady     = Bool(OUTPUT);                            rxReady.setName("CHNL_RX_DATA_REN")
    val rxOn        = Bool(INPUT);                             rxOn.setName("CHNL_RX")
    val rxLast      = Bool(INPUT);                             rxLast.setName("CHNL_RX_LAST")
    val rxLen       = UInt(INPUT, width=32);                   rxLen.setName("CHNL_RX_LEN")
    val rxOff       = UInt(INPUT, width=31);                   rxOff.setName("CHNL_RX_OFF")
    val rxData      = Bits(INPUT, width=C_PCI_DATA_WIDTH);     rxData.setName("CHNL_RX_DATA")
    val rxDataValid = Bool(INPUT);                             rxDataValid.setName("CHNL_RX_DATA_VALID")

    // TX
    val txAck       = Bool(INPUT);                             txAck.setName("CHNL_TX_ACK")
    val txReady     = Bool(INPUT);                             txReady.setName("CHNL_TX_DATA_REN")
    val txOn        = Bool(OUTPUT);                            txOn.setName("CHNL_TX")
    val txLast      = Bool(OUTPUT);                            txLast.setName("CHNL_TX_LAST")
    val txLen       = UInt(OUTPUT, width=32);                  txLen.setName("CHNL_TX_LEN")
    val txOff       = UInt(OUTPUT, width=31);                  txOff.setName("CHNL_TX_OFF")
    val txData      = Bits(OUTPUT, width=C_PCI_DATA_WIDTH);    txData.setName("CHNL_TX_DATA")
    val txDataValid = Bool(OUTPUT);                            txDataValid.setName("CHNL_TX_DATA_VALID")
  }

  // create the user module
  val userModule = Module(userConstructor())

  // Create the tx and rx clocks, the implicit clock is for user
  val txReset = Bool(false)
  txReset.setName("txReset")
  val rxReset = Bool(false)
  rxReset.setName("rxReset")
  addResetPin(txReset)
  addResetPin(rxReset)
  val txClock = new Clock(txReset)
  val rxClock = new Clock(rxReset)
  txClock.setName("CHNL_TX_CLOCK")
  rxClock.setName("CHNL_RX_CLOCK")

  /*
   Policy:
   if value is smaller than PCI transfer, assume that consists of multiple
   in each data packet.
   eg, size of 8 with DATA_WIDTH = 32 means 4 contained fill from least significant
   eg, size of 17 means least sig 17 are used, 15 msb are discarded
   eg, size of 33 means two transfers are used, 31 msb in second transfer are discarded
   same on tx (will be padded with zeros)
   */

  // Rx Async Data - Recieve data and determine the splitting of the bits
  val rxDataWidth = userModule.io.rx.bits.getWidth()
  val rxMultiTransfer = (rxDataWidth > C_PCI_DATA_WIDTH)
  val rxNoTransfers = (rxDataWidth/C_PCI_DATA_WIDTH) + 1
  val rxSerialize = (rxDataWidth <= (C_PCI_DATA_WIDTH/2))
  val rxNoSerial = C_PCI_DATA_WIDTH/rxDataWidth
  val rxSerialCounter = Reg(init=UInt(0,width=log2Up(rxNoSerial)), clock = rxClock)
  val rxMultiCounter = Reg(init=UInt(0,width=log2Up(rxNoTransfers)), clock = rxClock)
  val rxSerialVec = Vec(Bits(width=rxDataWidth), rxNoSerial)
  val rxSerialSection = rxSerialVec(rxSerialCounter)
  rxSerialVec := io.rxData(rxDataWidth*rxNoSerial - 1, 0)

  val rxFifo = Module(new AsyncFifo[Rx](userModule.io.rx.bits, rxFifoEntries, rxClock, Driver.implicitClock))
  rxFifo.io.enq.valid := Bool(false) // Set default

  // Tx Async Data - Join Tx data into packet size chunks with last asserted properly
  val txDataWidth = userModule.io.tx.bits.getWidth()
  val txMultiTransfer = (txDataWidth > C_PCI_DATA_WIDTH)
  val txNoTransfers = (txDataWidth/C_PCI_DATA_WIDTH) + 1
  val txSerialize = (txDataWidth <= (C_PCI_DATA_WIDTH/2))
  val txNoSerial = C_PCI_DATA_WIDTH/txDataWidth

  val txFifo = Module(new AsyncFifo[Tx](userModule.io.tx.bits, txFifoEntries, Driver.implicitClock, txClock))
  txFifo.io.deq.ready := Bool(false) // Set default

  // Rx state machine
  val rxIdle :: rxActive :: Nil = Enum(UInt(), 2)
  val rxState = Reg(init=rxIdle, clock = rxClock)
  val rxFormatState = Reg(init=rxIdle, clock = rxClock)
  val rxLen   = Reg(init=UInt(rxNoTransfers, width=32), clock = rxClock)
  val rxCount = Reg(init=UInt(0, width=32), clock = rxClock)
  val rxData  = Reg(init=Bits(0, width=C_PCI_DATA_WIDTH), clock = rxClock)

  // State machine to read into FIFO, rely on RIFFA not to overflow fifo
  rxLen := io.rxLen
  rxCount := UInt(0)
  rxState := rxIdle

  when ( rxState === rxIdle ) {
    rxLen   := io.rxLen
    rxCount := UInt(0)
    when ( io.rxOn ) {
      rxState := rxActive
    }
  } .otherwise {
    when ( io.rxDataValid ) {
      rxData := io.rxData
      rxCount := rxCount + UInt(C_PCI_DATA_WIDTH/32)
    }
    when ( rxCount >= rxLen ) {
      rxState := rxIdle
    }
  }

  // State machine to format the Rx data
  if ( rxSerialize ) {
    // switch between various sections until all queued
    rxFifo.io.enq.bits := rxSerialSection

    // defaults
    io.rxReady := Bool(false)
    io.rxAck   := Bool(false)
    when ( rxFormatState === rxIdle ) {
      when ( io.rxDataValid && rxFifo.io.enq.ready ) {
        rxFormatState := rxActive
        rxFifo.io.enq.valid := Bool(true)
        rxSerialCounter := UInt(1, log2Up(rxNoSerial))
      } .otherwise {
        rxFifo.io.enq.valid := Bool(false)
        rxSerialCounter := UInt(0, log2Up(rxNoSerial))
      }
      io.rxReady := Bool(false)
    } .otherwise {
      when ( io.rxDataValid && rxFifo.io.enq.ready ) {
        rxFifo.io.enq.valid := Bool(true)
        rxSerialCounter := rxSerialCounter + UInt(1)
      } .otherwise {
        rxFifo.io.enq.valid := Bool(false)
      }
      when ( rxSerialCounter === UInt(rxNoSerial - 1) ) {
        io.rxReady := Bool(true)
      } .otherwise {
        io.rxReady := Bool(false)
      }
    }
  } else if ( rxMultiTransfer ) {
    // use rxData reg to build up each transfer
    rxFifo.io.enq.bits := rxData

    switch ( rxFormatState ) {
      is ( rxIdle ) {
        when ( io.rxDataValid && rxFifo.io.enq.ready ) {
          rxFormatState := rxActive
          io.rxReady := Bool(true)
        } .otherwise {
          io.rxReady := Bool(false)
        }
        rxMultiCounter := UInt(1)
        rxData ( C_PCI_DATA_WIDTH - 1, 0 ) := io.rxData
        rxFifo.io.enq.valid := Bool(false)
      }
      is ( rxActive ) {
        io.rxReady := Bool(true)
        when ( io.rxDataValid ) {
          for ( i <- 1 until rxNoTransfers - 1 ) {
            when ( rxMultiCounter === UInt(i) ) {
              rxData( (i+1)*C_PCI_DATA_WIDTH - 1, i*C_PCI_DATA_WIDTH ) := io.rxData
            }
          }
          when ( rxMultiCounter === UInt(rxNoTransfers - 1) ) {
            rxData( rxNoTransfers*C_PCI_DATA_WIDTH - 1, (rxNoTransfers - 1)*C_PCI_DATA_WIDTH ) := io.rxData
            rxFifo.io.enq.valid := Bool(true)
            rxFormatState := rxIdle
          } .otherwise {
            rxFifo.io.enq.valid := Bool(false)
          }
          rxMultiCounter := rxMultiCounter + UInt(1)
        } .otherwise {
          rxFifo.io.enq.valid := Bool(false)
        }
      }
    }
  } else {
    // The simple case, 1 transfer = 1 use segment
    rxFifo.io.enq.bits := io.rxData(rxDataWidth - 1, 0)
    io.rxReady := rxFifo.io.enq.ready
    rxFifo.io.enq.valid := (io.rxDataValid && rxFifo.io.enq.ready)
  }

  // RX Logic
  io.rxAck := (rxState === rxActive)
  rxFifo.io.deq <> userModule.io.rx

  // TX State Machine
  val txIdle :: txActive :: Nil = Enum(UInt(), 2)
  val txState = Reg(init = txIdle, clock = txClock)
  val txCount = Reg(init=UInt(0, width=32), clock = txClock)
  val txData  = Reg(init=Bits(0, width=C_PCI_DATA_WIDTH), clock = txClock)
  val txSerialCounter = Reg(init=UInt(0, width=log2Up(txNoSerial + 1)), clock = txClock)

  if ( txSerialize ) {
    io.txData := txData
    io.txLast := Bool(true)
    for ( i <- 0 until txNoSerial ) {
      when ( txSerialCounter === UInt(i) ) {
        txData((i + 1)*txDataWidth - 1, i*txDataWidth) := txFifo.io.deq.bits
      }
    }

    // defaults
    io.txOn := Bool(false)
    io.txDataValid := Bool(false)

    switch ( txState ) {
      is ( txIdle ) {
        // Collect serial data into txData
        txFifo.io.deq.ready := txFifo.io.deq.valid
        io.txOn := Bool(false)
        when ( txFifo.io.deq.valid ) {
          txSerialCounter := txSerialCounter + UInt(1)
        }
        when ( ( txSerialCounter === UInt(txNoSerial - 2) ) && txFifo.io.deq.valid ) {
          txState := txActive
        }
        io.txDataValid := Bool(false)
      }
      is ( txActive ) {
        io.txDataValid := txFifo.io.deq.valid
        io.txOn        := (txFifo.io.deq.valid && io.txReady)
        txFifo.io.deq.ready := (io.txAck && io.txReady)
        when ( io.txAck && io.txReady ) {
          txState := txIdle
        }
      }
    }
  } else if ( txMultiTransfer ) {
    /*
    txCount := UInt(C_PCI_DATA_WIDTH)
    when (txState === txActive) {
      txCount := txCount + UInt(C_PCI_DATA_WIDTH)
      when (txCount >= txLen) {
        txState := txIdle
      }
    }*/
  } else {
    // The simple case, 1 transfer = 1 used segment
    io.txData := txFifo.io.deq.bits
    io.txLast := Bool(true)
    io.txDataValid := txFifo.io.deq.valid
    switch ( txState ) {
      is ( txIdle ) {
        txFifo.io.deq.ready := Bool(false)
        io.txOn          := Bool(false)
        when ( txFifo.io.deq.valid ) {
          txState := txActive
        }
      }
      is ( txActive ) {
        txFifo.io.deq.ready := ( io.txReady && io.txAck )
        io.txOn          := txFifo.io.deq.valid
        when ( io.txAck ) {
          txState := txIdle
        }
      }
    }
  }
  txFifo.io.enq <> userModule.io.tx

  // TX Logic
  io.txOff  := UInt(0, width=31)
  io.txLen  := UInt(txNoTransfers*(C_PCI_DATA_WIDTH/32), width=32)
}
