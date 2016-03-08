package chiselutils.interfaces

import Chisel._

class HARPIO_E extends Bundle {
  val splEnable = Bool(INPUT)

  // AFU TX read request
  val splTxRdAlmostFull = Bool(INPUT)
  val afuTxRdValid = Bool(OUTPUT)
  val afuTxRdHdr = UInt(OUTPUT, 99)

  // AFU TX write request
  val splTxWrAlmostFull = Bool(INPUT)
  val afuTxWrValid = Bool(OUTPUT)
  val afuTxIntrValid = Bool(OUTPUT)
  val afuTxWrHdr = UInt(OUTPUT, 99)
  val afuTxData = UInt(OUTPUT, 512)

  // AFU RX read response
  val splRxRdValid = Bool(INPUT)
  val splRxWrValid0 = Bool(INPUT)
  val splRxCfgValid = Bool(INPUT)
  val splRxIntrValid0 = Bool(INPUT)
  val splRxUmsgValid = Bool(INPUT)
  val splRxHdr0 = UInt(INPUT, 18)
  val splRxData = UInt(INPUT, 512)

  // AFU RX write response
  val splRxWrValid1 = Bool(INPUT)
  val splRxIntrValid1 = Bool(INPUT)
  val splRxHdr1 = UInt(INPUT, 18)

  // Set names to match
  splEnable.setName("spl_enable")
  splTxRdAlmostFull.setName("spl_tx_rd_almostfull")
  afuTxRdValid.setName("afu_tx_rd_valid")
  afuTxRdHdr.setName("afu_tx_rd_hdr")
  splTxWrAlmostFull.setName("spl_tx_wr_almostfull")
  afuTxWrValid.setName("afu_tx_wr_valid")
  afuTxIntrValid.setName("afu_tx_intr_valid")
  afuTxWrHdr.setName("afu_tx_wr_hdr")
  afuTxData.setName("afu_tx_data")
  splRxRdValid.setName("spl_rx_rd_valid")
  splRxWrValid0.setName("spl_rx_wr_valid0")
  splRxCfgValid.setName("spl_rx_cfg_valid")
  splRxIntrValid0.setName("spl_rx_intr_valid0")
  splRxUmsgValid.setName("spl_rx_umsg_valid")
  splRxHdr0.setName("spl_rx_hdr0")
  splRxData.setName("spl_rx_data")
  splRxWrValid1.setName("spl_rx_wr_valid1")
  splRxIntrValid1.setName("spl_rx_intr_valid1")
  splRxHdr1.setName("spl_rx_hdr1")
}
  

class HARPModule extends Module {
  val io = new HARPIO_E

  setModuleName("afu_top")

  // Constants
  val AFU_CSR_DSR_BASEL = Lit("b000000", 6) { UInt() }
  val AFU_CSR_DSR_BASEH = Lit("b000001", 6) { UInt() }
  val AFU_CSR_CTX_BASEL = Lit("b000010", 6) { UInt() }
  val AFU_CSR_CTX_BASEH = Lit("b000011", 6) { UInt() }
  val CSR_ID = Lit("h8a", 8) { UInt() }
  val CCI_REQ_WR = UInt(2, 4)
  val CCI_REQ_RD = UInt(4, 4)
  val CCI_REQ_WR_FENCE = UInt(5, 4)

  // CSR Registers
  val csrCtxBaseValid = RegInit(Bool(false))
  val csrIdValid = RegInit(Bool(false))
  val csrIdAddr = Reg(UInt(width=32))
  val csrCtxBase = Reg(UInt(width=58))
  val afuDsrBase = Reg(UInt(width=32))

  // IO Registers
  val afuTxRdValid = RegInit(Bool(false))
  val afuTxRdHdr = Reg(UInt(width=99))
  val afuTxData = Reg(UInt(width = 512))
  val ioRxCsrAddr = Reg(UInt(width = 14))
  val ioRxCsrData = Reg(UInt(width = 32))
  val afuTxWrValid = RegInit(Bool(false))
  val afuTxIntrValid = RegInit(Bool(false))
  val afuTxWrHdr = Reg(UInt(width = 99))
  val ioRxRdValid = RegInit(Bool(false))
  val ioRxCsrValid = RegInit(Bool(false))
  val ioRxData = Reg(UInt(width = 512))
  val txWrTag = RegInit(UInt(0, width = 6)) // tag to keep track of writes
  val txRdTag = RegInit(UInt(0, width = 6)) // tag to keep track of reads
  val txWrBlock = RegInit(Bool(false))
  val txWrBlockCnt = Reg(UInt(width = 6))

  // Core Registers
  val corTxRdValid = RegInit(Bool(false))
  val corTxRdAddr = Reg(UInt(width = 58))
  val corTxRdLen = Reg(UInt(width = 5))
  val corTxWrValid = RegInit(Bool(false))
  val corTxDsrValid = RegInit(Bool(false))
  val corTxFenceValid = RegInit(Bool(false))
  val corTxDoneValid = RegInit(Bool(false))
  val corTxWrAddr = Reg(UInt(width = 58))
  val corTxWrLen = Reg(UInt(width = 5))
  val corTxData = Reg(UInt(width = 511))
  val csrIdDone = RegInit(Bool(false))

  // ---------------------------------------------------------------------
  // IO Output Connections
  // ---------------------------------------------------------------------
  io.afuTxRdValid := afuTxRdValid
  io.afuTxRdHdr := afuTxRdHdr
  io.afuTxWrValid := afuTxWrValid
  io.afuTxIntrValid := afuTxIntrValid
  io.afuTxWrHdr := afuTxWrHdr
  io.afuTxData := afuTxData

  // ---------------------------------------------------------------------
  // CSR logic
  // ---------------------------------------------------------------------

  // When the AFU indicates that the id was read, set valid to false
  when ( csrIdDone ) { csrIdValid := Bool(false) }

  // When a valid CSR command is recieved
  when ( ioRxCsrValid && ( ioRxCsrAddr(13,6) === CSR_ID ) ) {
    switch ( ioRxCsrAddr(5, 0) ) {
      is ( AFU_CSR_DSR_BASEH ) {
        afuDsrBase(31, 26) := ioRxCsrData(5, 0)
      }
      is ( AFU_CSR_DSR_BASEL ) {
        afuDsrBase(25, 0) := ioRxCsrData(31, 6)
        csrIdValid := Bool(true)
        csrIdAddr := ( afuDsrBase(31,26) ## ioRxCsrData(31, 6) )
      }
      is ( AFU_CSR_CTX_BASEH ) {
        csrCtxBase(57, 26) := ioRxCsrData
      }
      is ( AFU_CSR_CTX_BASEL ) {
        csrCtxBase(25,0) := ioRxCsrData(31,6)
        csrCtxBaseValid := Bool(true)
      }
    }
  }

  // ---------------------------------------------------------------------
  // IO Control Logic
  // ---------------------------------------------------------------------

  // AFU is requesting to write to memory
  when ( corTxWrValid ) {
    // default Tx Write header
    afuTxWrHdr := { corTxWrLen ## corTxWrAddr(57,32) ## UInt(1024, 11) ## CCI_REQ_WR ## UInt(0, 6) ## corTxWrAddr(31,0) ## UInt(3, 8) ## txWrTag }
    // when the block is ready and there are more than 1 remaining
    when ( txWrBlock && txWrBlockCnt > UInt(1, 6) ) {
      txWrBlockCnt := txWrBlockCnt - UInt(1, 6)
    }
    // for the last block, set the tag to the next set
    when ( txWrBlock && !(txWrBlockCnt > UInt(1, 6)) ){
      txWrBlock := Bool(false)
      txWrTag := txWrTag + UInt(1, 6)
    }
    // read from the core module if available
    when ( !txWrBlock && corTxWrLen > UInt(1, 6) ) {
      txWrBlock := Bool(true)
      txWrBlockCnt := corTxWrLen - UInt(1, 6)
    }
    // increment tag for valid data passed through
    when ( !txWrBlock && !(corTxWrLen > UInt(1, 6)) ) {
      txWrTag := txWrTag + UInt(1, 6)
    }
    // If the data is for the device status register, change the header to point to it
    when ( corTxDsrValid && !corTxFenceValid && !corTxDoneValid ) {
      afuTxWrHdr := { UInt(0, 43) ## CCI_REQ_WR ## UInt(0, 6) ## corTxWrAddr(31,0) ## UInt(3, 8) ## txWrTag }
      txWrTag := txWrTag + UInt(1, 6)
    }
    // if a write fence command
    when ( !corTxDsrValid && corTxFenceValid && !corTxDoneValid ) {
      afuTxWrHdr := { UInt(0, 43) ## CCI_REQ_WR_FENCE ## UInt( 0, 52 ) }
    }
    // send done
    when ( !corTxDsrValid && !corTxFenceValid && corTxDoneValid ) {
      afuTxWrHdr := { corTxWrLen ## corTxWrAddr(57,32) ## UInt(1024, 11) ## CCI_REQ_WR ## UInt(0,6) ## corTxWrAddr(31,0) ## UInt(3, 8) ## txWrTag }
      txWrTag := txWrTag + UInt(1, 6)
    }
  }

  // Core is requesting to read from memory
  afuTxRdHdr := { corTxRdLen ## corTxRdAddr(57,32) ## UInt(0, 11) ## CCI_REQ_RD ## UInt(0, 6) ## corTxRdAddr(31,0) ## UInt(2, 8) ## txRdTag }
  when ( corTxRdValid ) {
    txRdTag := txRdTag + UInt(1, 6)
  }

  // Rx for the CSR
  ioRxCsrValid := io.splRxCfgValid
  ioRxCsrAddr := io.splRxHdr0(13, 0)
  ioRxCsrData := io.splRxData(31, 0)

  // Rx for core
  ioRxRdValid := io.splRxRdValid
  ioRxData := io.splRxData

  // ---------------------------------------------------------------------
  // Core Management Logic
  // ---------------------------------------------------------------------


  // TODO: create interface ...

}
