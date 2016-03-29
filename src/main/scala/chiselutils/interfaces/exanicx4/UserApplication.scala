package chiselutils.interfaces.exanicx4

import Chisel._
import chiselutils.utils.Serializer

/** User application IO
  */
class UserApplicationIO extends Bundle {
  /** The general Tx IO port
    */
  class TxPortIO extends Bundle {
    val hndshk = HandshakeIO()
    val ack = Bool( INPUT )

    def setNames( prefix : String, suffix : String ) {
      hndshk.setNames( prefix, suffix )
      ack.setName( prefix + "_ack_" + suffix )
    }
  }

  /** The general rx IO port
    */
  class RxPortIO extends Bundle {
    val hndshk = HandshakeIO().flip
    val err = Bool( INPUT )
    val pktDrop = Bool( INPUT )
    val crcFail = Bool( INPUT )
    val timeStamp = UInt( INPUT, 32 )

    def setNames( prefix : String, suffix : String ) {
      hndshk.setNames( prefix, suffix )
      err.setName( prefix + "_err_" + suffix )
      pktDrop.setName( prefix + "_pkt_drop_" + suffix )
      crcFail.setName( prefix + "_crc_fail_" + suffix )
      timeStamp.setName( prefix + "_timestamp_" + suffix )
    }
  }

  /** Rx port for usr as host is not symmetrical
    */
  class RxUsrPortIO extends Bundle {
    val rxComm = new RxPortIO
    val isVlanUsr = Bool( INPUT )
    val vlanUsr = UInt( INPUT, 12 )

    def setNames( prefix : String ) {
      rxComm.setNames( prefix, "usr" )
      isVlanUsr.setName( prefix + "_is_vlan_usr" )
      vlanUsr.setName( prefix + "_vlan_usr" )
    }
  }

  /** Rx port for host as usr is not symmetrical
    */
  class RxHostPortIO extends Bundle {
    val rxComm = (new RxPortIO).flip
    val matchHost = UInt( OUTPUT, 8 )
    val bufferHost = UInt( OUTPUT, 6 )

    def setNames( prefix : String ) {
      rxComm.setNames( prefix, "host" )
      matchHost.setName( prefix + "_match_host" )
      bufferHost.setName( prefix + "_buffer_host" )
    }
  }

  val devkitVersion = UInt( INPUT, 32 )
  val hwTime = UInt( INPUT, 32 )
  val hwRev = UInt( INPUT, 3 )

  /* User register interface at BAR0. */
  val regWEn = Bool( INPUT )
  val regWAddr = UInt( INPUT, 11 )
  val regWData = UInt( INPUT, 32 )
  val regRAddr = UInt( INPUT, 11 )
  val regRData = UInt( OUTPUT, 32 )

  /* User memory space at BAR2. */
  val memWEn = UInt( INPUT, 16 )
  val memWAddr = UInt( INPUT, 19 )
  val memWData = UInt( INPUT, 128 )

  /* Port 0 interface. */
  val trx0LinkUp = Bool( INPUT )

  val tx0Usr = new TxPortIO
  val tx0Host = (new TxPortIO).flip

  val rx0Usr = new RxUsrPortIO
  val rx0Host = new RxHostPortIO

  /* Port 1 interface. */
  val trx1LinkUp = Bool( INPUT )

  val tx1Usr = new TxPortIO
  val tx1Host = (new TxPortIO).flip

  val rx1Usr = new RxUsrPortIO
  val rx1Host = new RxHostPortIO

  /* Port 2 interface. */
  val trx2LinkUp = Bool( INPUT )

  val tx2Usr = new TxPortIO
  val tx2Host = (new TxPortIO).flip

  val rx2Usr = new RxUsrPortIO
  val rx2Host = new RxHostPortIO

  /* Port 3 interface. */
  val trx3LinkUp = Bool( INPUT )

  val tx3Usr = new TxPortIO
  val tx3Host = (new TxPortIO).flip

  val rx3Usr = new RxUsrPortIO
  val rx3Host = new RxHostPortIO

  def setNames {
    devkitVersion.setName("devkit_version")
    hwTime.setName("hw_time")
    hwRev.setName("hw_rev")

    regWEn.setName("reg_w_en")
    regWAddr.setName("reg_w_addr")
    regWData.setName("reg_w_data")
    regRAddr.setName("reg_r_addr")
    regRData.setName("reg_r_data")

    memWEn.setName("mem_w_en")
    memWAddr.setName("mem_w_addr")
    memWData.setName("mem_w_data")

    tx0Usr.setNames( "tx0", "usr" )
    tx0Host.setNames( "tx0" , "host" )
    rx0Usr.setNames( "rx0" )
    rx0Host.setNames( "rx0" )

    tx1Usr.setNames( "tx1", "usr" )
    tx1Host.setNames( "tx1" , "host" )
    rx1Usr.setNames( "rx1" )
    rx1Host.setNames( "rx1" )

    tx2Usr.setNames( "tx2", "usr" )
    tx2Host.setNames( "tx2" , "host" )
    rx2Usr.setNames( "rx2" )
    rx2Host.setNames( "rx2" )

    tx3Usr.setNames( "tx3", "usr" )
    tx3Host.setNames( "tx3" , "host" )
    rx3Usr.setNames( "rx3" )
    rx3Host.setNames( "rx3" )

    trx0LinkUp.setName("trx0_link_up")
    trx1LinkUp.setName("trx1_link_up")
    trx2LinkUp.setName("trx2_link_up")
    trx3LinkUp.setName("trx3_link_up")
  }
}

class UserApplication[ T <: Bits ]( genType : T, fifoDepth : Int, memSize : Int,
  getUserMod : () => TimeSeriesInterface[T], bytesOut : Int ) extends Module {
  val io = new UserApplicationIO
  setModuleName("user_application")
  reset.setName("rst")
  io.setNames

  // Default connections for functioning as a network card
  io.tx0Usr <> io.tx0Host
  io.rx0Usr.rxComm <> io.rx0Host.rxComm
  io.rx0Host.matchHost := UInt( 0, 8 )
  io.rx0Host.bufferHost := UInt( 0, 6 )

  // In this example, tx1 and rx1 are used by the TimeSeriesInterface
  // These connections are overridden below
  io.tx1Usr <> io.tx1Host
  io.rx1Usr.rxComm <> io.rx1Host.rxComm
  io.rx1Host.matchHost := UInt( 0, 8 )
  io.rx1Host.bufferHost := UInt( 0, 6 )

  io.tx2Usr <> io.tx2Host
  io.rx2Usr.rxComm <> io.rx2Host.rxComm
  io.rx2Host.matchHost := UInt( 0, 8 )
  io.rx2Host.bufferHost := UInt( 0, 6 )

  io.tx3Usr <> io.tx3Host
  io.rx3Usr.rxComm <> io.rx3Host.rxComm
  io.rx3Host.matchHost := UInt( 0, 8 )
  io.rx3Host.bufferHost := UInt( 0, 6 )

  // create the user module
  val userMod = Module( getUserMod() )
  val noReg = userMod.getNoReg()

  // connect memory interface, read only
  val userMem = Mem( memSize, Vec.fill(16) { UInt( width = 8 ) } )
  val memWData = Vec.fill( 16 ) { UInt( width = 8 ) }
  (0 until 16).foreach( idx => { memWData(idx) := io.memWData(8*(idx + 1) - 1, 8*idx) } )
  userMem.write( io.memWAddr, memWData, io.memWEn )

  // The register interface
  val regIntR = RegInit( Vec.fill( noReg + 1 ) { UInt( width = 32 ) } )
  val regIntW = RegInit( Vec.fill( noReg + 1 ) { UInt( width = 32 ) } )
  val controlReg = regIntR(noReg) // bit0 => reset error, bit1 => userMod reset, bit2 => drain
  val errRst = controlReg(0)
  userMod.reset := controlReg(1) || reset
  val fifoDrain = controlReg(2)
  val statusVal = regIntW(noReg)
  statusVal := UInt( 0, 32 )
  when ( io.regWEn ) {
    regIntR( io.regWAddr ) := io.regWData
  }
  io.regRData := regIntW( io.regRAddr )

  // error checking
  val rx1Err = RegInit( Bool(false) )
  val pktDrop = RegInit( Bool(false) )
  val crcFail = RegInit( Bool(false) )
  val fifoFull = RegInit( Bool(false) )
  val txFifoFull = RegInit( Bool(false) )
  val dirOutFull = RegInit( Bool(false) )
  val userErr = RegInit( Bool(false) )
  val error = UInt( width = 7 )
  error := rx1Err ## pktDrop ## crcFail ## fifoFull ## txFifoFull ## dirOutFull ## userErr

  // Strip the CRC off data and pass into combiner
  val stripper = Module(new StripCrc)
  stripper.io.in <> io.rx1Usr.rxComm.hndshk

  // combine data into continuous stream
  val combiner = Module(new DataCombiner)
  for ( idx <- 0 until 8 ) {
    combiner.io.dataIn(idx) := stripper.io.out.data( 8*(idx + 1) - 1, 8*idx )
  }
  combiner.io.vld := stripper.io.out.vld
  combiner.io.len := stripper.io.out.len

  // data fifo broken into bytes
  val fifo = Module(new Queue( Vec.fill( 8 ) { UInt( width = 8 ) } , fifoDepth ) )
  fifo.io.enq.bits := combiner.io.dataOut
  fifo.io.enq.valid := combiner.io.vldOut

  // pass to time series interface
  userMod.io.dataIn <> fifo.io.deq
  val memData = userMem.read( userMod.io.memAddr )
  userMod.io.memData := UInt(0, 128)
  (0 until 16).foreach( idx => { userMod.io.memData(8*(idx + 1) - 1, 8*idx) := memData(idx) } )

  (0 until noReg).foreach( idx => { userMod.io.regIn(idx) := regIntR(idx) } )
  when ( userMod.io.regOutEn ) {
    (0 until noReg).foreach( idx => { regIntW(idx) := userMod.io.regOut(idx) } )
  }

  // combine time series output
  val noOutputs = math.ceil( bytesOut*8.0/userMod.bwOfGenType ).toInt - 1
  val vecDataOut = Vec.fill( userMod.bwOfGenType ) { UInt( width = 1 ) }
  ( 0 until userMod.bwOfGenType ).foreach( x => { vecDataOut(x) := userMod.io.dataOut.bits(x) } )

  // change width vecDataOut -> buffer
  val directOutputFifo = Module( new Queue( Vec.fill( userMod.bwOfGenType ) { UInt( width = 1 ) }, fifoDepth ) )
  directOutputFifo.io.enq.bits := vecDataOut
  directOutputFifo.io.enq.valid := userMod.io.dataOut.valid
  userMod.io.dataOut.ready := directOutputFifo.io.enq.ready

  val outToBuffer = Module( new Serializer( UInt( width = 1 ), userMod.bwOfGenType, 64 ) )
  outToBuffer.io.dataIn <> directOutputFifo.io.deq

  val buffCount = RegInit( UInt( 0, log2Up( noOutputs + 1 ) ) )
  val flush = ( ( buffCount >= UInt( noOutputs, log2Up( noOutputs + 1 ) ) )
    && outToBuffer.io.dataIn.valid ) || fifoDrain
  when( outToBuffer.io.dataIn.ready && outToBuffer.io.dataIn.valid ) {
    buffCount := buffCount + UInt( 1, log2Up( noOutputs + 1 ) )
  }
  when( flush ) {
    buffCount := UInt( 0, log2Up( noOutputs + 1 ) )
  }
  outToBuffer.io.flush := flush
  val buffer = RegNext( outToBuffer.io.dataOut.bits )
  val bufferVld = RegNext( outToBuffer.io.dataOut.valid )

  // Join bits together into bytes
  val bufferByte = Vec.fill( 8 ) { UInt( width = 8 ) }
  ( 0 until 8).foreach( idx => bufferByte(idx) := UInt(0, 8) )
  ( 0 until 64).foreach( idx => { bufferByte( idx/8 )( idx % 8 ) := buffer( idx ) })

  // output fifo
  val fifoTxOut = Module(new Queue( Vec.fill( 8 ) { UInt( width = 8 ) } , fifoDepth ) )
  fifoTxOut.io.enq.bits := bufferByte
  fifoTxOut.io.enq.valid := bufferVld

  // reset errors after put in fifo
  rx1Err := rx1Err || io.rx1Usr.rxComm.err
  pktDrop := pktDrop || io.rx1Usr.rxComm.pktDrop
  crcFail := crcFail || io.rx1Usr.rxComm.crcFail
  fifoFull := fifoFull || !fifo.io.enq.ready
  dirOutFull := dirOutFull || !directOutputFifo.io.enq.ready
  userErr := userErr || userMod.io.error
  txFifoFull := txFifoFull || !fifoTxOut.io.enq.ready
  when ( errRst ) {
    rx1Err := Bool(false)
    pktDrop := Bool(false)
    crcFail := Bool(false)
    fifoFull := Bool(false)
    dirOutFull := Bool(false)
    txFifoFull := Bool(false)
    userErr := Bool(false)
  }

  val noSegment = math.floor( (bytesOut - 1)/8.0 ).toInt
  val segmentCounter = RegInit( UInt( 0, log2Up(noSegment + 1) ) )
  val tx1Output = UInt( width = 64 )
  val sof = ( segmentCounter === UInt( 0, log2Up(noSegment + 1) ) )
  val eof = ( segmentCounter >= UInt( noSegment, log2Up(noSegment + 1) ) )
  val len = UInt( width = 3 )
  len := UInt( 0, 3 )
  tx1Output := UInt(0, 64)
  for ( x <- 0 until 8 ) {
    tx1Output(8*(x + 1) - 1, 8*x) := fifoTxOut.io.deq.bits(x)
  }
  when ( eof ) {
    len := UInt( bytesOut % 8, 3 )
  }
  when ( fifoTxOut.io.deq.valid && fifoTxOut.io.deq.ready ) {
    segmentCounter := segmentCounter + UInt( 1, log2Up(noSegment + 1) )
    when( eof ) {
      segmentCounter := UInt( 0, log2Up(noSegment + 1) )
    }
  }
  when ( fifoDrain ) {
    segmentCounter := UInt( 0, log2Up(noSegment + 1) )
  }

  statusVal := directOutputFifo.io.count ## fifoTxOut.io.count ## segmentCounter ## error

  val sending = RegInit( Bool(false) )
  sending := sending || ( fifoTxOut.io.count >= UInt( noSegment, log2Up(fifoDepth) ) )
  when ( ( eof && io.tx1Usr.hndshk.vld && io.tx1Usr.ack ) || fifoDrain ) {
    sending := Bool(false)
  }

  io.tx1Usr.hndshk.data := tx1Output
  io.tx1Usr.hndshk.vld := fifoTxOut.io.deq.valid && sending
  io.tx1Usr.hndshk.sof := sof && io.tx1Usr.hndshk.vld
  io.tx1Usr.hndshk.eof := eof && io.tx1Usr.hndshk.vld
  io.tx1Usr.hndshk.len := len
  fifoTxOut.io.deq.ready := ( io.tx1Usr.ack && sending ) || fifoDrain
}
