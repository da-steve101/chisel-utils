package chiselutils.interfaces.exanicx4

import Chisel._

/** ExanicX4 IO
  */
class ExanicX4IO extends Bundle {
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

class ExanicX4Interface extends Module {
  val io = new ExanicX4IO
  setModuleName("user_application")
  reset.setName("rst")
  io.setNames

  // Default connections for functioning as a network card
  io.tx0Usr <> io.tx0Host
  io.rx0Usr.rxComm <> io.rx0Host.rxComm
  io.rx0Host.matchHost := UInt( 0, 8 )
  io.rx0Host.bufferHost := UInt( 0, 6 )

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

}
