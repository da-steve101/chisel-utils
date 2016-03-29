package chiselutils.interfaces.exanicx4

import Chisel._

/** Factory method for HandshakeIO
  */
object HandshakeIO {
  def apply() : HandshakeIO = {
    new HandshakeIO
  }
}

/** A bundle to represent the data handshaking interface to enable easy connection
  * Between rx and tx
  */
class HandshakeIO extends Bundle {
  val data = UInt( OUTPUT, 64 )
  val sof = Bool( OUTPUT )
  val eof = Bool( OUTPUT )
  val len = UInt( OUTPUT, 3 )
  val vld = Bool( OUTPUT )

  def setNames( prefix : String, suffix : String ) {
    data.setName( prefix + "_data_" + suffix )
    sof.setName( prefix + "_sof_" + suffix )
    eof.setName( prefix + "_eof_" + suffix)
    len.setName( prefix + "_len_" + suffix )
    vld.setName( prefix + "_vld_" + suffix )
  }
}
