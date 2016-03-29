package chiselutils.interfaces.exanicx4

import Chisel._

/** This module strips off the checksum from a frame
  */
class StripCrc extends Module {

  val io = new Bundle {
    val in = HandshakeIO().flip
    val out = HandshakeIO()
  }

  val initVal = io.in.cloneType
  initVal.len := UInt(0, 3)
  initVal.vld := Bool(false)
  initVal.eof := Bool(false)
  initVal.sof := Bool(false)
  initVal.data := UInt(0, 64)
  val frame = RegInit( initVal )

  when ( io.in.vld || io.in.eof || frame.eof ) {
    frame := io.in
  }

  io.out.len := UInt(0, 3)
  io.out.vld := Bool(false)
  io.out.eof := Bool(false)
  io.out.sof := frame.sof
  io.out.data := frame.data

  // len without crc
  when ( io.in.eof && io.in.len < UInt(5, 3) && io.in.len =/= UInt(0, 3) ) {
    io.out.len := io.in.len - UInt(4, 3)
  }
  when ( frame.eof && ( frame.len > UInt(4, 3) || frame.len === UInt(0, 3) ) ) {
    io.out.len := frame.len - UInt(4, 3)
  }

  // vld without crc
  io.out.vld := {
    ( frame.vld && io.in.vld ) ||
    ( frame.vld && io.in.eof ) ||
    ( frame.eof && frame.vld && ( frame.len === UInt(0, 3) || frame.len > UInt(4, 3) ) )
  }

  // eof without crc
  io.out.eof := {
    ( frame.vld && ( !io.in.vld && io.in.eof ) ) ||
    ( frame.eof && frame.vld && ( frame.len === UInt(0, 3) || frame.len > UInt(4, 3) ) ) ||
    ( io.in.eof && io.in.vld && ( io.in.len < UInt(5, 3) ) && io.in.len =/= UInt(0, 3) )
  }
}
