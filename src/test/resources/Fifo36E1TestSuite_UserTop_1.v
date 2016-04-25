module Fifo36E1TestSuite_UserTop_1(input deqClk, input enqClk, input reset,
    output io_enq_ready,
    input  io_enq_valid,
    input [71:0] io_enq_bits,
    input  io_deq_ready,
    output io_deq_valid,
    output[71:0] io_deq_bits,
    output[8:0] io_count
);

  wire[7:0] T0;
  wire[63:0] T1;
  wire[71:0] T2;
  wire T3;
  wire T4;
  wire[63:0] testFifo_io_dout;
  wire[7:0] testFifo_io_dop;
  wire testFifo_io_full;
  wire testFifo_io_empty;


`ifndef SYNTHESIS
// synthesis translate_off
//  assign io_count = {1{$random}};
// synthesis translate_on
`endif
  assign T0 = io_enq_bits[7:0];
  assign T1 = io_enq_bits[71:8];
  assign io_deq_bits = T2;
  assign T2 = {testFifo_io_dout, testFifo_io_dop};
  assign io_deq_valid = T3;
  assign T3 = testFifo_io_empty ^ 1'h1;
  assign io_enq_ready = T4;
  assign T4 = testFifo_io_full ^ 1'h1;
  Fifo36E1 # (
    .ALMOST_FULL_OFFSET(500),
    .ALMOST_EMPTY_OFFSET(100),
    .FIRST_WORD_FALL_THROUGH(TRUE),
    .DO_REG(1),
    .DATA_WIDTH(72),
    .FIFO_MODE("FIFO36"),
    .EN_SYN(FALSE),
    .SRVAL(0),
    .INIT(0)
  ) testFifo(.deqClk(deqClk), .enqClk(enqClk), .RST(reset),
       .io_din( T1 ),
       .io_dip( T0 ),
       .io_wren( io_enq_valid ),
       .io_rden( io_deq_ready ),
       .io_dout( testFifo_io_dout ),
       .io_dop( testFifo_io_dop ),
       .io_full( testFifo_io_full ),
       //.io_almostFull(  )
       .io_empty( testFifo_io_empty )
       //.io_almostEmpty(  )
       //.io_rdCount(  )
       //.io_wrCount(  )
       //.io_wrErr(  )
       //.io_rdErr(  )
  );
endmodule

