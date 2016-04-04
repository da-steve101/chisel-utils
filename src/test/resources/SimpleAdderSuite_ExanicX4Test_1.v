module Queue_2(input clk, input reset,
    output io_enq_ready,
    input  io_enq_valid,
    input [7:0] io_enq_bits,
    input  io_deq_ready,
    output io_deq_valid,
    output[7:0] io_deq_bits,
    output[5:0] io_count
);

  wire[5:0] T0;
  wire[4:0] ptr_diff;
  reg [4:0] R1;
  wire[4:0] T15;
  wire[4:0] T2;
  wire[4:0] T3;
  wire do_deq;
  reg [4:0] R4;
  wire[4:0] T16;
  wire[4:0] T5;
  wire[4:0] T6;
  wire do_enq;
  wire T7;
  wire ptr_match;
  reg  maybe_full;
  wire T17;
  wire T8;
  wire T9;
  wire[7:0] T10;
  reg [7:0] ram [31:0];
  wire[7:0] T11;
  wire T12;
  wire empty;
  wire T13;
  wire T14;
  wire full;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    R1 = {1{$random}};
    R4 = {1{$random}};
    maybe_full = {1{$random}};
    for (initvar = 0; initvar < 32; initvar = initvar+1)
      ram[initvar] = {1{$random}};
  end
// synthesis translate_on
`endif

  assign io_count = T0;
  assign T0 = {T7, ptr_diff};
  assign ptr_diff = R4 - R1;
  assign T15 = reset ? 5'h0 : T2;
  assign T2 = do_deq ? T3 : R1;
  assign T3 = R1 + 5'h1;
  assign do_deq = io_deq_ready & io_deq_valid;
  assign T16 = reset ? 5'h0 : T5;
  assign T5 = do_enq ? T6 : R4;
  assign T6 = R4 + 5'h1;
  assign do_enq = io_enq_ready & io_enq_valid;
  assign T7 = maybe_full & ptr_match;
  assign ptr_match = R4 == R1;
  assign T17 = reset ? 1'h0 : T8;
  assign T8 = T9 ? do_enq : maybe_full;
  assign T9 = do_enq != do_deq;
  assign io_deq_bits = T10;
  assign T10 = ram[R1];
  assign io_deq_valid = T12;
  assign T12 = empty ^ 1'h1;
  assign empty = ptr_match & T13;
  assign T13 = maybe_full ^ 1'h1;
  assign io_enq_ready = T14;
  assign T14 = full ^ 1'h1;
  assign full = ptr_match & maybe_full;

  always @(posedge clk) begin
    if(reset) begin
      R1 <= 5'h0;
    end else if(do_deq) begin
      R1 <= T3;
    end
    if(reset) begin
      R4 <= 5'h0;
    end else if(do_enq) begin
      R4 <= T6;
    end
    if(reset) begin
      maybe_full <= 1'h0;
    end else if(T9) begin
      maybe_full <= do_enq;
    end
    if (do_enq)
      ram[R4] <= io_enq_bits;
  end
endmodule

module SimpleAdderSuite_SimpleAdder_1(input clk, input reset,
    output io_dataIn_ready,
    input  io_dataIn_valid,
    input [7:0] io_dataIn_bits_7,
    input [7:0] io_dataIn_bits_6,
    input [7:0] io_dataIn_bits_5,
    input [7:0] io_dataIn_bits_4,
    input [7:0] io_dataIn_bits_3,
    input [7:0] io_dataIn_bits_2,
    input [7:0] io_dataIn_bits_1,
    input [7:0] io_dataIn_bits_0,
    input [31:0] io_regIn_14,
    input [31:0] io_regIn_13,
    input [31:0] io_regIn_12,
    input [31:0] io_regIn_11,
    input [31:0] io_regIn_10,
    input [31:0] io_regIn_9,
    input [31:0] io_regIn_8,
    input [31:0] io_regIn_7,
    input [31:0] io_regIn_6,
    input [31:0] io_regIn_5,
    input [31:0] io_regIn_4,
    input [31:0] io_regIn_3,
    input [31:0] io_regIn_2,
    input [31:0] io_regIn_1,
    input [31:0] io_regIn_0,
    output[31:0] io_regOut_14,
    output[31:0] io_regOut_13,
    output[31:0] io_regOut_12,
    output[31:0] io_regOut_11,
    output[31:0] io_regOut_10,
    output[31:0] io_regOut_9,
    output[31:0] io_regOut_8,
    output[31:0] io_regOut_7,
    output[31:0] io_regOut_6,
    output[31:0] io_regOut_5,
    output[31:0] io_regOut_4,
    output[31:0] io_regOut_3,
    output[31:0] io_regOut_2,
    output[31:0] io_regOut_1,
    output[31:0] io_regOut_0,
    output io_regOutEn,
    output[18:0] io_memAddr,
    input [127:0] io_memData,
    output io_error,
    input  io_dataOut_ready,
    output io_dataOut_valid,
    output[7:0] io_dataOut_bits
);

  reg [7:0] R0;
  wire[7:0] T1_1;
  reg [7:0] addLayer2_1;
  wire[7:0] T2_1;
  reg [7:0] addLayer1_3;
  wire[7:0] T3_1;
  reg [7:0] addLayer1_2;
  wire[7:0] T4_1;
  reg [7:0] addLayer2_0;
  wire[7:0] T5_1;
  reg [7:0] addLayer1_1;
  wire[7:0] T6_1;
  reg [7:0] addLayer1_0;
  wire[7:0] T7;
  reg  R8;
  reg  R9;
  reg  R10;
  wire T11;
  wire T12;
  wire T13;
  wire[31:0] T14;
  wire[31:0] T15;
  wire[31:0] T16;
  wire[31:0] T17;
  wire[31:0] T18;
  wire[31:0] T19;
  wire[31:0] T20;
  wire[31:0] T21;
  wire[31:0] T22;
  wire fifo_io_enq_ready;
  wire fifo_io_deq_valid;
  wire[7:0] fifo_io_deq_bits;
  wire[5:0] fifo_io_count;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    R0 = {1{$random}};
    addLayer2_1 = {1{$random}};
    addLayer1_3 = {1{$random}};
    addLayer1_2 = {1{$random}};
    addLayer2_0 = {1{$random}};
    addLayer1_1 = {1{$random}};
    addLayer1_0 = {1{$random}};
    R8 = {1{$random}};
    R9 = {1{$random}};
    R10 = {1{$random}};
  end
// synthesis translate_on
`endif

  assign T1_1 = addLayer2_0 + addLayer2_1;
  assign T2_1 = addLayer1_2 + addLayer1_3;
  assign T3_1 = io_dataIn_bits_6 + io_dataIn_bits_7;
  assign T4_1 = io_dataIn_bits_4 + io_dataIn_bits_5;
  assign T5_1 = addLayer1_0 + addLayer1_1;
  assign T6_1 = io_dataIn_bits_2 + io_dataIn_bits_3;
  assign T7 = io_dataIn_bits_0 + io_dataIn_bits_1;
  assign io_dataOut_bits = fifo_io_deq_bits;
  assign io_dataOut_valid = fifo_io_deq_valid;
  assign io_error = T11;
  assign T11 = T13 | T12;
  assign T12 = io_dataOut_ready ^ 1'h1;
  assign T13 = fifo_io_enq_ready ^ 1'h1;
  assign io_memAddr = 19'h0;
  assign io_regOutEn = fifo_io_deq_valid;
  assign io_regOut_0 = T14;
  assign T14 = {26'h0, fifo_io_count};
  assign io_regOut_1 = T15;
  assign T15 = {24'h0, io_dataIn_bits_0};
  assign io_regOut_2 = T16;
  assign T16 = {24'h0, io_dataIn_bits_1};
  assign io_regOut_3 = T17;
  assign T17 = {24'h0, io_dataIn_bits_2};
  assign io_regOut_4 = T18;
  assign T18 = {24'h0, io_dataIn_bits_3};
  assign io_regOut_5 = T19;
  assign T19 = {24'h0, io_dataIn_bits_4};
  assign io_regOut_6 = T20;
  assign T20 = {24'h0, io_dataIn_bits_5};
  assign io_regOut_7 = T21;
  assign T21 = {24'h0, io_dataIn_bits_6};
  assign io_regOut_8 = T22;
  assign T22 = {24'h0, io_dataIn_bits_7};
  assign io_regOut_9 = 32'h0;
  assign io_regOut_10 = 32'h0;
  assign io_regOut_11 = 32'h0;
  assign io_regOut_12 = 32'h0;
  assign io_regOut_13 = 32'h0;
  assign io_regOut_14 = 32'h0;
  assign io_dataIn_ready = 1'h1;
  Queue_2 fifo(.clk(clk), .reset(reset),
       .io_enq_ready( fifo_io_enq_ready ),
       .io_enq_valid( R8 ),
       .io_enq_bits( R0 ),
       .io_deq_ready( io_dataOut_ready ),
       .io_deq_valid( fifo_io_deq_valid ),
       .io_deq_bits( fifo_io_deq_bits ),
       .io_count( fifo_io_count )
  );

  always @(posedge clk) begin
    R0 <= T1_1;
    addLayer2_1 <= T2_1;
    addLayer1_3 <= T3_1;
    addLayer1_2 <= T4_1;
    addLayer2_0 <= T5_1;
    addLayer1_1 <= T6_1;
    addLayer1_0 <= T7;
    R8 <= R9;
    R9 <= R10;
    R10 <= io_dataIn_valid;
  end
endmodule

module StripCrc(input clk, input reset,
    input [63:0] io_in_data,
    input  io_in_sof,
    input  io_in_eof,
    input [2:0] io_in_len,
    input  io_in_vld,
    output[63:0] io_out_data,
    output io_out_sof,
    output io_out_eof,
    output[2:0] io_out_len,
    output io_out_vld
);

  wire T0;
  wire T1;
  wire T2;
  wire T3;
  reg [2:0] frame_len;
  wire[2:0] initVal_len;
  wire[2:0] T43;
  wire[2:0] T4;
  wire T5;
  reg  frame_eof;
  wire initVal_eof;
  wire T44;
  wire T6;
  wire T7;
  wire T8;
  wire T9;
  reg  frame_vld;
  wire initVal_vld;
  wire T45;
  wire T10;
  wire T11;
  wire T12;
  wire T13;
  wire[2:0] T14;
  wire[2:0] T15;
  wire[2:0] T16;
  wire T17;
  wire T18;
  wire T19;
  wire T20;
  wire[2:0] T21;
  wire T22;
  wire T23;
  wire T24;
  wire T25;
  wire T26;
  wire T27;
  wire T28;
  wire T29;
  wire T30;
  wire T31;
  wire T32;
  wire T33;
  wire T34;
  wire T35;
  wire T36;
  wire T37;
  wire T38;
  wire T39;
  wire T40;
  reg  frame_sof;
  wire initVal_sof;
  wire T46;
  wire T41;
  reg [63:0] frame_data;
  wire[63:0] initVal_data;
  wire[63:0] T47;
  wire[63:0] T42;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    frame_len = {1{$random}};
    frame_eof = {1{$random}};
    frame_vld = {1{$random}};
    frame_sof = {1{$random}};
    frame_data = {2{$random}};
  end
// synthesis translate_on
`endif

  assign io_out_vld = T0;
  assign T0 = T11 | T1;
  assign T1 = T9 & T2;
  assign T2 = T8 | T3;
  assign T3 = 3'h4 < frame_len;
  assign initVal_len = 3'h0;
  assign T43 = reset ? initVal_len : T4;
  assign T4 = T5 ? io_in_len : frame_len;
  assign T5 = T7 | frame_eof;
  assign initVal_eof = 1'h0;
  assign T44 = reset ? initVal_eof : T6;
  assign T6 = T5 ? io_in_eof : frame_eof;
  assign T7 = io_in_vld | io_in_eof;
  assign T8 = frame_len == 3'h0;
  assign T9 = frame_eof & frame_vld;
  assign initVal_vld = 1'h0;
  assign T45 = reset ? initVal_vld : T10;
  assign T10 = T5 ? io_in_vld : frame_vld;
  assign T11 = T13 | T12;
  assign T12 = frame_vld & io_in_eof;
  assign T13 = frame_vld & io_in_vld;
  assign io_out_len = T14;
  assign T14 = T22 ? T21 : T15;
  assign T15 = T17 ? T16 : 3'h0;
  assign T16 = io_in_len - 3'h4;
  assign T17 = T19 & T18;
  assign T18 = io_in_len != 3'h0;
  assign T19 = io_in_eof & T20;
  assign T20 = io_in_len < 3'h5;
  assign T21 = frame_len - 3'h4;
  assign T22 = frame_eof & T23;
  assign T23 = T25 | T24;
  assign T24 = frame_len == 3'h0;
  assign T25 = 3'h4 < frame_len;
  assign io_out_eof = T26;
  assign T26 = T32 | T27;
  assign T27 = T29 & T28;
  assign T28 = io_in_len != 3'h0;
  assign T29 = T31 & T30;
  assign T30 = io_in_len < 3'h5;
  assign T31 = io_in_eof & io_in_vld;
  assign T32 = T38 | T33;
  assign T33 = T37 & T34;
  assign T34 = T36 | T35;
  assign T35 = 3'h4 < frame_len;
  assign T36 = frame_len == 3'h0;
  assign T37 = frame_eof & frame_vld;
  assign T38 = frame_vld & T39;
  assign T39 = T40 & io_in_eof;
  assign T40 = io_in_vld ^ 1'h1;
  assign io_out_sof = frame_sof;
  assign initVal_sof = 1'h0;
  assign T46 = reset ? initVal_sof : T41;
  assign T41 = T5 ? io_in_sof : frame_sof;
  assign io_out_data = frame_data;
  assign initVal_data = 64'h0;
  assign T47 = reset ? initVal_data : T42;
  assign T42 = T5 ? io_in_data : frame_data;

  always @(posedge clk) begin
    if(reset) begin
      frame_len <= initVal_len;
    end else if(T5) begin
      frame_len <= io_in_len;
    end
    if(reset) begin
      frame_eof <= initVal_eof;
    end else if(T5) begin
      frame_eof <= io_in_eof;
    end
    if(reset) begin
      frame_vld <= initVal_vld;
    end else if(T5) begin
      frame_vld <= io_in_vld;
    end
    if(reset) begin
      frame_sof <= initVal_sof;
    end else if(T5) begin
      frame_sof <= io_in_sof;
    end
    if(reset) begin
      frame_data <= initVal_data;
    end else if(T5) begin
      frame_data <= io_in_data;
    end
  end
endmodule

module DataCombiner(input clk, input reset,
    input [7:0] io_dataIn_7,
    input [7:0] io_dataIn_6,
    input [7:0] io_dataIn_5,
    input [7:0] io_dataIn_4,
    input [7:0] io_dataIn_3,
    input [7:0] io_dataIn_2,
    input [7:0] io_dataIn_1,
    input [7:0] io_dataIn_0,
    input  io_vld,
    input [2:0] io_len,
    output[7:0] io_dataOut_7,
    output[7:0] io_dataOut_6,
    output[7:0] io_dataOut_5,
    output[7:0] io_dataOut_4,
    output[7:0] io_dataOut_3,
    output[7:0] io_dataOut_2,
    output[7:0] io_dataOut_1,
    output[7:0] io_dataOut_0,
    output io_vldOut
);

  wire T0;
  wire[2:0] T1;
  wire[63:0] T2;
  wire T3;
  wire[2:0] T4;
  reg [2:0] count;
  wire[2:0] T452;
  wire[3:0] T453;
  wire[3:0] T5;
  wire[3:0] T6;
  wire[3:0] T454;
  wire[3:0] total;
  wire[3:0] T7;
  wire[3:0] T8;
  wire[3:0] T9;
  wire[3:0] T455;
  wire[3:0] T10;
  wire[3:0] T456;
  wire[2:0] T11;
  wire T12;
  wire[79:0] T13;
  wire T14;
  wire[3:0] T15;
  wire[87:0] T16;
  wire T17;
  wire T18;
  wire T19;
  wire T20;
  wire[7:0] T21;
  reg [7:0] store_0;
  wire[7:0] T22;
  wire[7:0] T23;
  wire[7:0] T24;
  wire[7:0] T25;
  wire[7:0] T26;
  wire T27;
  wire[2:0] T28;
  wire[2:0] T29;
  wire[7:0] T30;
  wire T31;
  wire T32;
  wire[7:0] T33;
  wire[7:0] T34;
  wire T35;
  wire[7:0] T36;
  wire T37;
  wire T38;
  wire T39;
  wire T40;
  wire T41;
  wire T42;
  wire[2:0] T43;
  wire T44;
  wire[7:0] T45;
  wire[7:0] T46;
  wire[7:0] T47;
  wire T48;
  wire[2:0] T49;
  wire[2:0] T457;
  wire[3:0] T50;
  wire[3:0] T51;
  wire[3:0] T458;
  wire[7:0] T52;
  wire T53;
  wire T54;
  wire[7:0] T55;
  wire[7:0] T56;
  wire T57;
  wire[7:0] T58;
  wire T59;
  wire T60;
  wire T61;
  wire T62;
  wire[7:0] T63;
  wire[7:0] T64;
  wire[7:0] T65;
  wire T66;
  wire[2:0] T67;
  wire[2:0] T459;
  wire[3:0] T68;
  wire[3:0] T460;
  wire[7:0] T69;
  wire T70;
  wire T71;
  wire[7:0] T72;
  wire[7:0] T73;
  wire T74;
  wire[7:0] T75;
  wire T76;
  wire T77;
  wire T78;
  wire T79;
  wire[7:0] T80;
  reg [7:0] store_1;
  wire[7:0] T81;
  wire[7:0] T82;
  wire[7:0] T83;
  wire[7:0] T84;
  wire[7:0] T85;
  wire T86;
  wire[2:0] T87;
  wire[2:0] T88;
  wire[7:0] T89;
  wire T90;
  wire T91;
  wire[7:0] T92;
  wire[7:0] T93;
  wire T94;
  wire[7:0] T95;
  wire T96;
  wire T97;
  wire T98;
  wire T99;
  wire T100;
  wire T101;
  wire[2:0] T102;
  wire T103;
  wire[7:0] T104;
  wire[7:0] T105;
  wire[7:0] T106;
  wire T107;
  wire[2:0] T108;
  wire[2:0] T461;
  wire[3:0] T109;
  wire[3:0] T110;
  wire[3:0] T462;
  wire[7:0] T111;
  wire T112;
  wire T113;
  wire[7:0] T114;
  wire[7:0] T115;
  wire T116;
  wire[7:0] T117;
  wire T118;
  wire T119;
  wire T120;
  wire T121;
  wire[7:0] T122;
  wire[7:0] T123;
  wire[7:0] T124;
  wire T125;
  wire[2:0] T126;
  wire[2:0] T463;
  wire[3:0] T127;
  wire[3:0] T464;
  wire[7:0] T128;
  wire T129;
  wire T130;
  wire[7:0] T131;
  wire[7:0] T132;
  wire T133;
  wire[7:0] T134;
  wire T135;
  wire T136;
  wire T137;
  wire T138;
  wire[7:0] T139;
  reg [7:0] store_2;
  wire[7:0] T140;
  wire[7:0] T141;
  wire[7:0] T142;
  wire[7:0] T143;
  wire[7:0] T144;
  wire T145;
  wire[2:0] T146;
  wire[2:0] T147;
  wire[7:0] T148;
  wire T149;
  wire T150;
  wire[7:0] T151;
  wire[7:0] T152;
  wire T153;
  wire[7:0] T154;
  wire T155;
  wire T156;
  wire T157;
  wire T158;
  wire T159;
  wire T160;
  wire[2:0] T161;
  wire T162;
  wire[7:0] T163;
  wire[7:0] T164;
  wire[7:0] T165;
  wire T166;
  wire[2:0] T167;
  wire[2:0] T465;
  wire[3:0] T168;
  wire[3:0] T169;
  wire[3:0] T466;
  wire[7:0] T170;
  wire T171;
  wire T172;
  wire[7:0] T173;
  wire[7:0] T174;
  wire T175;
  wire[7:0] T176;
  wire T177;
  wire T178;
  wire T179;
  wire T180;
  wire[7:0] T181;
  wire[7:0] T182;
  wire[7:0] T183;
  wire T184;
  wire[2:0] T185;
  wire[2:0] T467;
  wire[3:0] T186;
  wire[3:0] T468;
  wire[7:0] T187;
  wire T188;
  wire T189;
  wire[7:0] T190;
  wire[7:0] T191;
  wire T192;
  wire[7:0] T193;
  wire T194;
  wire T195;
  wire T196;
  wire T197;
  wire[7:0] T198;
  reg [7:0] store_3;
  wire[7:0] T199;
  wire[7:0] T200;
  wire[7:0] T201;
  wire[7:0] T202;
  wire[7:0] T203;
  wire T204;
  wire[2:0] T205;
  wire[2:0] T206;
  wire[7:0] T207;
  wire T208;
  wire T209;
  wire[7:0] T210;
  wire[7:0] T211;
  wire T212;
  wire[7:0] T213;
  wire T214;
  wire T215;
  wire T216;
  wire T217;
  wire T218;
  wire T219;
  wire[2:0] T220;
  wire T221;
  wire[7:0] T222;
  wire[7:0] T223;
  wire[7:0] T224;
  wire T225;
  wire[2:0] T226;
  wire[2:0] T469;
  wire[3:0] T227;
  wire[3:0] T228;
  wire[3:0] T470;
  wire[7:0] T229;
  wire T230;
  wire T231;
  wire[7:0] T232;
  wire[7:0] T233;
  wire T234;
  wire[7:0] T235;
  wire T236;
  wire T237;
  wire T238;
  wire T239;
  wire[7:0] T240;
  wire[7:0] T241;
  wire[7:0] T242;
  wire T243;
  wire[2:0] T244;
  wire[2:0] T471;
  wire[3:0] T245;
  wire[3:0] T472;
  wire[7:0] T246;
  wire T247;
  wire T248;
  wire[7:0] T249;
  wire[7:0] T250;
  wire T251;
  wire[7:0] T252;
  wire T253;
  wire T254;
  wire T255;
  wire T256;
  wire[7:0] T257;
  reg [7:0] store_4;
  wire[7:0] T258;
  wire[7:0] T259;
  wire[7:0] T260;
  wire[7:0] T261;
  wire[7:0] T262;
  wire T263;
  wire[2:0] T264;
  wire[2:0] T265;
  wire[7:0] T266;
  wire T267;
  wire T268;
  wire[7:0] T269;
  wire[7:0] T270;
  wire T271;
  wire[7:0] T272;
  wire T273;
  wire T274;
  wire T275;
  wire T276;
  wire T277;
  wire T278;
  wire[2:0] T279;
  wire T280;
  wire[7:0] T281;
  wire[7:0] T282;
  wire[7:0] T283;
  wire T284;
  wire[2:0] T285;
  wire[2:0] T473;
  wire[3:0] T286;
  wire[3:0] T287;
  wire[3:0] T474;
  wire[7:0] T288;
  wire T289;
  wire T290;
  wire[7:0] T291;
  wire[7:0] T292;
  wire T293;
  wire[7:0] T294;
  wire T295;
  wire T296;
  wire T297;
  wire T298;
  wire[7:0] T299;
  wire[7:0] T300;
  wire[7:0] T301;
  wire T302;
  wire[2:0] T303;
  wire[2:0] T475;
  wire[3:0] T304;
  wire[3:0] T476;
  wire[7:0] T305;
  wire T306;
  wire T307;
  wire[7:0] T308;
  wire[7:0] T309;
  wire T310;
  wire[7:0] T311;
  wire T312;
  wire T313;
  wire T314;
  wire T315;
  wire[7:0] T316;
  reg [7:0] store_5;
  wire[7:0] T317;
  wire[7:0] T318;
  wire[7:0] T319;
  wire[7:0] T320;
  wire[7:0] T321;
  wire T322;
  wire[2:0] T323;
  wire[2:0] T324;
  wire[7:0] T325;
  wire T326;
  wire T327;
  wire[7:0] T328;
  wire[7:0] T329;
  wire T330;
  wire[7:0] T331;
  wire T332;
  wire T333;
  wire T334;
  wire T335;
  wire T336;
  wire T337;
  wire[2:0] T338;
  wire T339;
  wire[7:0] T340;
  wire[7:0] T341;
  wire[7:0] T342;
  wire T343;
  wire[2:0] T344;
  wire[2:0] T477;
  wire[3:0] T345;
  wire[3:0] T346;
  wire[3:0] T478;
  wire[7:0] T347;
  wire T348;
  wire T349;
  wire[7:0] T350;
  wire[7:0] T351;
  wire T352;
  wire[7:0] T353;
  wire T354;
  wire T355;
  wire T356;
  wire T357;
  wire[7:0] T358;
  wire[7:0] T359;
  wire[7:0] T360;
  wire T361;
  wire[2:0] T362;
  wire[2:0] T479;
  wire[3:0] T363;
  wire[3:0] T480;
  wire[7:0] T364;
  wire T365;
  wire T366;
  wire[7:0] T367;
  wire[7:0] T368;
  wire T369;
  wire[7:0] T370;
  wire T371;
  wire T372;
  wire T373;
  wire T374;
  wire[7:0] T375;
  reg [7:0] store_6;
  wire[7:0] T376;
  wire[7:0] T377;
  wire[7:0] T378;
  wire[7:0] T379;
  wire[7:0] T380;
  wire T381;
  wire[2:0] T382;
  wire[2:0] T383;
  wire[7:0] T384;
  wire T385;
  wire T386;
  wire[7:0] T387;
  wire[7:0] T388;
  wire T389;
  wire[7:0] T390;
  wire T391;
  wire T392;
  wire T393;
  wire T394;
  wire T395;
  wire T396;
  wire[2:0] T397;
  wire T398;
  wire[7:0] T399;
  wire[7:0] T400;
  wire[7:0] T401;
  wire T402;
  wire[2:0] T403;
  wire[2:0] T481;
  wire[3:0] T404;
  wire[3:0] T405;
  wire[3:0] T482;
  wire[7:0] T406;
  wire T407;
  wire T408;
  wire[7:0] T409;
  wire[7:0] T410;
  wire T411;
  wire[7:0] T412;
  wire T413;
  wire T414;
  wire T415;
  wire T416;
  wire[7:0] T417;
  wire[7:0] T418;
  wire[7:0] T419;
  wire T420;
  wire[2:0] T421;
  wire[2:0] T483;
  wire[3:0] T422;
  wire[3:0] T484;
  wire[7:0] T423;
  wire T424;
  wire T425;
  wire[7:0] T426;
  wire[7:0] T427;
  wire T428;
  wire[7:0] T429;
  wire T430;
  wire T431;
  wire T432;
  wire T433;
  wire[7:0] T434;
  wire[7:0] T435;
  wire[7:0] T436;
  wire[7:0] T437;
  wire T438;
  wire[2:0] T439;
  wire[2:0] T485;
  wire[3:0] T440;
  wire[3:0] T486;
  wire[7:0] T441;
  wire T442;
  wire T443;
  wire[7:0] T444;
  wire[7:0] T445;
  wire T446;
  wire[7:0] T447;
  wire T448;
  wire T449;
  wire T450;
  wire T451;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    count = {1{$random}};
    store_0 = {1{$random}};
    store_1 = {1{$random}};
    store_2 = {1{$random}};
    store_3 = {1{$random}};
    store_4 = {1{$random}};
    store_5 = {1{$random}};
    store_6 = {1{$random}};
  end
// synthesis translate_on
`endif

  assign T0 = reset ^ 1'h1;
  assign T1 = io_len;
  assign T3 = reset ^ 1'h1;
  assign T4 = count;
  assign T452 = T453[2:0];
  assign T453 = reset ? 4'h0 : T5;
  assign T5 = T12 ? T456 : T6;
  assign T6 = io_vld ? total : T454;
  assign T454 = {1'h0, count};
  assign total = T7;
  assign T7 = io_vld ? T9 : T8;
  assign T8 = {1'h0, count};
  assign T9 = T10 + T455;
  assign T455 = {1'h0, io_len};
  assign T10 = {1'h0, count};
  assign T456 = {1'h0, T11};
  assign T11 = count + io_len;
  assign T12 = io_vld & io_vldOut;
  assign T14 = reset ^ 1'h1;
  assign T15 = total;
  assign io_vldOut = T17;
  assign T17 = T20 | T18;
  assign T18 = io_vld & T19;
  assign T19 = io_len == 3'h0;
  assign T20 = 4'h7 < total;
  assign io_dataOut_0 = T21;
  assign T21 = T79 ? T63 : store_0;
  assign T22 = T62 ? T45 : T23;
  assign T23 = T40 ? T24 : store_0;
  assign T24 = T39 ? T33 : T25;
  assign T25 = T32 ? T30 : T26;
  assign T26 = T27 ? io_dataIn_1 : io_dataIn_0;
  assign T27 = T28[0];
  assign T28 = T29;
  assign T29 = 3'h0 - count;
  assign T30 = T31 ? io_dataIn_3 : io_dataIn_2;
  assign T31 = T28[0];
  assign T32 = T28[1];
  assign T33 = T38 ? T36 : T34;
  assign T34 = T35 ? io_dataIn_5 : io_dataIn_4;
  assign T35 = T28[0];
  assign T36 = T37 ? io_dataIn_7 : io_dataIn_6;
  assign T37 = T28[0];
  assign T38 = T28[1];
  assign T39 = T28[2];
  assign T40 = io_vld & T41;
  assign T41 = T44 & T42;
  assign T42 = 3'h0 <= T43;
  assign T43 = count + io_len;
  assign T44 = count <= 3'h0;
  assign T45 = T61 ? T55 : T46;
  assign T46 = T54 ? T52 : T47;
  assign T47 = T48 ? io_dataIn_1 : io_dataIn_0;
  assign T48 = T49[0];
  assign T49 = T457;
  assign T457 = T50[2:0];
  assign T50 = T51 + 4'h0;
  assign T51 = 4'h8 - T458;
  assign T458 = {1'h0, count};
  assign T52 = T53 ? io_dataIn_3 : io_dataIn_2;
  assign T53 = T49[0];
  assign T54 = T49[1];
  assign T55 = T60 ? T58 : T56;
  assign T56 = T57 ? io_dataIn_5 : io_dataIn_4;
  assign T57 = T49[0];
  assign T58 = T59 ? io_dataIn_7 : io_dataIn_6;
  assign T59 = T49[0];
  assign T60 = T49[1];
  assign T61 = T49[2];
  assign T62 = io_vld & io_vldOut;
  assign T63 = T78 ? T72 : T64;
  assign T64 = T71 ? T69 : T65;
  assign T65 = T66 ? io_dataIn_1 : io_dataIn_0;
  assign T66 = T67[0];
  assign T67 = T459;
  assign T459 = T68[2:0];
  assign T68 = 4'h0 - T460;
  assign T460 = {1'h0, count};
  assign T69 = T70 ? io_dataIn_3 : io_dataIn_2;
  assign T70 = T67[0];
  assign T71 = T67[1];
  assign T72 = T77 ? T75 : T73;
  assign T73 = T74 ? io_dataIn_5 : io_dataIn_4;
  assign T74 = T67[0];
  assign T75 = T76 ? io_dataIn_7 : io_dataIn_6;
  assign T76 = T67[0];
  assign T77 = T67[1];
  assign T78 = T67[2];
  assign T79 = count <= 3'h0;
  assign io_dataOut_1 = T80;
  assign T80 = T138 ? T122 : store_1;
  assign T81 = T121 ? T104 : T82;
  assign T82 = T99 ? T83 : store_1;
  assign T83 = T98 ? T92 : T84;
  assign T84 = T91 ? T89 : T85;
  assign T85 = T86 ? io_dataIn_1 : io_dataIn_0;
  assign T86 = T87[0];
  assign T87 = T88;
  assign T88 = 3'h1 - count;
  assign T89 = T90 ? io_dataIn_3 : io_dataIn_2;
  assign T90 = T87[0];
  assign T91 = T87[1];
  assign T92 = T97 ? T95 : T93;
  assign T93 = T94 ? io_dataIn_5 : io_dataIn_4;
  assign T94 = T87[0];
  assign T95 = T96 ? io_dataIn_7 : io_dataIn_6;
  assign T96 = T87[0];
  assign T97 = T87[1];
  assign T98 = T87[2];
  assign T99 = io_vld & T100;
  assign T100 = T103 & T101;
  assign T101 = 3'h1 <= T102;
  assign T102 = count + io_len;
  assign T103 = count <= 3'h1;
  assign T104 = T120 ? T114 : T105;
  assign T105 = T113 ? T111 : T106;
  assign T106 = T107 ? io_dataIn_1 : io_dataIn_0;
  assign T107 = T108[0];
  assign T108 = T461;
  assign T461 = T109[2:0];
  assign T109 = T110 + 4'h1;
  assign T110 = 4'h8 - T462;
  assign T462 = {1'h0, count};
  assign T111 = T112 ? io_dataIn_3 : io_dataIn_2;
  assign T112 = T108[0];
  assign T113 = T108[1];
  assign T114 = T119 ? T117 : T115;
  assign T115 = T116 ? io_dataIn_5 : io_dataIn_4;
  assign T116 = T108[0];
  assign T117 = T118 ? io_dataIn_7 : io_dataIn_6;
  assign T118 = T108[0];
  assign T119 = T108[1];
  assign T120 = T108[2];
  assign T121 = io_vld & io_vldOut;
  assign T122 = T137 ? T131 : T123;
  assign T123 = T130 ? T128 : T124;
  assign T124 = T125 ? io_dataIn_1 : io_dataIn_0;
  assign T125 = T126[0];
  assign T126 = T463;
  assign T463 = T127[2:0];
  assign T127 = 4'h1 - T464;
  assign T464 = {1'h0, count};
  assign T128 = T129 ? io_dataIn_3 : io_dataIn_2;
  assign T129 = T126[0];
  assign T130 = T126[1];
  assign T131 = T136 ? T134 : T132;
  assign T132 = T133 ? io_dataIn_5 : io_dataIn_4;
  assign T133 = T126[0];
  assign T134 = T135 ? io_dataIn_7 : io_dataIn_6;
  assign T135 = T126[0];
  assign T136 = T126[1];
  assign T137 = T126[2];
  assign T138 = count <= 3'h1;
  assign io_dataOut_2 = T139;
  assign T139 = T197 ? T181 : store_2;
  assign T140 = T180 ? T163 : T141;
  assign T141 = T158 ? T142 : store_2;
  assign T142 = T157 ? T151 : T143;
  assign T143 = T150 ? T148 : T144;
  assign T144 = T145 ? io_dataIn_1 : io_dataIn_0;
  assign T145 = T146[0];
  assign T146 = T147;
  assign T147 = 3'h2 - count;
  assign T148 = T149 ? io_dataIn_3 : io_dataIn_2;
  assign T149 = T146[0];
  assign T150 = T146[1];
  assign T151 = T156 ? T154 : T152;
  assign T152 = T153 ? io_dataIn_5 : io_dataIn_4;
  assign T153 = T146[0];
  assign T154 = T155 ? io_dataIn_7 : io_dataIn_6;
  assign T155 = T146[0];
  assign T156 = T146[1];
  assign T157 = T146[2];
  assign T158 = io_vld & T159;
  assign T159 = T162 & T160;
  assign T160 = 3'h2 <= T161;
  assign T161 = count + io_len;
  assign T162 = count <= 3'h2;
  assign T163 = T179 ? T173 : T164;
  assign T164 = T172 ? T170 : T165;
  assign T165 = T166 ? io_dataIn_1 : io_dataIn_0;
  assign T166 = T167[0];
  assign T167 = T465;
  assign T465 = T168[2:0];
  assign T168 = T169 + 4'h2;
  assign T169 = 4'h8 - T466;
  assign T466 = {1'h0, count};
  assign T170 = T171 ? io_dataIn_3 : io_dataIn_2;
  assign T171 = T167[0];
  assign T172 = T167[1];
  assign T173 = T178 ? T176 : T174;
  assign T174 = T175 ? io_dataIn_5 : io_dataIn_4;
  assign T175 = T167[0];
  assign T176 = T177 ? io_dataIn_7 : io_dataIn_6;
  assign T177 = T167[0];
  assign T178 = T167[1];
  assign T179 = T167[2];
  assign T180 = io_vld & io_vldOut;
  assign T181 = T196 ? T190 : T182;
  assign T182 = T189 ? T187 : T183;
  assign T183 = T184 ? io_dataIn_1 : io_dataIn_0;
  assign T184 = T185[0];
  assign T185 = T467;
  assign T467 = T186[2:0];
  assign T186 = 4'h2 - T468;
  assign T468 = {1'h0, count};
  assign T187 = T188 ? io_dataIn_3 : io_dataIn_2;
  assign T188 = T185[0];
  assign T189 = T185[1];
  assign T190 = T195 ? T193 : T191;
  assign T191 = T192 ? io_dataIn_5 : io_dataIn_4;
  assign T192 = T185[0];
  assign T193 = T194 ? io_dataIn_7 : io_dataIn_6;
  assign T194 = T185[0];
  assign T195 = T185[1];
  assign T196 = T185[2];
  assign T197 = count <= 3'h2;
  assign io_dataOut_3 = T198;
  assign T198 = T256 ? T240 : store_3;
  assign T199 = T239 ? T222 : T200;
  assign T200 = T217 ? T201 : store_3;
  assign T201 = T216 ? T210 : T202;
  assign T202 = T209 ? T207 : T203;
  assign T203 = T204 ? io_dataIn_1 : io_dataIn_0;
  assign T204 = T205[0];
  assign T205 = T206;
  assign T206 = 3'h3 - count;
  assign T207 = T208 ? io_dataIn_3 : io_dataIn_2;
  assign T208 = T205[0];
  assign T209 = T205[1];
  assign T210 = T215 ? T213 : T211;
  assign T211 = T212 ? io_dataIn_5 : io_dataIn_4;
  assign T212 = T205[0];
  assign T213 = T214 ? io_dataIn_7 : io_dataIn_6;
  assign T214 = T205[0];
  assign T215 = T205[1];
  assign T216 = T205[2];
  assign T217 = io_vld & T218;
  assign T218 = T221 & T219;
  assign T219 = 3'h3 <= T220;
  assign T220 = count + io_len;
  assign T221 = count <= 3'h3;
  assign T222 = T238 ? T232 : T223;
  assign T223 = T231 ? T229 : T224;
  assign T224 = T225 ? io_dataIn_1 : io_dataIn_0;
  assign T225 = T226[0];
  assign T226 = T469;
  assign T469 = T227[2:0];
  assign T227 = T228 + 4'h3;
  assign T228 = 4'h8 - T470;
  assign T470 = {1'h0, count};
  assign T229 = T230 ? io_dataIn_3 : io_dataIn_2;
  assign T230 = T226[0];
  assign T231 = T226[1];
  assign T232 = T237 ? T235 : T233;
  assign T233 = T234 ? io_dataIn_5 : io_dataIn_4;
  assign T234 = T226[0];
  assign T235 = T236 ? io_dataIn_7 : io_dataIn_6;
  assign T236 = T226[0];
  assign T237 = T226[1];
  assign T238 = T226[2];
  assign T239 = io_vld & io_vldOut;
  assign T240 = T255 ? T249 : T241;
  assign T241 = T248 ? T246 : T242;
  assign T242 = T243 ? io_dataIn_1 : io_dataIn_0;
  assign T243 = T244[0];
  assign T244 = T471;
  assign T471 = T245[2:0];
  assign T245 = 4'h3 - T472;
  assign T472 = {1'h0, count};
  assign T246 = T247 ? io_dataIn_3 : io_dataIn_2;
  assign T247 = T244[0];
  assign T248 = T244[1];
  assign T249 = T254 ? T252 : T250;
  assign T250 = T251 ? io_dataIn_5 : io_dataIn_4;
  assign T251 = T244[0];
  assign T252 = T253 ? io_dataIn_7 : io_dataIn_6;
  assign T253 = T244[0];
  assign T254 = T244[1];
  assign T255 = T244[2];
  assign T256 = count <= 3'h3;
  assign io_dataOut_4 = T257;
  assign T257 = T315 ? T299 : store_4;
  assign T258 = T298 ? T281 : T259;
  assign T259 = T276 ? T260 : store_4;
  assign T260 = T275 ? T269 : T261;
  assign T261 = T268 ? T266 : T262;
  assign T262 = T263 ? io_dataIn_1 : io_dataIn_0;
  assign T263 = T264[0];
  assign T264 = T265;
  assign T265 = 3'h4 - count;
  assign T266 = T267 ? io_dataIn_3 : io_dataIn_2;
  assign T267 = T264[0];
  assign T268 = T264[1];
  assign T269 = T274 ? T272 : T270;
  assign T270 = T271 ? io_dataIn_5 : io_dataIn_4;
  assign T271 = T264[0];
  assign T272 = T273 ? io_dataIn_7 : io_dataIn_6;
  assign T273 = T264[0];
  assign T274 = T264[1];
  assign T275 = T264[2];
  assign T276 = io_vld & T277;
  assign T277 = T280 & T278;
  assign T278 = 3'h4 <= T279;
  assign T279 = count + io_len;
  assign T280 = count <= 3'h4;
  assign T281 = T297 ? T291 : T282;
  assign T282 = T290 ? T288 : T283;
  assign T283 = T284 ? io_dataIn_1 : io_dataIn_0;
  assign T284 = T285[0];
  assign T285 = T473;
  assign T473 = T286[2:0];
  assign T286 = T287 + 4'h4;
  assign T287 = 4'h8 - T474;
  assign T474 = {1'h0, count};
  assign T288 = T289 ? io_dataIn_3 : io_dataIn_2;
  assign T289 = T285[0];
  assign T290 = T285[1];
  assign T291 = T296 ? T294 : T292;
  assign T292 = T293 ? io_dataIn_5 : io_dataIn_4;
  assign T293 = T285[0];
  assign T294 = T295 ? io_dataIn_7 : io_dataIn_6;
  assign T295 = T285[0];
  assign T296 = T285[1];
  assign T297 = T285[2];
  assign T298 = io_vld & io_vldOut;
  assign T299 = T314 ? T308 : T300;
  assign T300 = T307 ? T305 : T301;
  assign T301 = T302 ? io_dataIn_1 : io_dataIn_0;
  assign T302 = T303[0];
  assign T303 = T475;
  assign T475 = T304[2:0];
  assign T304 = 4'h4 - T476;
  assign T476 = {1'h0, count};
  assign T305 = T306 ? io_dataIn_3 : io_dataIn_2;
  assign T306 = T303[0];
  assign T307 = T303[1];
  assign T308 = T313 ? T311 : T309;
  assign T309 = T310 ? io_dataIn_5 : io_dataIn_4;
  assign T310 = T303[0];
  assign T311 = T312 ? io_dataIn_7 : io_dataIn_6;
  assign T312 = T303[0];
  assign T313 = T303[1];
  assign T314 = T303[2];
  assign T315 = count <= 3'h4;
  assign io_dataOut_5 = T316;
  assign T316 = T374 ? T358 : store_5;
  assign T317 = T357 ? T340 : T318;
  assign T318 = T335 ? T319 : store_5;
  assign T319 = T334 ? T328 : T320;
  assign T320 = T327 ? T325 : T321;
  assign T321 = T322 ? io_dataIn_1 : io_dataIn_0;
  assign T322 = T323[0];
  assign T323 = T324;
  assign T324 = 3'h5 - count;
  assign T325 = T326 ? io_dataIn_3 : io_dataIn_2;
  assign T326 = T323[0];
  assign T327 = T323[1];
  assign T328 = T333 ? T331 : T329;
  assign T329 = T330 ? io_dataIn_5 : io_dataIn_4;
  assign T330 = T323[0];
  assign T331 = T332 ? io_dataIn_7 : io_dataIn_6;
  assign T332 = T323[0];
  assign T333 = T323[1];
  assign T334 = T323[2];
  assign T335 = io_vld & T336;
  assign T336 = T339 & T337;
  assign T337 = 3'h5 <= T338;
  assign T338 = count + io_len;
  assign T339 = count <= 3'h5;
  assign T340 = T356 ? T350 : T341;
  assign T341 = T349 ? T347 : T342;
  assign T342 = T343 ? io_dataIn_1 : io_dataIn_0;
  assign T343 = T344[0];
  assign T344 = T477;
  assign T477 = T345[2:0];
  assign T345 = T346 + 4'h5;
  assign T346 = 4'h8 - T478;
  assign T478 = {1'h0, count};
  assign T347 = T348 ? io_dataIn_3 : io_dataIn_2;
  assign T348 = T344[0];
  assign T349 = T344[1];
  assign T350 = T355 ? T353 : T351;
  assign T351 = T352 ? io_dataIn_5 : io_dataIn_4;
  assign T352 = T344[0];
  assign T353 = T354 ? io_dataIn_7 : io_dataIn_6;
  assign T354 = T344[0];
  assign T355 = T344[1];
  assign T356 = T344[2];
  assign T357 = io_vld & io_vldOut;
  assign T358 = T373 ? T367 : T359;
  assign T359 = T366 ? T364 : T360;
  assign T360 = T361 ? io_dataIn_1 : io_dataIn_0;
  assign T361 = T362[0];
  assign T362 = T479;
  assign T479 = T363[2:0];
  assign T363 = 4'h5 - T480;
  assign T480 = {1'h0, count};
  assign T364 = T365 ? io_dataIn_3 : io_dataIn_2;
  assign T365 = T362[0];
  assign T366 = T362[1];
  assign T367 = T372 ? T370 : T368;
  assign T368 = T369 ? io_dataIn_5 : io_dataIn_4;
  assign T369 = T362[0];
  assign T370 = T371 ? io_dataIn_7 : io_dataIn_6;
  assign T371 = T362[0];
  assign T372 = T362[1];
  assign T373 = T362[2];
  assign T374 = count <= 3'h5;
  assign io_dataOut_6 = T375;
  assign T375 = T433 ? T417 : store_6;
  assign T376 = T416 ? T399 : T377;
  assign T377 = T394 ? T378 : store_6;
  assign T378 = T393 ? T387 : T379;
  assign T379 = T386 ? T384 : T380;
  assign T380 = T381 ? io_dataIn_1 : io_dataIn_0;
  assign T381 = T382[0];
  assign T382 = T383;
  assign T383 = 3'h6 - count;
  assign T384 = T385 ? io_dataIn_3 : io_dataIn_2;
  assign T385 = T382[0];
  assign T386 = T382[1];
  assign T387 = T392 ? T390 : T388;
  assign T388 = T389 ? io_dataIn_5 : io_dataIn_4;
  assign T389 = T382[0];
  assign T390 = T391 ? io_dataIn_7 : io_dataIn_6;
  assign T391 = T382[0];
  assign T392 = T382[1];
  assign T393 = T382[2];
  assign T394 = io_vld & T395;
  assign T395 = T398 & T396;
  assign T396 = 3'h6 <= T397;
  assign T397 = count + io_len;
  assign T398 = count <= 3'h6;
  assign T399 = T415 ? T409 : T400;
  assign T400 = T408 ? T406 : T401;
  assign T401 = T402 ? io_dataIn_1 : io_dataIn_0;
  assign T402 = T403[0];
  assign T403 = T481;
  assign T481 = T404[2:0];
  assign T404 = T405 + 4'h6;
  assign T405 = 4'h8 - T482;
  assign T482 = {1'h0, count};
  assign T406 = T407 ? io_dataIn_3 : io_dataIn_2;
  assign T407 = T403[0];
  assign T408 = T403[1];
  assign T409 = T414 ? T412 : T410;
  assign T410 = T411 ? io_dataIn_5 : io_dataIn_4;
  assign T411 = T403[0];
  assign T412 = T413 ? io_dataIn_7 : io_dataIn_6;
  assign T413 = T403[0];
  assign T414 = T403[1];
  assign T415 = T403[2];
  assign T416 = io_vld & io_vldOut;
  assign T417 = T432 ? T426 : T418;
  assign T418 = T425 ? T423 : T419;
  assign T419 = T420 ? io_dataIn_1 : io_dataIn_0;
  assign T420 = T421[0];
  assign T421 = T483;
  assign T483 = T422[2:0];
  assign T422 = 4'h6 - T484;
  assign T484 = {1'h0, count};
  assign T423 = T424 ? io_dataIn_3 : io_dataIn_2;
  assign T424 = T421[0];
  assign T425 = T421[1];
  assign T426 = T431 ? T429 : T427;
  assign T427 = T428 ? io_dataIn_5 : io_dataIn_4;
  assign T428 = T421[0];
  assign T429 = T430 ? io_dataIn_7 : io_dataIn_6;
  assign T430 = T421[0];
  assign T431 = T421[1];
  assign T432 = T421[2];
  assign T433 = count <= 3'h6;
  assign io_dataOut_7 = T434;
  assign T434 = T451 ? T435 : 8'h0;
  assign T435 = T450 ? T444 : T436;
  assign T436 = T443 ? T441 : T437;
  assign T437 = T438 ? io_dataIn_1 : io_dataIn_0;
  assign T438 = T439[0];
  assign T439 = T485;
  assign T485 = T440[2:0];
  assign T440 = 4'h7 - T486;
  assign T486 = {1'h0, count};
  assign T441 = T442 ? io_dataIn_3 : io_dataIn_2;
  assign T442 = T439[0];
  assign T443 = T439[1];
  assign T444 = T449 ? T447 : T445;
  assign T445 = T446 ? io_dataIn_5 : io_dataIn_4;
  assign T446 = T439[0];
  assign T447 = T448 ? io_dataIn_7 : io_dataIn_6;
  assign T448 = T439[0];
  assign T449 = T439[1];
  assign T450 = T439[2];
  assign T451 = count <= 3'h7;

  always @(posedge clk) begin
    count <= T452;
    if(T62) begin
      store_0 <= T45;
    end else if(T40) begin
      store_0 <= T24;
    end
    if(T121) begin
      store_1 <= T104;
    end else if(T99) begin
      store_1 <= T83;
    end
    if(T180) begin
      store_2 <= T163;
    end else if(T158) begin
      store_2 <= T142;
    end
    if(T239) begin
      store_3 <= T222;
    end else if(T217) begin
      store_3 <= T201;
    end
    if(T298) begin
      store_4 <= T281;
    end else if(T276) begin
      store_4 <= T260;
    end
    if(T357) begin
      store_5 <= T340;
    end else if(T335) begin
      store_5 <= T319;
    end
    if(T416) begin
      store_6 <= T399;
    end else if(T394) begin
      store_6 <= T378;
    end
`ifndef SYNTHESIS
// synthesis translate_off
`ifdef PRINTF_COND
    if (`PRINTF_COND)
`endif
      if (T14)
        $fwrite(32'h80000002, "total = %d\n", T15);
// synthesis translate_on
`endif
`ifndef SYNTHESIS
// synthesis translate_off
`ifdef PRINTF_COND
    if (`PRINTF_COND)
`endif
      if (T3)
        $fwrite(32'h80000002, "count = %d\n", T4);
// synthesis translate_on
`endif
`ifndef SYNTHESIS
// synthesis translate_off
`ifdef PRINTF_COND
    if (`PRINTF_COND)
`endif
      if (T0)
        $fwrite(32'h80000002, "len = %d\n", T1);
// synthesis translate_on
`endif
  end
endmodule

module Queue_0(input clk, input reset,
    output io_enq_ready,
    input  io_enq_valid,
    input [7:0] io_enq_bits_7,
    input [7:0] io_enq_bits_6,
    input [7:0] io_enq_bits_5,
    input [7:0] io_enq_bits_4,
    input [7:0] io_enq_bits_3,
    input [7:0] io_enq_bits_2,
    input [7:0] io_enq_bits_1,
    input [7:0] io_enq_bits_0,
    input  io_deq_ready,
    output io_deq_valid,
    output[7:0] io_deq_bits_7,
    output[7:0] io_deq_bits_6,
    output[7:0] io_deq_bits_5,
    output[7:0] io_deq_bits_4,
    output[7:0] io_deq_bits_3,
    output[7:0] io_deq_bits_2,
    output[7:0] io_deq_bits_1,
    output[7:0] io_deq_bits_0,
    output[5:0] io_count
);

  wire[5:0] T0;
  wire[4:0] ptr_diff;
  reg [4:0] R1;
  wire[4:0] T31;
  wire[4:0] T2;
  wire[4:0] T3;
  wire do_deq;
  reg [4:0] R4;
  wire[4:0] T32;
  wire[4:0] T5;
  wire[4:0] T6;
  wire do_enq;
  wire T7;
  wire ptr_match;
  reg  maybe_full;
  wire T33;
  wire T8;
  wire T9;
  wire[7:0] T10;
  wire[63:0] T11;
  reg [63:0] ram [31:0];
  wire[63:0] T12;
  wire[63:0] T13;
  wire[63:0] T14;
  wire[31:0] T15;
  wire[15:0] T16;
  wire[15:0] T17;
  wire[31:0] T18;
  wire[15:0] T19;
  wire[15:0] T20;
  wire[7:0] T21;
  wire[7:0] T22;
  wire[7:0] T23;
  wire[7:0] T24;
  wire[7:0] T25;
  wire[7:0] T26;
  wire[7:0] T27;
  wire T28;
  wire empty;
  wire T29;
  wire T30;
  wire full;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    R1 = {1{$random}};
    R4 = {1{$random}};
    maybe_full = {1{$random}};
    for (initvar = 0; initvar < 32; initvar = initvar+1)
      ram[initvar] = {2{$random}};
  end
// synthesis translate_on
`endif

  assign io_count = T0;
  assign T0 = {T7, ptr_diff};
  assign ptr_diff = R4 - R1;
  assign T31 = reset ? 5'h0 : T2;
  assign T2 = do_deq ? T3 : R1;
  assign T3 = R1 + 5'h1;
  assign do_deq = io_deq_ready & io_deq_valid;
  assign T32 = reset ? 5'h0 : T5;
  assign T5 = do_enq ? T6 : R4;
  assign T6 = R4 + 5'h1;
  assign do_enq = io_enq_ready & io_enq_valid;
  assign T7 = maybe_full & ptr_match;
  assign ptr_match = R4 == R1;
  assign T33 = reset ? 1'h0 : T8;
  assign T8 = T9 ? do_enq : maybe_full;
  assign T9 = do_enq != do_deq;
  assign io_deq_bits_0 = T10;
  assign T10 = T11[7:0];
  assign T11 = ram[R1];
  assign T13 = T14;
  assign T14 = {T18, T15};
  assign T15 = {T17, T16};
  assign T16 = {io_enq_bits_1, io_enq_bits_0};
  assign T17 = {io_enq_bits_3, io_enq_bits_2};
  assign T18 = {T20, T19};
  assign T19 = {io_enq_bits_5, io_enq_bits_4};
  assign T20 = {io_enq_bits_7, io_enq_bits_6};
  assign io_deq_bits_1 = T21;
  assign T21 = T11[15:8];
  assign io_deq_bits_2 = T22;
  assign T22 = T11[23:16];
  assign io_deq_bits_3 = T23;
  assign T23 = T11[31:24];
  assign io_deq_bits_4 = T24;
  assign T24 = T11[39:32];
  assign io_deq_bits_5 = T25;
  assign T25 = T11[47:40];
  assign io_deq_bits_6 = T26;
  assign T26 = T11[55:48];
  assign io_deq_bits_7 = T27;
  assign T27 = T11[63:56];
  assign io_deq_valid = T28;
  assign T28 = empty ^ 1'h1;
  assign empty = ptr_match & T29;
  assign T29 = maybe_full ^ 1'h1;
  assign io_enq_ready = T30;
  assign T30 = full ^ 1'h1;
  assign full = ptr_match & maybe_full;

  always @(posedge clk) begin
    if(reset) begin
      R1 <= 5'h0;
    end else if(do_deq) begin
      R1 <= T3;
    end
    if(reset) begin
      R4 <= 5'h0;
    end else if(do_enq) begin
      R4 <= T6;
    end
    if(reset) begin
      maybe_full <= 1'h0;
    end else if(T9) begin
      maybe_full <= do_enq;
    end
    if (do_enq)
      ram[R4] <= T13;
  end
endmodule

module Queue_1(input clk, input reset,
    output io_enq_ready,
    input  io_enq_valid,
    input  io_enq_bits_7,
    input  io_enq_bits_6,
    input  io_enq_bits_5,
    input  io_enq_bits_4,
    input  io_enq_bits_3,
    input  io_enq_bits_2,
    input  io_enq_bits_1,
    input  io_enq_bits_0,
    input  io_deq_ready,
    output io_deq_valid,
    output io_deq_bits_7,
    output io_deq_bits_6,
    output io_deq_bits_5,
    output io_deq_bits_4,
    output io_deq_bits_3,
    output io_deq_bits_2,
    output io_deq_bits_1,
    output io_deq_bits_0,
    output[5:0] io_count
);

  wire[5:0] T0;
  wire[4:0] ptr_diff;
  reg [4:0] R1;
  wire[4:0] T31;
  wire[4:0] T2;
  wire[4:0] T3;
  wire do_deq;
  reg [4:0] R4;
  wire[4:0] T32;
  wire[4:0] T5;
  wire[4:0] T6;
  wire do_enq;
  wire T7;
  wire ptr_match;
  reg  maybe_full;
  wire T33;
  wire T8;
  wire T9;
  wire T10;
  wire[7:0] T11;
  reg [7:0] ram [31:0];
  wire[7:0] T12;
  wire[7:0] T13;
  wire[7:0] T14;
  wire[3:0] T15;
  wire[1:0] T16;
  wire[1:0] T17;
  wire[3:0] T18;
  wire[1:0] T19;
  wire[1:0] T20;
  wire T21;
  wire T22;
  wire T23;
  wire T24;
  wire T25;
  wire T26;
  wire T27;
  wire T28;
  wire empty;
  wire T29;
  wire T30;
  wire full;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    R1 = {1{$random}};
    R4 = {1{$random}};
    maybe_full = {1{$random}};
    for (initvar = 0; initvar < 32; initvar = initvar+1)
      ram[initvar] = {1{$random}};
  end
// synthesis translate_on
`endif

  assign io_count = T0;
  assign T0 = {T7, ptr_diff};
  assign ptr_diff = R4 - R1;
  assign T31 = reset ? 5'h0 : T2;
  assign T2 = do_deq ? T3 : R1;
  assign T3 = R1 + 5'h1;
  assign do_deq = io_deq_ready & io_deq_valid;
  assign T32 = reset ? 5'h0 : T5;
  assign T5 = do_enq ? T6 : R4;
  assign T6 = R4 + 5'h1;
  assign do_enq = io_enq_ready & io_enq_valid;
  assign T7 = maybe_full & ptr_match;
  assign ptr_match = R4 == R1;
  assign T33 = reset ? 1'h0 : T8;
  assign T8 = T9 ? do_enq : maybe_full;
  assign T9 = do_enq != do_deq;
  assign io_deq_bits_0 = T10;
  assign T10 = T11[0];
  assign T11 = ram[R1];
  assign T13 = T14;
  assign T14 = {T18, T15};
  assign T15 = {T17, T16};
  assign T16 = {io_enq_bits_1, io_enq_bits_0};
  assign T17 = {io_enq_bits_3, io_enq_bits_2};
  assign T18 = {T20, T19};
  assign T19 = {io_enq_bits_5, io_enq_bits_4};
  assign T20 = {io_enq_bits_7, io_enq_bits_6};
  assign io_deq_bits_1 = T21;
  assign T21 = T11[1];
  assign io_deq_bits_2 = T22;
  assign T22 = T11[2];
  assign io_deq_bits_3 = T23;
  assign T23 = T11[3];
  assign io_deq_bits_4 = T24;
  assign T24 = T11[4];
  assign io_deq_bits_5 = T25;
  assign T25 = T11[5];
  assign io_deq_bits_6 = T26;
  assign T26 = T11[6];
  assign io_deq_bits_7 = T27;
  assign T27 = T11[7];
  assign io_deq_valid = T28;
  assign T28 = empty ^ 1'h1;
  assign empty = ptr_match & T29;
  assign T29 = maybe_full ^ 1'h1;
  assign io_enq_ready = T30;
  assign T30 = full ^ 1'h1;
  assign full = ptr_match & maybe_full;

  always @(posedge clk) begin
    if(reset) begin
      R1 <= 5'h0;
    end else if(do_deq) begin
      R1 <= T3;
    end
    if(reset) begin
      R4 <= 5'h0;
    end else if(do_enq) begin
      R4 <= T6;
    end
    if(reset) begin
      maybe_full <= 1'h0;
    end else if(T9) begin
      maybe_full <= do_enq;
    end
    if (do_enq)
      ram[R4] <= T13;
  end
endmodule

module Serializer(input clk, input reset,
    output io_dataIn_ready,
    input  io_dataIn_valid,
    input  io_dataIn_bits_7,
    input  io_dataIn_bits_6,
    input  io_dataIn_bits_5,
    input  io_dataIn_bits_4,
    input  io_dataIn_bits_3,
    input  io_dataIn_bits_2,
    input  io_dataIn_bits_1,
    input  io_dataIn_bits_0,
    input  io_flush,
    output io_dataOut_valid,
    output io_dataOut_bits_63,
    output io_dataOut_bits_62,
    output io_dataOut_bits_61,
    output io_dataOut_bits_60,
    output io_dataOut_bits_59,
    output io_dataOut_bits_58,
    output io_dataOut_bits_57,
    output io_dataOut_bits_56,
    output io_dataOut_bits_55,
    output io_dataOut_bits_54,
    output io_dataOut_bits_53,
    output io_dataOut_bits_52,
    output io_dataOut_bits_51,
    output io_dataOut_bits_50,
    output io_dataOut_bits_49,
    output io_dataOut_bits_48,
    output io_dataOut_bits_47,
    output io_dataOut_bits_46,
    output io_dataOut_bits_45,
    output io_dataOut_bits_44,
    output io_dataOut_bits_43,
    output io_dataOut_bits_42,
    output io_dataOut_bits_41,
    output io_dataOut_bits_40,
    output io_dataOut_bits_39,
    output io_dataOut_bits_38,
    output io_dataOut_bits_37,
    output io_dataOut_bits_36,
    output io_dataOut_bits_35,
    output io_dataOut_bits_34,
    output io_dataOut_bits_33,
    output io_dataOut_bits_32,
    output io_dataOut_bits_31,
    output io_dataOut_bits_30,
    output io_dataOut_bits_29,
    output io_dataOut_bits_28,
    output io_dataOut_bits_27,
    output io_dataOut_bits_26,
    output io_dataOut_bits_25,
    output io_dataOut_bits_24,
    output io_dataOut_bits_23,
    output io_dataOut_bits_22,
    output io_dataOut_bits_21,
    output io_dataOut_bits_20,
    output io_dataOut_bits_19,
    output io_dataOut_bits_18,
    output io_dataOut_bits_17,
    output io_dataOut_bits_16,
    output io_dataOut_bits_15,
    output io_dataOut_bits_14,
    output io_dataOut_bits_13,
    output io_dataOut_bits_12,
    output io_dataOut_bits_11,
    output io_dataOut_bits_10,
    output io_dataOut_bits_9,
    output io_dataOut_bits_8,
    output io_dataOut_bits_7,
    output io_dataOut_bits_6,
    output io_dataOut_bits_5,
    output io_dataOut_bits_4,
    output io_dataOut_bits_3,
    output io_dataOut_bits_2,
    output io_dataOut_bits_1,
    output io_dataOut_bits_0,
    output io_flushed
);

  wire T0;
  reg  R1;
  wire T2;
  wire T3;
  wire T4;
  wire T5;
  reg [2:0] R6;
  wire[2:0] T346;
  wire[3:0] T347;
  wire[3:0] T7;
  wire[3:0] T8;
  wire[3:0] T9;
  wire[3:0] T348;
  wire[3:0] T10;
  wire[3:0] T349;
  wire[3:0] T350;
  wire[2:0] T11;
  wire T12;
  wire T13;
  wire T14;
  wire T15;
  wire T16;
  wire T17;
  wire[7:0] vecOutComb_0;
  wire[7:0] T18;
  wire[7:0] T19;
  wire[7:0] T20;
  wire[7:0] T21;
  wire[7:0] T22;
  wire[7:0] T23;
  wire[7:0] T24;
  wire[7:0] T25;
  wire[7:0] T26;
  wire T34;
  wire T35;
  wire[7:0] T36;
  wire[2:0] T37;
  wire T38;
  wire T39;
  wire T40;
  wire[2:0] T41;
  wire[2:0] T42;
  wire T43;
  wire[2:0] T44;
  wire[2:0] T351;
  wire[3:0] T45;
  wire[3:0] T46;
  wire[3:0] T352;
  wire[2:0] T47;
  wire T48;
  wire T49;
  wire T50;
  wire T51;
  wire T52;
  wire T53;
  wire[7:0] T54;
  wire[2:0] T55;
  wire T56;
  wire T57;
  wire T58;
  wire T59;
  wire T60;
  wire T61;
  wire[7:0] T62;
  wire[2:0] T63;
  wire T64;
  wire T65;
  wire T66;
  wire T67;
  wire T68;
  wire T69;
  wire[7:0] T70;
  wire[2:0] T71;
  wire T72;
  wire T73;
  wire T74;
  wire T75;
  wire T76;
  wire T77;
  wire[7:0] T78;
  wire[2:0] T79;
  wire T80;
  wire T81;
  wire T82;
  wire T83;
  wire T84;
  wire T85;
  wire[7:0] T86;
  wire[2:0] T87;
  wire T88;
  wire T89;
  wire T90;
  wire T91;
  wire[7:0] vecInComb_0;
  wire[7:0] T27;
  wire[6:0] T28;
  wire[5:0] T29;
  wire[4:0] T30;
  wire[3:0] T31;
  wire[2:0] T32;
  wire[1:0] T33;
  wire T92;
  wire T93;
  wire[7:0] T94;
  wire[2:0] T95;
  wire T96;
  wire T97;
  wire T98;
  wire T99;
  reg [7:0] R100;
  wire T101;
  wire T102;
  wire T103;
  wire T104;
  wire T105;
  wire T106;
  wire T107;
  wire T108;
  wire[7:0] vecOutComb_1;
  wire[7:0] T109;
  wire[7:0] T110;
  wire[7:0] T111;
  wire[7:0] T112;
  wire[7:0] T113;
  wire[7:0] T114;
  wire[7:0] T115;
  wire[7:0] T116;
  wire[7:0] T117;
  wire T118;
  wire T119;
  wire T120;
  wire T121;
  wire T122;
  wire T123;
  wire T124;
  wire T125;
  wire T126;
  wire T127;
  wire T128;
  wire T129;
  wire T130;
  wire T131;
  reg [7:0] R132;
  wire T133;
  wire T134;
  wire T135;
  wire T136;
  wire T137;
  wire T138;
  wire T139;
  wire T140;
  wire[7:0] vecOutComb_2;
  wire[7:0] T141;
  wire[7:0] T142;
  wire[7:0] T143;
  wire[7:0] T144;
  wire[7:0] T145;
  wire[7:0] T146;
  wire[7:0] T147;
  wire[7:0] T148;
  wire[7:0] T149;
  wire T150;
  wire T151;
  wire T152;
  wire T153;
  wire T154;
  wire T155;
  wire T156;
  wire T157;
  wire T158;
  wire T159;
  wire T160;
  wire T161;
  wire T162;
  wire T163;
  reg [7:0] R164;
  wire T165;
  wire T166;
  wire T167;
  wire T168;
  wire T169;
  wire T170;
  wire T171;
  wire T172;
  wire[7:0] vecOutComb_3;
  wire[7:0] T173;
  wire[7:0] T174;
  wire[7:0] T175;
  wire[7:0] T176;
  wire[7:0] T177;
  wire[7:0] T178;
  wire[7:0] T179;
  wire[7:0] T180;
  wire[7:0] T181;
  wire T182;
  wire T183;
  wire T184;
  wire T185;
  wire T186;
  wire T187;
  wire T188;
  wire T189;
  wire T190;
  wire T191;
  wire T192;
  wire T193;
  wire T194;
  wire T195;
  reg [7:0] R196;
  wire T197;
  wire T198;
  wire T199;
  wire T200;
  wire T201;
  wire T202;
  wire T203;
  wire T204;
  wire[7:0] vecOutComb_4;
  wire[7:0] T205;
  wire[7:0] T206;
  wire[7:0] T207;
  wire[7:0] T208;
  wire[7:0] T209;
  wire[7:0] T210;
  wire[7:0] T211;
  wire[7:0] T212;
  wire[7:0] T213;
  wire T214;
  wire T215;
  wire T216;
  wire T217;
  wire T218;
  wire T219;
  wire T220;
  wire T221;
  wire T222;
  wire T223;
  wire T224;
  wire T225;
  wire T226;
  wire T227;
  reg [7:0] R228;
  wire T229;
  wire T230;
  wire T231;
  wire T232;
  wire T233;
  wire T234;
  wire T235;
  wire T236;
  wire[7:0] vecOutComb_5;
  wire[7:0] T237;
  wire[7:0] T238;
  wire[7:0] T239;
  wire[7:0] T240;
  wire[7:0] T241;
  wire[7:0] T242;
  wire[7:0] T243;
  wire[7:0] T244;
  wire[7:0] T245;
  wire T246;
  wire T247;
  wire T248;
  wire T249;
  wire T250;
  wire T251;
  wire T252;
  wire T253;
  wire T254;
  wire T255;
  wire T256;
  wire T257;
  wire T258;
  wire T259;
  reg [7:0] R260;
  wire T261;
  wire T262;
  wire T263;
  wire T264;
  wire T265;
  wire T266;
  wire T267;
  wire T268;
  wire[7:0] vecOutComb_6;
  wire[7:0] T269;
  wire[7:0] T270;
  wire[7:0] T271;
  wire[7:0] T272;
  wire[7:0] T273;
  wire[7:0] T274;
  wire[7:0] T275;
  wire[7:0] T276;
  wire[7:0] T277;
  wire T278;
  wire T279;
  wire T280;
  wire T281;
  wire T282;
  wire T283;
  wire T284;
  wire T285;
  wire T286;
  wire T287;
  wire T288;
  wire T289;
  wire T290;
  wire T291;
  reg [7:0] R292;
  wire T293;
  wire T294;
  wire T295;
  wire T296;
  wire T297;
  wire T298;
  wire T299;
  wire T300;
  wire[7:0] vecOutComb_7;
  wire[7:0] T301;
  wire[7:0] T302;
  wire[7:0] T303;
  wire[7:0] T304;
  wire[7:0] T305;
  wire[7:0] T306;
  wire T307;
  wire[2:0] T308;
  wire[7:0] T309;
  wire T310;
  wire T311;
  wire[7:0] T312;
  wire[7:0] T313;
  wire T314;
  wire T315;
  wire T316;
  wire T317;
  wire[7:0] T318;
  wire[7:0] T319;
  wire[7:0] T320;
  wire T321;
  wire[2:0] T322;
  wire[7:0] T323;
  wire T324;
  wire T325;
  wire[7:0] T326;
  wire[7:0] T327;
  wire T328;
  wire T329;
  wire T330;
  wire T331;
  wire T332;
  wire T333;
  wire T334;
  wire T335;
  wire T336;
  wire T337;
  wire T338;
  wire T339;
  wire T340;
  wire T341;
  wire T342;
  wire T343;
  wire T344;
  wire T345;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    R1 = {1{$random}};
    R6 = {1{$random}};
    R100 = {1{$random}};
    R132 = {1{$random}};
    R164 = {1{$random}};
    R196 = {1{$random}};
    R228 = {1{$random}};
    R260 = {1{$random}};
    R292 = {1{$random}};
  end
// synthesis translate_on
`endif

  assign io_flushed = T0;
  assign T0 = T15 | R1;
  assign T2 = io_flushed ? 1'h0 : T3;
  assign T3 = T4 & io_flush;
  assign T4 = T5 & io_dataIn_valid;
  assign T5 = 3'h7 < R6;
  assign T346 = T347[2:0];
  assign T347 = reset ? 4'h0 : T7;
  assign T7 = io_flushed ? 4'h0 : T8;
  assign T8 = T12 ? T350 : T9;
  assign T9 = io_dataIn_valid ? T10 : T348;
  assign T348 = {1'h0, R6};
  assign T10 = T349 + 4'h1;
  assign T349 = {1'h0, R6};
  assign T350 = {1'h0, T11};
  assign T11 = R6 - 3'h7;
  assign T12 = io_dataIn_valid & T13;
  assign T13 = T14 & io_dataIn_valid;
  assign T14 = 3'h7 <= R6;
  assign T15 = T16 & io_flush;
  assign T16 = T4 ^ 1'h1;
  assign io_dataOut_bits_0 = T17;
  assign T17 = vecOutComb_0[0];
  assign vecOutComb_0 = T18;
  assign T18 = T13 ? R100 : T19;
  assign T19 = T20;
  assign T20 = T92 ? vecInComb_0 : T21;
  assign T21 = T84 ? vecInComb_0 : T22;
  assign T22 = T76 ? vecInComb_0 : T23;
  assign T23 = T68 ? vecInComb_0 : T24;
  assign T24 = T60 ? vecInComb_0 : T25;
  assign T25 = T52 ? vecInComb_0 : T26;
  assign T26 = T34 ? vecInComb_0 : R100;
  assign T34 = T38 & T35;
  assign T35 = T36[0];
  assign T36 = 1'h1 << T37;
  assign T37 = 3'h0;
  assign T38 = T48 & T39;
  assign T39 = T43 & T40;
  assign T40 = T41 <= 3'h0;
  assign T41 = T42;
  assign T42 = T13 ? 3'h0 : R6;
  assign T43 = 3'h0 <= T44;
  assign T44 = T351;
  assign T351 = T45[2:0];
  assign T45 = T13 ? T352 : T46;
  assign T46 = T10 - 4'h1;
  assign T352 = {1'h0, T47};
  assign T47 = T11 - 3'h1;
  assign T48 = io_dataIn_valid & T49;
  assign T49 = T50 ^ 1'h1;
  assign T50 = T13 & T51;
  assign T51 = T11 == 3'h0;
  assign T52 = T56 & T53;
  assign T53 = T54[0];
  assign T54 = 1'h1 << T55;
  assign T55 = 3'h1;
  assign T56 = T48 & T57;
  assign T57 = T59 & T58;
  assign T58 = T41 <= 3'h1;
  assign T59 = 3'h1 <= T44;
  assign T60 = T64 & T61;
  assign T61 = T62[0];
  assign T62 = 1'h1 << T63;
  assign T63 = 3'h2;
  assign T64 = T48 & T65;
  assign T65 = T67 & T66;
  assign T66 = T41 <= 3'h2;
  assign T67 = 3'h2 <= T44;
  assign T68 = T72 & T69;
  assign T69 = T70[0];
  assign T70 = 1'h1 << T71;
  assign T71 = 3'h3;
  assign T72 = T48 & T73;
  assign T73 = T75 & T74;
  assign T74 = T41 <= 3'h3;
  assign T75 = 3'h3 <= T44;
  assign T76 = T80 & T77;
  assign T77 = T78[0];
  assign T78 = 1'h1 << T79;
  assign T79 = 3'h4;
  assign T80 = T48 & T81;
  assign T81 = T83 & T82;
  assign T82 = T41 <= 3'h4;
  assign T83 = 3'h4 <= T44;
  assign T84 = T88 & T85;
  assign T85 = T86[0];
  assign T86 = 1'h1 << T87;
  assign T87 = 3'h5;
  assign T88 = T48 & T89;
  assign T89 = T91 & T90;
  assign T90 = T41 <= 3'h5;
  assign T91 = 3'h5 <= T44;
  assign vecInComb_0 = T27;
  assign T27 = {T28, io_dataIn_bits_0};
  assign T28 = {T29, io_dataIn_bits_1};
  assign T29 = {T30, io_dataIn_bits_2};
  assign T30 = {T31, io_dataIn_bits_3};
  assign T31 = {T32, io_dataIn_bits_4};
  assign T32 = {T33, io_dataIn_bits_5};
  assign T33 = {io_dataIn_bits_7, io_dataIn_bits_6};
  assign T92 = T96 & T93;
  assign T93 = T94[0];
  assign T94 = 1'h1 << T95;
  assign T95 = 3'h6;
  assign T96 = T48 & T97;
  assign T97 = T99 & T98;
  assign T98 = T41 <= 3'h6;
  assign T99 = 3'h6 <= T44;
  assign io_dataOut_bits_1 = T101;
  assign T101 = vecOutComb_0[1];
  assign io_dataOut_bits_2 = T102;
  assign T102 = vecOutComb_0[2];
  assign io_dataOut_bits_3 = T103;
  assign T103 = vecOutComb_0[3];
  assign io_dataOut_bits_4 = T104;
  assign T104 = vecOutComb_0[4];
  assign io_dataOut_bits_5 = T105;
  assign T105 = vecOutComb_0[5];
  assign io_dataOut_bits_6 = T106;
  assign T106 = vecOutComb_0[6];
  assign io_dataOut_bits_7 = T107;
  assign T107 = vecOutComb_0[7];
  assign io_dataOut_bits_8 = T108;
  assign T108 = vecOutComb_1[0];
  assign vecOutComb_1 = T109;
  assign T109 = T13 ? R132 : T110;
  assign T110 = T111;
  assign T111 = T130 ? vecInComb_0 : T112;
  assign T112 = T128 ? vecInComb_0 : T113;
  assign T113 = T126 ? vecInComb_0 : T114;
  assign T114 = T124 ? vecInComb_0 : T115;
  assign T115 = T122 ? vecInComb_0 : T116;
  assign T116 = T120 ? vecInComb_0 : T117;
  assign T117 = T118 ? vecInComb_0 : R132;
  assign T118 = T38 & T119;
  assign T119 = T36[1];
  assign T120 = T56 & T121;
  assign T121 = T54[1];
  assign T122 = T64 & T123;
  assign T123 = T62[1];
  assign T124 = T72 & T125;
  assign T125 = T70[1];
  assign T126 = T80 & T127;
  assign T127 = T78[1];
  assign T128 = T88 & T129;
  assign T129 = T86[1];
  assign T130 = T96 & T131;
  assign T131 = T94[1];
  assign io_dataOut_bits_9 = T133;
  assign T133 = vecOutComb_1[1];
  assign io_dataOut_bits_10 = T134;
  assign T134 = vecOutComb_1[2];
  assign io_dataOut_bits_11 = T135;
  assign T135 = vecOutComb_1[3];
  assign io_dataOut_bits_12 = T136;
  assign T136 = vecOutComb_1[4];
  assign io_dataOut_bits_13 = T137;
  assign T137 = vecOutComb_1[5];
  assign io_dataOut_bits_14 = T138;
  assign T138 = vecOutComb_1[6];
  assign io_dataOut_bits_15 = T139;
  assign T139 = vecOutComb_1[7];
  assign io_dataOut_bits_16 = T140;
  assign T140 = vecOutComb_2[0];
  assign vecOutComb_2 = T141;
  assign T141 = T13 ? R164 : T142;
  assign T142 = T143;
  assign T143 = T162 ? vecInComb_0 : T144;
  assign T144 = T160 ? vecInComb_0 : T145;
  assign T145 = T158 ? vecInComb_0 : T146;
  assign T146 = T156 ? vecInComb_0 : T147;
  assign T147 = T154 ? vecInComb_0 : T148;
  assign T148 = T152 ? vecInComb_0 : T149;
  assign T149 = T150 ? vecInComb_0 : R164;
  assign T150 = T38 & T151;
  assign T151 = T36[2];
  assign T152 = T56 & T153;
  assign T153 = T54[2];
  assign T154 = T64 & T155;
  assign T155 = T62[2];
  assign T156 = T72 & T157;
  assign T157 = T70[2];
  assign T158 = T80 & T159;
  assign T159 = T78[2];
  assign T160 = T88 & T161;
  assign T161 = T86[2];
  assign T162 = T96 & T163;
  assign T163 = T94[2];
  assign io_dataOut_bits_17 = T165;
  assign T165 = vecOutComb_2[1];
  assign io_dataOut_bits_18 = T166;
  assign T166 = vecOutComb_2[2];
  assign io_dataOut_bits_19 = T167;
  assign T167 = vecOutComb_2[3];
  assign io_dataOut_bits_20 = T168;
  assign T168 = vecOutComb_2[4];
  assign io_dataOut_bits_21 = T169;
  assign T169 = vecOutComb_2[5];
  assign io_dataOut_bits_22 = T170;
  assign T170 = vecOutComb_2[6];
  assign io_dataOut_bits_23 = T171;
  assign T171 = vecOutComb_2[7];
  assign io_dataOut_bits_24 = T172;
  assign T172 = vecOutComb_3[0];
  assign vecOutComb_3 = T173;
  assign T173 = T13 ? R196 : T174;
  assign T174 = T175;
  assign T175 = T194 ? vecInComb_0 : T176;
  assign T176 = T192 ? vecInComb_0 : T177;
  assign T177 = T190 ? vecInComb_0 : T178;
  assign T178 = T188 ? vecInComb_0 : T179;
  assign T179 = T186 ? vecInComb_0 : T180;
  assign T180 = T184 ? vecInComb_0 : T181;
  assign T181 = T182 ? vecInComb_0 : R196;
  assign T182 = T38 & T183;
  assign T183 = T36[3];
  assign T184 = T56 & T185;
  assign T185 = T54[3];
  assign T186 = T64 & T187;
  assign T187 = T62[3];
  assign T188 = T72 & T189;
  assign T189 = T70[3];
  assign T190 = T80 & T191;
  assign T191 = T78[3];
  assign T192 = T88 & T193;
  assign T193 = T86[3];
  assign T194 = T96 & T195;
  assign T195 = T94[3];
  assign io_dataOut_bits_25 = T197;
  assign T197 = vecOutComb_3[1];
  assign io_dataOut_bits_26 = T198;
  assign T198 = vecOutComb_3[2];
  assign io_dataOut_bits_27 = T199;
  assign T199 = vecOutComb_3[3];
  assign io_dataOut_bits_28 = T200;
  assign T200 = vecOutComb_3[4];
  assign io_dataOut_bits_29 = T201;
  assign T201 = vecOutComb_3[5];
  assign io_dataOut_bits_30 = T202;
  assign T202 = vecOutComb_3[6];
  assign io_dataOut_bits_31 = T203;
  assign T203 = vecOutComb_3[7];
  assign io_dataOut_bits_32 = T204;
  assign T204 = vecOutComb_4[0];
  assign vecOutComb_4 = T205;
  assign T205 = T13 ? R228 : T206;
  assign T206 = T207;
  assign T207 = T226 ? vecInComb_0 : T208;
  assign T208 = T224 ? vecInComb_0 : T209;
  assign T209 = T222 ? vecInComb_0 : T210;
  assign T210 = T220 ? vecInComb_0 : T211;
  assign T211 = T218 ? vecInComb_0 : T212;
  assign T212 = T216 ? vecInComb_0 : T213;
  assign T213 = T214 ? vecInComb_0 : R228;
  assign T214 = T38 & T215;
  assign T215 = T36[4];
  assign T216 = T56 & T217;
  assign T217 = T54[4];
  assign T218 = T64 & T219;
  assign T219 = T62[4];
  assign T220 = T72 & T221;
  assign T221 = T70[4];
  assign T222 = T80 & T223;
  assign T223 = T78[4];
  assign T224 = T88 & T225;
  assign T225 = T86[4];
  assign T226 = T96 & T227;
  assign T227 = T94[4];
  assign io_dataOut_bits_33 = T229;
  assign T229 = vecOutComb_4[1];
  assign io_dataOut_bits_34 = T230;
  assign T230 = vecOutComb_4[2];
  assign io_dataOut_bits_35 = T231;
  assign T231 = vecOutComb_4[3];
  assign io_dataOut_bits_36 = T232;
  assign T232 = vecOutComb_4[4];
  assign io_dataOut_bits_37 = T233;
  assign T233 = vecOutComb_4[5];
  assign io_dataOut_bits_38 = T234;
  assign T234 = vecOutComb_4[6];
  assign io_dataOut_bits_39 = T235;
  assign T235 = vecOutComb_4[7];
  assign io_dataOut_bits_40 = T236;
  assign T236 = vecOutComb_5[0];
  assign vecOutComb_5 = T237;
  assign T237 = T13 ? R260 : T238;
  assign T238 = T239;
  assign T239 = T258 ? vecInComb_0 : T240;
  assign T240 = T256 ? vecInComb_0 : T241;
  assign T241 = T254 ? vecInComb_0 : T242;
  assign T242 = T252 ? vecInComb_0 : T243;
  assign T243 = T250 ? vecInComb_0 : T244;
  assign T244 = T248 ? vecInComb_0 : T245;
  assign T245 = T246 ? vecInComb_0 : R260;
  assign T246 = T38 & T247;
  assign T247 = T36[5];
  assign T248 = T56 & T249;
  assign T249 = T54[5];
  assign T250 = T64 & T251;
  assign T251 = T62[5];
  assign T252 = T72 & T253;
  assign T253 = T70[5];
  assign T254 = T80 & T255;
  assign T255 = T78[5];
  assign T256 = T88 & T257;
  assign T257 = T86[5];
  assign T258 = T96 & T259;
  assign T259 = T94[5];
  assign io_dataOut_bits_41 = T261;
  assign T261 = vecOutComb_5[1];
  assign io_dataOut_bits_42 = T262;
  assign T262 = vecOutComb_5[2];
  assign io_dataOut_bits_43 = T263;
  assign T263 = vecOutComb_5[3];
  assign io_dataOut_bits_44 = T264;
  assign T264 = vecOutComb_5[4];
  assign io_dataOut_bits_45 = T265;
  assign T265 = vecOutComb_5[5];
  assign io_dataOut_bits_46 = T266;
  assign T266 = vecOutComb_5[6];
  assign io_dataOut_bits_47 = T267;
  assign T267 = vecOutComb_5[7];
  assign io_dataOut_bits_48 = T268;
  assign T268 = vecOutComb_6[0];
  assign vecOutComb_6 = T269;
  assign T269 = T13 ? R292 : T270;
  assign T270 = T271;
  assign T271 = T290 ? vecInComb_0 : T272;
  assign T272 = T288 ? vecInComb_0 : T273;
  assign T273 = T286 ? vecInComb_0 : T274;
  assign T274 = T284 ? vecInComb_0 : T275;
  assign T275 = T282 ? vecInComb_0 : T276;
  assign T276 = T280 ? vecInComb_0 : T277;
  assign T277 = T278 ? vecInComb_0 : R292;
  assign T278 = T38 & T279;
  assign T279 = T36[6];
  assign T280 = T56 & T281;
  assign T281 = T54[6];
  assign T282 = T64 & T283;
  assign T283 = T62[6];
  assign T284 = T72 & T285;
  assign T285 = T70[6];
  assign T286 = T80 & T287;
  assign T287 = T78[6];
  assign T288 = T88 & T289;
  assign T289 = T86[6];
  assign T290 = T96 & T291;
  assign T291 = T94[6];
  assign io_dataOut_bits_49 = T293;
  assign T293 = vecOutComb_6[1];
  assign io_dataOut_bits_50 = T294;
  assign T294 = vecOutComb_6[2];
  assign io_dataOut_bits_51 = T295;
  assign T295 = vecOutComb_6[3];
  assign io_dataOut_bits_52 = T296;
  assign T296 = vecOutComb_6[4];
  assign io_dataOut_bits_53 = T297;
  assign T297 = vecOutComb_6[5];
  assign io_dataOut_bits_54 = T298;
  assign T298 = vecOutComb_6[6];
  assign io_dataOut_bits_55 = T299;
  assign T299 = vecOutComb_6[7];
  assign io_dataOut_bits_56 = T300;
  assign T300 = vecOutComb_7[0];
  assign vecOutComb_7 = T301;
  assign T301 = T302;
  assign T302 = T331 ? T318 : T303;
  assign T303 = T317 ? T304 : vecInComb_0;
  assign T304 = T316 ? T312 : T305;
  assign T305 = T311 ? T309 : T306;
  assign T306 = T307 ? T110 : T19;
  assign T307 = T308[0];
  assign T308 = 3'h7;
  assign T309 = T310 ? T174 : T142;
  assign T310 = T308[0];
  assign T311 = T308[1];
  assign T312 = T315 ? T270 : T313;
  assign T313 = T314 ? T238 : T206;
  assign T314 = T308[0];
  assign T315 = T308[1];
  assign T316 = T308[2];
  assign T317 = 3'h0 < T11;
  assign T318 = T330 ? T326 : T319;
  assign T319 = T325 ? T323 : T320;
  assign T320 = T321 ? R132 : R100;
  assign T321 = T322[0];
  assign T322 = 3'h7;
  assign T323 = T324 ? R196 : R164;
  assign T324 = T322[0];
  assign T325 = T322[1];
  assign T326 = T329 ? R292 : T327;
  assign T327 = T328 ? R260 : R228;
  assign T328 = T322[0];
  assign T329 = T322[1];
  assign T330 = T322[2];
  assign T331 = T317 & T13;
  assign io_dataOut_bits_57 = T332;
  assign T332 = vecOutComb_7[1];
  assign io_dataOut_bits_58 = T333;
  assign T333 = vecOutComb_7[2];
  assign io_dataOut_bits_59 = T334;
  assign T334 = vecOutComb_7[3];
  assign io_dataOut_bits_60 = T335;
  assign T335 = vecOutComb_7[4];
  assign io_dataOut_bits_61 = T336;
  assign T336 = vecOutComb_7[5];
  assign io_dataOut_bits_62 = T337;
  assign T337 = vecOutComb_7[6];
  assign io_dataOut_bits_63 = T338;
  assign T338 = vecOutComb_7[7];
  assign io_dataOut_valid = T339;
  assign T339 = T13 | T340;
  assign T340 = io_flushed & T341;
  assign T341 = io_dataIn_valid | T342;
  assign T342 = R6 != 3'h0;
  assign io_dataIn_ready = T343;
  assign T343 = T344 | io_flushed;
  assign T344 = T345 ^ 1'h1;
  assign T345 = T4 & io_flush;

  always @(posedge clk) begin
    if(io_flushed) begin
      R1 <= 1'h0;
    end else begin
      R1 <= T3;
    end
    R6 <= T346;
    R100 <= T19;
    R132 <= T110;
    R164 <= T142;
    R196 <= T174;
    R228 <= T206;
    R260 <= T238;
    R292 <= T270;
  end
endmodule

module user_application(input clk, input rst,
    input [31:0] devkit_version,
    input [31:0] hw_time,
    input [2:0] hw_rev,
    input  reg_w_en,
    input [10:0] reg_w_addr,
    input [31:0] reg_w_data,
    input [10:0] reg_r_addr,
    output[31:0] reg_r_data,
    input [15:0] mem_w_en,
    input [18:0] mem_w_addr,
    input [127:0] mem_w_data,
    input  trx0_link_up,
    output[63:0] tx0_data_usr,
    output tx0_sof_usr,
    output tx0_eof_usr,
    output[2:0] tx0_len_usr,
    output tx0_vld_usr,
    input  tx0_ack_usr,
    input [63:0] tx0_data_host,
    input  tx0_sof_host,
    input  tx0_eof_host,
    input [2:0] tx0_len_host,
    input  tx0_vld_host,
    output tx0_ack_host,
    input [63:0] rx0_data_usr,
    input  rx0_sof_usr,
    input  rx0_eof_usr,
    input [2:0] rx0_len_usr,
    input  rx0_vld_usr,
    input  rx0_err_usr,
    input  rx0_pkt_drop_usr,
    input  rx0_crc_fail_usr,
    input [31:0] rx0_timestamp_usr,
    input  rx0_is_vlan_usr,
    input [11:0] rx0_vlan_usr,
    output[63:0] rx0_data_host,
    output rx0_sof_host,
    output rx0_eof_host,
    output[2:0] rx0_len_host,
    output rx0_vld_host,
    output rx0_err_host,
    output rx0_pkt_drop_host,
    output rx0_crc_fail_host,
    output[31:0] rx0_timestamp_host,
    output[7:0] rx0_match_host,
    output[5:0] rx0_buffer_host,
    input  trx1_link_up,
    output[63:0] tx1_data_usr,
    output tx1_sof_usr,
    output tx1_eof_usr,
    output[2:0] tx1_len_usr,
    output tx1_vld_usr,
    input  tx1_ack_usr,
    input [63:0] tx1_data_host,
    input  tx1_sof_host,
    input  tx1_eof_host,
    input [2:0] tx1_len_host,
    input  tx1_vld_host,
    output tx1_ack_host,
    input [63:0] rx1_data_usr,
    input  rx1_sof_usr,
    input  rx1_eof_usr,
    input [2:0] rx1_len_usr,
    input  rx1_vld_usr,
    input  rx1_err_usr,
    input  rx1_pkt_drop_usr,
    input  rx1_crc_fail_usr,
    input [31:0] rx1_timestamp_usr,
    input  rx1_is_vlan_usr,
    input [11:0] rx1_vlan_usr,
    output[63:0] rx1_data_host,
    output rx1_sof_host,
    output rx1_eof_host,
    output[2:0] rx1_len_host,
    output rx1_vld_host,
    output rx1_err_host,
    output rx1_pkt_drop_host,
    output rx1_crc_fail_host,
    output[31:0] rx1_timestamp_host,
    output[7:0] rx1_match_host,
    output[5:0] rx1_buffer_host,
    input  trx2_link_up,
    output[63:0] tx2_data_usr,
    output tx2_sof_usr,
    output tx2_eof_usr,
    output[2:0] tx2_len_usr,
    output tx2_vld_usr,
    input  tx2_ack_usr,
    input [63:0] tx2_data_host,
    input  tx2_sof_host,
    input  tx2_eof_host,
    input [2:0] tx2_len_host,
    input  tx2_vld_host,
    output tx2_ack_host,
    input [63:0] rx2_data_usr,
    input  rx2_sof_usr,
    input  rx2_eof_usr,
    input [2:0] rx2_len_usr,
    input  rx2_vld_usr,
    input  rx2_err_usr,
    input  rx2_pkt_drop_usr,
    input  rx2_crc_fail_usr,
    input [31:0] rx2_timestamp_usr,
    input  rx2_is_vlan_usr,
    input [11:0] rx2_vlan_usr,
    output[63:0] rx2_data_host,
    output rx2_sof_host,
    output rx2_eof_host,
    output[2:0] rx2_len_host,
    output rx2_vld_host,
    output rx2_err_host,
    output rx2_pkt_drop_host,
    output rx2_crc_fail_host,
    output[31:0] rx2_timestamp_host,
    output[7:0] rx2_match_host,
    output[5:0] rx2_buffer_host,
    input  trx3_link_up,
    output[63:0] tx3_data_usr,
    output tx3_sof_usr,
    output tx3_eof_usr,
    output[2:0] tx3_len_usr,
    output tx3_vld_usr,
    input  tx3_ack_usr,
    input [63:0] tx3_data_host,
    input  tx3_sof_host,
    input  tx3_eof_host,
    input [2:0] tx3_len_host,
    input  tx3_vld_host,
    output tx3_ack_host,
    input [63:0] rx3_data_usr,
    input  rx3_sof_usr,
    input  rx3_eof_usr,
    input [2:0] rx3_len_usr,
    input  rx3_vld_usr,
    input  rx3_err_usr,
    input  rx3_pkt_drop_usr,
    input  rx3_crc_fail_usr,
    input [31:0] rx3_timestamp_usr,
    input  rx3_is_vlan_usr,
    input [11:0] rx3_vlan_usr,
    output[63:0] rx3_data_host,
    output rx3_sof_host,
    output rx3_eof_host,
    output[2:0] rx3_len_host,
    output rx3_vld_host,
    output rx3_err_host,
    output rx3_pkt_drop_host,
    output rx3_crc_fail_host,
    output[31:0] rx3_timestamp_host,
    output[7:0] rx3_match_host,
    output[5:0] rx3_buffer_host
);

  wire T836;
  wire T837;
  reg [31:0] controlReg;
  wire[31:0] T1;
  wire[31:0] T838;
  wire[31:0] T2;
  wire T3;
  wire T4;
  wire[15:0] T5;
  wire[3:0] T6;
  wire[3:0] T839;
  wire T0;
  wire fifoDrain;
  wire T7;
  reg  sending;
  wire T840;
  wire T8;
  wire T9;
  wire T10;
  wire T11;
  wire T12;
  wire T13;
  wire eof;
  reg [3:0] segmentCounter;
  wire[3:0] T841;
  wire[3:0] T14;
  wire[3:0] T15;
  wire[3:0] T16;
  wire[3:0] T17;
  wire T18;
  wire T19;
  wire[7:0] bufferByte_0;
  wire[7:0] T842;
  wire[8:0] T20;
  wire[8:0] T21;
  wire[8:0] T22;
  wire[8:0] T843;
  wire T23;
  wire T24;
  reg  buffer_7;
  wire[8:0] T25;
  wire[8:0] T26;
  wire[8:0] T844;
  wire[7:0] T27;
  wire[7:0] T28;
  wire[7:0] T29;
  wire[7:0] T845;
  wire T30;
  wire T31;
  reg  buffer_6;
  wire[7:0] T32;
  wire[7:0] T33;
  wire[7:0] T34;
  wire[7:0] T846;
  wire[6:0] T35;
  wire[6:0] T36;
  wire[6:0] T847;
  wire T37;
  wire T38;
  reg  buffer_5;
  wire T848;
  wire[7:0] T39;
  wire[7:0] T849;
  wire[6:0] T40;
  wire T850;
  wire[7:0] T41;
  wire[7:0] T851;
  wire[5:0] T42;
  wire[5:0] T43;
  wire[5:0] T852;
  wire T44;
  wire T45;
  reg  buffer_4;
  wire[1:0] T853;
  wire T854;
  wire[7:0] T46;
  wire[7:0] T855;
  wire[5:0] T47;
  wire[1:0] T856;
  wire T857;
  wire[7:0] T48;
  wire[7:0] T858;
  wire[4:0] T49;
  wire[4:0] T50;
  wire[4:0] T859;
  wire T51;
  wire T52;
  reg  buffer_3;
  wire[2:0] T860;
  wire T861;
  wire[7:0] T53;
  wire[7:0] T862;
  wire[4:0] T54;
  wire[2:0] T863;
  wire T864;
  wire[7:0] T55;
  wire[7:0] T865;
  wire[3:0] T56;
  wire[3:0] T57;
  wire[3:0] T866;
  wire T58;
  wire T59;
  reg  buffer_2;
  wire[3:0] T867;
  wire T868;
  wire[7:0] T60;
  wire[7:0] T869;
  wire[3:0] T61;
  wire[3:0] T870;
  wire T871;
  wire[7:0] T62;
  wire[7:0] T872;
  wire[2:0] T63;
  wire[2:0] T64;
  wire[2:0] T873;
  wire T65;
  wire T66;
  reg  buffer_1;
  wire[4:0] T874;
  wire T875;
  wire[7:0] T67;
  wire[7:0] T876;
  wire[2:0] T68;
  wire[4:0] T877;
  wire T878;
  wire[7:0] T69;
  wire[7:0] T879;
  wire[1:0] T70;
  wire[1:0] T71;
  wire[1:0] T880;
  wire T72;
  wire T73;
  reg  buffer_0;
  wire[5:0] T881;
  wire T882;
  wire[7:0] T74;
  wire[7:0] T883;
  wire[1:0] T75;
  wire[5:0] T884;
  wire T885;
  wire[7:0] bufferByte_1;
  wire[7:0] T886;
  wire[8:0] T76;
  wire[8:0] T77;
  wire[8:0] T78;
  wire[8:0] T887;
  wire T79;
  wire T80;
  reg  buffer_15;
  wire[8:0] T81;
  wire[8:0] T82;
  wire[8:0] T888;
  wire[7:0] T83;
  wire[7:0] T84;
  wire[7:0] T85;
  wire[7:0] T889;
  wire T86;
  wire T87;
  reg  buffer_14;
  wire[7:0] T88;
  wire[7:0] T89;
  wire[7:0] T90;
  wire[7:0] T890;
  wire[6:0] T91;
  wire[6:0] T92;
  wire[6:0] T891;
  wire T93;
  wire T94;
  reg  buffer_13;
  wire T892;
  wire[7:0] T95;
  wire[7:0] T893;
  wire[6:0] T96;
  wire T894;
  wire[7:0] T97;
  wire[7:0] T895;
  wire[5:0] T98;
  wire[5:0] T99;
  wire[5:0] T896;
  wire T100;
  wire T101;
  reg  buffer_12;
  wire[1:0] T897;
  wire T898;
  wire[7:0] T102;
  wire[7:0] T899;
  wire[5:0] T103;
  wire[1:0] T900;
  wire T901;
  wire[7:0] T104;
  wire[7:0] T902;
  wire[4:0] T105;
  wire[4:0] T106;
  wire[4:0] T903;
  wire T107;
  wire T108;
  reg  buffer_11;
  wire[2:0] T904;
  wire T905;
  wire[7:0] T109;
  wire[7:0] T906;
  wire[4:0] T110;
  wire[2:0] T907;
  wire T908;
  wire[7:0] T111;
  wire[7:0] T909;
  wire[3:0] T112;
  wire[3:0] T113;
  wire[3:0] T910;
  wire T114;
  wire T115;
  reg  buffer_10;
  wire[3:0] T911;
  wire T912;
  wire[7:0] T116;
  wire[7:0] T913;
  wire[3:0] T117;
  wire[3:0] T914;
  wire T915;
  wire[7:0] T118;
  wire[7:0] T916;
  wire[2:0] T119;
  wire[2:0] T120;
  wire[2:0] T917;
  wire T121;
  wire T122;
  reg  buffer_9;
  wire[4:0] T918;
  wire T919;
  wire[7:0] T123;
  wire[7:0] T920;
  wire[2:0] T124;
  wire[4:0] T921;
  wire T922;
  wire[7:0] T125;
  wire[7:0] T923;
  wire[1:0] T126;
  wire[1:0] T127;
  wire[1:0] T924;
  wire T128;
  wire T129;
  reg  buffer_8;
  wire[5:0] T925;
  wire T926;
  wire[7:0] T130;
  wire[7:0] T927;
  wire[1:0] T131;
  wire[5:0] T928;
  wire T929;
  wire[7:0] bufferByte_2;
  wire[7:0] T930;
  wire[8:0] T132;
  wire[8:0] T133;
  wire[8:0] T134;
  wire[8:0] T931;
  wire T135;
  wire T136;
  reg  buffer_23;
  wire[8:0] T137;
  wire[8:0] T138;
  wire[8:0] T932;
  wire[7:0] T139;
  wire[7:0] T140;
  wire[7:0] T141;
  wire[7:0] T933;
  wire T142;
  wire T143;
  reg  buffer_22;
  wire[7:0] T144;
  wire[7:0] T145;
  wire[7:0] T146;
  wire[7:0] T934;
  wire[6:0] T147;
  wire[6:0] T148;
  wire[6:0] T935;
  wire T149;
  wire T150;
  reg  buffer_21;
  wire T936;
  wire[7:0] T151;
  wire[7:0] T937;
  wire[6:0] T152;
  wire T938;
  wire[7:0] T153;
  wire[7:0] T939;
  wire[5:0] T154;
  wire[5:0] T155;
  wire[5:0] T940;
  wire T156;
  wire T157;
  reg  buffer_20;
  wire[1:0] T941;
  wire T942;
  wire[7:0] T158;
  wire[7:0] T943;
  wire[5:0] T159;
  wire[1:0] T944;
  wire T945;
  wire[7:0] T160;
  wire[7:0] T946;
  wire[4:0] T161;
  wire[4:0] T162;
  wire[4:0] T947;
  wire T163;
  wire T164;
  reg  buffer_19;
  wire[2:0] T948;
  wire T949;
  wire[7:0] T165;
  wire[7:0] T950;
  wire[4:0] T166;
  wire[2:0] T951;
  wire T952;
  wire[7:0] T167;
  wire[7:0] T953;
  wire[3:0] T168;
  wire[3:0] T169;
  wire[3:0] T954;
  wire T170;
  wire T171;
  reg  buffer_18;
  wire[3:0] T955;
  wire T956;
  wire[7:0] T172;
  wire[7:0] T957;
  wire[3:0] T173;
  wire[3:0] T958;
  wire T959;
  wire[7:0] T174;
  wire[7:0] T960;
  wire[2:0] T175;
  wire[2:0] T176;
  wire[2:0] T961;
  wire T177;
  wire T178;
  reg  buffer_17;
  wire[4:0] T962;
  wire T963;
  wire[7:0] T179;
  wire[7:0] T964;
  wire[2:0] T180;
  wire[4:0] T965;
  wire T966;
  wire[7:0] T181;
  wire[7:0] T967;
  wire[1:0] T182;
  wire[1:0] T183;
  wire[1:0] T968;
  wire T184;
  wire T185;
  reg  buffer_16;
  wire[5:0] T969;
  wire T970;
  wire[7:0] T186;
  wire[7:0] T971;
  wire[1:0] T187;
  wire[5:0] T972;
  wire T973;
  wire[7:0] bufferByte_3;
  wire[7:0] T974;
  wire[8:0] T188;
  wire[8:0] T189;
  wire[8:0] T190;
  wire[8:0] T975;
  wire T191;
  wire T192;
  reg  buffer_31;
  wire[8:0] T193;
  wire[8:0] T194;
  wire[8:0] T976;
  wire[7:0] T195;
  wire[7:0] T196;
  wire[7:0] T197;
  wire[7:0] T977;
  wire T198;
  wire T199;
  reg  buffer_30;
  wire[7:0] T200;
  wire[7:0] T201;
  wire[7:0] T202;
  wire[7:0] T978;
  wire[6:0] T203;
  wire[6:0] T204;
  wire[6:0] T979;
  wire T205;
  wire T206;
  reg  buffer_29;
  wire T980;
  wire[7:0] T207;
  wire[7:0] T981;
  wire[6:0] T208;
  wire T982;
  wire[7:0] T209;
  wire[7:0] T983;
  wire[5:0] T210;
  wire[5:0] T211;
  wire[5:0] T984;
  wire T212;
  wire T213;
  reg  buffer_28;
  wire[1:0] T985;
  wire T986;
  wire[7:0] T214;
  wire[7:0] T987;
  wire[5:0] T215;
  wire[1:0] T988;
  wire T989;
  wire[7:0] T216;
  wire[7:0] T990;
  wire[4:0] T217;
  wire[4:0] T218;
  wire[4:0] T991;
  wire T219;
  wire T220;
  reg  buffer_27;
  wire[2:0] T992;
  wire T993;
  wire[7:0] T221;
  wire[7:0] T994;
  wire[4:0] T222;
  wire[2:0] T995;
  wire T996;
  wire[7:0] T223;
  wire[7:0] T997;
  wire[3:0] T224;
  wire[3:0] T225;
  wire[3:0] T998;
  wire T226;
  wire T227;
  reg  buffer_26;
  wire[3:0] T999;
  wire T1000;
  wire[7:0] T228;
  wire[7:0] T1001;
  wire[3:0] T229;
  wire[3:0] T1002;
  wire T1003;
  wire[7:0] T230;
  wire[7:0] T1004;
  wire[2:0] T231;
  wire[2:0] T232;
  wire[2:0] T1005;
  wire T233;
  wire T234;
  reg  buffer_25;
  wire[4:0] T1006;
  wire T1007;
  wire[7:0] T235;
  wire[7:0] T1008;
  wire[2:0] T236;
  wire[4:0] T1009;
  wire T1010;
  wire[7:0] T237;
  wire[7:0] T1011;
  wire[1:0] T238;
  wire[1:0] T239;
  wire[1:0] T1012;
  wire T240;
  wire T241;
  reg  buffer_24;
  wire[5:0] T1013;
  wire T1014;
  wire[7:0] T242;
  wire[7:0] T1015;
  wire[1:0] T243;
  wire[5:0] T1016;
  wire T1017;
  wire[7:0] bufferByte_4;
  wire[7:0] T1018;
  wire[8:0] T244;
  wire[8:0] T245;
  wire[8:0] T246;
  wire[8:0] T1019;
  wire T247;
  wire T248;
  reg  buffer_39;
  wire[8:0] T249;
  wire[8:0] T250;
  wire[8:0] T1020;
  wire[7:0] T251;
  wire[7:0] T252;
  wire[7:0] T253;
  wire[7:0] T1021;
  wire T254;
  wire T255;
  reg  buffer_38;
  wire[7:0] T256;
  wire[7:0] T257;
  wire[7:0] T258;
  wire[7:0] T1022;
  wire[6:0] T259;
  wire[6:0] T260;
  wire[6:0] T1023;
  wire T261;
  wire T262;
  reg  buffer_37;
  wire T1024;
  wire[7:0] T263;
  wire[7:0] T1025;
  wire[6:0] T264;
  wire T1026;
  wire[7:0] T265;
  wire[7:0] T1027;
  wire[5:0] T266;
  wire[5:0] T267;
  wire[5:0] T1028;
  wire T268;
  wire T269;
  reg  buffer_36;
  wire[1:0] T1029;
  wire T1030;
  wire[7:0] T270;
  wire[7:0] T1031;
  wire[5:0] T271;
  wire[1:0] T1032;
  wire T1033;
  wire[7:0] T272;
  wire[7:0] T1034;
  wire[4:0] T273;
  wire[4:0] T274;
  wire[4:0] T1035;
  wire T275;
  wire T276;
  reg  buffer_35;
  wire[2:0] T1036;
  wire T1037;
  wire[7:0] T277;
  wire[7:0] T1038;
  wire[4:0] T278;
  wire[2:0] T1039;
  wire T1040;
  wire[7:0] T279;
  wire[7:0] T1041;
  wire[3:0] T280;
  wire[3:0] T281;
  wire[3:0] T1042;
  wire T282;
  wire T283;
  reg  buffer_34;
  wire[3:0] T1043;
  wire T1044;
  wire[7:0] T284;
  wire[7:0] T1045;
  wire[3:0] T285;
  wire[3:0] T1046;
  wire T1047;
  wire[7:0] T286;
  wire[7:0] T1048;
  wire[2:0] T287;
  wire[2:0] T288;
  wire[2:0] T1049;
  wire T289;
  wire T290;
  reg  buffer_33;
  wire[4:0] T1050;
  wire T1051;
  wire[7:0] T291;
  wire[7:0] T1052;
  wire[2:0] T292;
  wire[4:0] T1053;
  wire T1054;
  wire[7:0] T293;
  wire[7:0] T1055;
  wire[1:0] T294;
  wire[1:0] T295;
  wire[1:0] T1056;
  wire T296;
  wire T297;
  reg  buffer_32;
  wire[5:0] T1057;
  wire T1058;
  wire[7:0] T298;
  wire[7:0] T1059;
  wire[1:0] T299;
  wire[5:0] T1060;
  wire T1061;
  wire[7:0] bufferByte_5;
  wire[7:0] T1062;
  wire[8:0] T300;
  wire[8:0] T301;
  wire[8:0] T302;
  wire[8:0] T1063;
  wire T303;
  wire T304;
  reg  buffer_47;
  wire[8:0] T305;
  wire[8:0] T306;
  wire[8:0] T1064;
  wire[7:0] T307;
  wire[7:0] T308;
  wire[7:0] T309;
  wire[7:0] T1065;
  wire T310;
  wire T311;
  reg  buffer_46;
  wire[7:0] T312;
  wire[7:0] T313;
  wire[7:0] T314;
  wire[7:0] T1066;
  wire[6:0] T315;
  wire[6:0] T316;
  wire[6:0] T1067;
  wire T317;
  wire T318;
  reg  buffer_45;
  wire T1068;
  wire[7:0] T319;
  wire[7:0] T1069;
  wire[6:0] T320;
  wire T1070;
  wire[7:0] T321;
  wire[7:0] T1071;
  wire[5:0] T322;
  wire[5:0] T323;
  wire[5:0] T1072;
  wire T324;
  wire T325;
  reg  buffer_44;
  wire[1:0] T1073;
  wire T1074;
  wire[7:0] T326;
  wire[7:0] T1075;
  wire[5:0] T327;
  wire[1:0] T1076;
  wire T1077;
  wire[7:0] T328;
  wire[7:0] T1078;
  wire[4:0] T329;
  wire[4:0] T330;
  wire[4:0] T1079;
  wire T331;
  wire T332;
  reg  buffer_43;
  wire[2:0] T1080;
  wire T1081;
  wire[7:0] T333;
  wire[7:0] T1082;
  wire[4:0] T334;
  wire[2:0] T1083;
  wire T1084;
  wire[7:0] T335;
  wire[7:0] T1085;
  wire[3:0] T336;
  wire[3:0] T337;
  wire[3:0] T1086;
  wire T338;
  wire T339;
  reg  buffer_42;
  wire[3:0] T1087;
  wire T1088;
  wire[7:0] T340;
  wire[7:0] T1089;
  wire[3:0] T341;
  wire[3:0] T1090;
  wire T1091;
  wire[7:0] T342;
  wire[7:0] T1092;
  wire[2:0] T343;
  wire[2:0] T344;
  wire[2:0] T1093;
  wire T345;
  wire T346;
  reg  buffer_41;
  wire[4:0] T1094;
  wire T1095;
  wire[7:0] T347;
  wire[7:0] T1096;
  wire[2:0] T348;
  wire[4:0] T1097;
  wire T1098;
  wire[7:0] T349;
  wire[7:0] T1099;
  wire[1:0] T350;
  wire[1:0] T351;
  wire[1:0] T1100;
  wire T352;
  wire T353;
  reg  buffer_40;
  wire[5:0] T1101;
  wire T1102;
  wire[7:0] T354;
  wire[7:0] T1103;
  wire[1:0] T355;
  wire[5:0] T1104;
  wire T1105;
  wire[7:0] bufferByte_6;
  wire[7:0] T1106;
  wire[8:0] T356;
  wire[8:0] T357;
  wire[8:0] T358;
  wire[8:0] T1107;
  wire T359;
  wire T360;
  reg  buffer_55;
  wire[8:0] T361;
  wire[8:0] T362;
  wire[8:0] T1108;
  wire[7:0] T363;
  wire[7:0] T364;
  wire[7:0] T365;
  wire[7:0] T1109;
  wire T366;
  wire T367;
  reg  buffer_54;
  wire[7:0] T368;
  wire[7:0] T369;
  wire[7:0] T370;
  wire[7:0] T1110;
  wire[6:0] T371;
  wire[6:0] T372;
  wire[6:0] T1111;
  wire T373;
  wire T374;
  reg  buffer_53;
  wire T1112;
  wire[7:0] T375;
  wire[7:0] T1113;
  wire[6:0] T376;
  wire T1114;
  wire[7:0] T377;
  wire[7:0] T1115;
  wire[5:0] T378;
  wire[5:0] T379;
  wire[5:0] T1116;
  wire T380;
  wire T381;
  reg  buffer_52;
  wire[1:0] T1117;
  wire T1118;
  wire[7:0] T382;
  wire[7:0] T1119;
  wire[5:0] T383;
  wire[1:0] T1120;
  wire T1121;
  wire[7:0] T384;
  wire[7:0] T1122;
  wire[4:0] T385;
  wire[4:0] T386;
  wire[4:0] T1123;
  wire T387;
  wire T388;
  reg  buffer_51;
  wire[2:0] T1124;
  wire T1125;
  wire[7:0] T389;
  wire[7:0] T1126;
  wire[4:0] T390;
  wire[2:0] T1127;
  wire T1128;
  wire[7:0] T391;
  wire[7:0] T1129;
  wire[3:0] T392;
  wire[3:0] T393;
  wire[3:0] T1130;
  wire T394;
  wire T395;
  reg  buffer_50;
  wire[3:0] T1131;
  wire T1132;
  wire[7:0] T396;
  wire[7:0] T1133;
  wire[3:0] T397;
  wire[3:0] T1134;
  wire T1135;
  wire[7:0] T398;
  wire[7:0] T1136;
  wire[2:0] T399;
  wire[2:0] T400;
  wire[2:0] T1137;
  wire T401;
  wire T402;
  reg  buffer_49;
  wire[4:0] T1138;
  wire T1139;
  wire[7:0] T403;
  wire[7:0] T1140;
  wire[2:0] T404;
  wire[4:0] T1141;
  wire T1142;
  wire[7:0] T405;
  wire[7:0] T1143;
  wire[1:0] T406;
  wire[1:0] T407;
  wire[1:0] T1144;
  wire T408;
  wire T409;
  reg  buffer_48;
  wire[5:0] T1145;
  wire T1146;
  wire[7:0] T410;
  wire[7:0] T1147;
  wire[1:0] T411;
  wire[5:0] T1148;
  wire T1149;
  wire[7:0] bufferByte_7;
  wire[7:0] T1150;
  wire[8:0] T412;
  wire[8:0] T413;
  wire[8:0] T414;
  wire[8:0] T1151;
  wire T415;
  wire T416;
  reg  buffer_63;
  wire[8:0] T417;
  wire[8:0] T418;
  wire[8:0] T1152;
  wire[7:0] T419;
  wire[7:0] T420;
  wire[7:0] T421;
  wire[7:0] T1153;
  wire T422;
  wire T423;
  reg  buffer_62;
  wire[7:0] T424;
  wire[7:0] T425;
  wire[7:0] T426;
  wire[7:0] T1154;
  wire[6:0] T427;
  wire[6:0] T428;
  wire[6:0] T1155;
  wire T429;
  wire T430;
  reg  buffer_61;
  wire T1156;
  wire[7:0] T431;
  wire[7:0] T1157;
  wire[6:0] T432;
  wire T1158;
  wire[7:0] T433;
  wire[7:0] T1159;
  wire[5:0] T434;
  wire[5:0] T435;
  wire[5:0] T1160;
  wire T436;
  wire T437;
  reg  buffer_60;
  wire[1:0] T1161;
  wire T1162;
  wire[7:0] T438;
  wire[7:0] T1163;
  wire[5:0] T439;
  wire[1:0] T1164;
  wire T1165;
  wire[7:0] T440;
  wire[7:0] T1166;
  wire[4:0] T441;
  wire[4:0] T442;
  wire[4:0] T1167;
  wire T443;
  wire T444;
  reg  buffer_59;
  wire[2:0] T1168;
  wire T1169;
  wire[7:0] T445;
  wire[7:0] T1170;
  wire[4:0] T446;
  wire[2:0] T1171;
  wire T1172;
  wire[7:0] T447;
  wire[7:0] T1173;
  wire[3:0] T448;
  wire[3:0] T449;
  wire[3:0] T1174;
  wire T450;
  wire T451;
  reg  buffer_58;
  wire[3:0] T1175;
  wire T1176;
  wire[7:0] T452;
  wire[7:0] T1177;
  wire[3:0] T453;
  wire[3:0] T1178;
  wire T1179;
  wire[7:0] T454;
  wire[7:0] T1180;
  wire[2:0] T455;
  wire[2:0] T456;
  wire[2:0] T1181;
  wire T457;
  wire T458;
  reg  buffer_57;
  wire[4:0] T1182;
  wire T1183;
  wire[7:0] T459;
  wire[7:0] T1184;
  wire[2:0] T460;
  wire[4:0] T1185;
  wire T1186;
  wire[7:0] T461;
  wire[7:0] T1187;
  wire[1:0] T462;
  wire[1:0] T463;
  wire[1:0] T1188;
  wire T464;
  wire T465;
  reg  buffer_56;
  wire[5:0] T1189;
  wire T1190;
  wire[7:0] T466;
  wire[7:0] T1191;
  wire[1:0] T467;
  wire[5:0] T1192;
  wire T1193;
  reg  bufferVld;
  wire flush;
  wire T468;
  wire T469;
  reg [6:0] buffCount;
  wire[6:0] T1194;
  wire[6:0] T470;
  wire[6:0] T471;
  wire[6:0] T472;
  wire T473;
  wire vecDataOut_0;
  wire T474;
  wire vecDataOut_1;
  wire T475;
  wire vecDataOut_2;
  wire T476;
  wire vecDataOut_3;
  wire T477;
  wire vecDataOut_4;
  wire T478;
  wire vecDataOut_5;
  wire T479;
  wire vecDataOut_6;
  wire T480;
  wire vecDataOut_7;
  wire T481;
  wire[7:0] T482;
  wire[7:0] T483;
  wire[7:0] T484;
  wire[7:0] T485;
  wire[7:0] T486;
  wire[7:0] T487;
  wire[7:0] T488;
  wire[7:0] T489;
  wire[127:0] T1195;
  wire[128:0] T490;
  wire[128:0] T1196;
  wire[127:0] T491;
  wire[7:0] T492;
  wire[7:0] T493;
  wire[127:0] T494;
  reg [127:0] userMem [127:0];
  wire[127:0] T495;
  wire[127:0] T1197;
  wire[127:0] T1198;
  wire[127:0] T1199;
  wire[127:0] T1200;
  wire[15:0] T1201;
  wire[127:0] T1202;
  wire[127:0] T1203;
  wire[63:0] T1204;
  wire[31:0] T1205;
  wire[15:0] T1206;
  wire[7:0] T1207;
  wire[127:0] T1208;
  wire[6:0] T1209;
  wire[7:0] T1210;
  wire[15:0] T1211;
  wire[7:0] T1212;
  wire[7:0] T1213;
  wire[31:0] T1214;
  wire[15:0] T1215;
  wire[7:0] T1216;
  wire[7:0] T1217;
  wire[15:0] T1218;
  wire[7:0] T1219;
  wire[7:0] T1220;
  wire[63:0] T1221;
  wire[31:0] T1222;
  wire[15:0] T1223;
  wire[7:0] T1224;
  wire[7:0] T1225;
  wire[15:0] T1226;
  wire[7:0] T1227;
  wire[7:0] T1228;
  wire[31:0] T1229;
  wire[15:0] T1230;
  wire[7:0] T1231;
  wire[7:0] T1232;
  wire[15:0] T1233;
  wire[7:0] T1234;
  wire[7:0] T1235;
  wire[127:0] T1236;
  wire[127:0] T1237;
  wire[127:0] T496;
  wire[127:0] T497;
  wire[63:0] T498;
  wire[31:0] T499;
  wire[15:0] T500;
  wire[7:0] memWData_0;
  wire[7:0] T501;
  wire[7:0] memWData_1;
  wire[7:0] T502;
  wire[15:0] T503;
  wire[7:0] memWData_2;
  wire[7:0] T504;
  wire[7:0] memWData_3;
  wire[7:0] T505;
  wire[31:0] T506;
  wire[15:0] T507;
  wire[7:0] memWData_4;
  wire[7:0] T508;
  wire[7:0] memWData_5;
  wire[7:0] T509;
  wire[15:0] T510;
  wire[7:0] memWData_6;
  wire[7:0] T511;
  wire[7:0] memWData_7;
  wire[7:0] T512;
  wire[63:0] T513;
  wire[31:0] T514;
  wire[15:0] T515;
  wire[7:0] memWData_8;
  wire[7:0] T516;
  wire[7:0] memWData_9;
  wire[7:0] T517;
  wire[15:0] T518;
  wire[7:0] memWData_10;
  wire[7:0] T519;
  wire[7:0] memWData_11;
  wire[7:0] T520;
  wire[31:0] T521;
  wire[15:0] T522;
  wire[7:0] memWData_12;
  wire[7:0] T523;
  wire[7:0] memWData_13;
  wire[7:0] T524;
  wire[15:0] T525;
  wire[7:0] memWData_14;
  wire[7:0] T526;
  wire[7:0] memWData_15;
  wire[7:0] T527;
  wire[6:0] T1238;
  wire[6:0] T1239;
  wire[128:0] T528;
  wire[128:0] T529;
  wire[128:0] T530;
  wire[128:0] T1240;
  wire[127:0] T531;
  wire[127:0] T1241;
  wire[119:0] T532;
  wire[7:0] T533;
  wire[7:0] T534;
  wire[127:0] T535;
  wire[127:0] T1242;
  wire[120:0] T536;
  wire[120:0] T537;
  wire[6:0] T1243;
  wire T1244;
  wire[127:0] T538;
  wire[127:0] T1245;
  wire[111:0] T539;
  wire[7:0] T540;
  wire[7:0] T541;
  wire[127:0] T542;
  wire[127:0] T1246;
  wire[112:0] T543;
  wire[112:0] T544;
  wire[14:0] T1247;
  wire T1248;
  wire[127:0] T545;
  wire[127:0] T1249;
  wire[103:0] T546;
  wire[7:0] T547;
  wire[7:0] T548;
  wire[127:0] T549;
  wire[127:0] T1250;
  wire[104:0] T550;
  wire[104:0] T551;
  wire[22:0] T1251;
  wire T1252;
  wire[127:0] T552;
  wire[127:0] T1253;
  wire[95:0] T553;
  wire[7:0] T554;
  wire[7:0] T555;
  wire[127:0] T556;
  wire[127:0] T1254;
  wire[96:0] T557;
  wire[96:0] T558;
  wire[30:0] T1255;
  wire T1256;
  wire[127:0] T559;
  wire[127:0] T1257;
  wire[87:0] T560;
  wire[7:0] T561;
  wire[7:0] T562;
  wire[127:0] T563;
  wire[127:0] T1258;
  wire[88:0] T564;
  wire[88:0] T565;
  wire[38:0] T1259;
  wire T1260;
  wire[127:0] T566;
  wire[127:0] T1261;
  wire[79:0] T567;
  wire[7:0] T568;
  wire[7:0] T569;
  wire[127:0] T570;
  wire[127:0] T1262;
  wire[80:0] T571;
  wire[80:0] T572;
  wire[46:0] T1263;
  wire T1264;
  wire[127:0] T573;
  wire[127:0] T1265;
  wire[71:0] T574;
  wire[7:0] T575;
  wire[7:0] T576;
  wire[127:0] T577;
  wire[127:0] T1266;
  wire[72:0] T578;
  wire[72:0] T579;
  wire[54:0] T1267;
  wire T1268;
  wire[127:0] T580;
  wire[127:0] T1269;
  wire[63:0] T581;
  wire[7:0] T582;
  wire[7:0] T583;
  wire[127:0] T584;
  wire[127:0] T1270;
  wire[64:0] T585;
  wire[64:0] T586;
  wire[62:0] T1271;
  wire T1272;
  wire[127:0] T587;
  wire[127:0] T1273;
  wire[55:0] T588;
  wire[7:0] T589;
  wire[7:0] T590;
  wire[127:0] T591;
  wire[127:0] T1274;
  wire[56:0] T592;
  wire[56:0] T593;
  wire[70:0] T1275;
  wire T1276;
  wire[127:0] T594;
  wire[127:0] T1277;
  wire[47:0] T595;
  wire[7:0] T596;
  wire[7:0] T597;
  wire[127:0] T598;
  wire[127:0] T1278;
  wire[48:0] T599;
  wire[48:0] T600;
  wire[78:0] T1279;
  wire T1280;
  wire[127:0] T601;
  wire[127:0] T1281;
  wire[39:0] T602;
  wire[7:0] T603;
  wire[7:0] T604;
  wire[127:0] T605;
  wire[127:0] T1282;
  wire[40:0] T606;
  wire[40:0] T607;
  wire[86:0] T1283;
  wire T1284;
  wire[127:0] T608;
  wire[127:0] T1285;
  wire[31:0] T609;
  wire[7:0] T610;
  wire[7:0] T611;
  wire[127:0] T612;
  wire[127:0] T1286;
  wire[32:0] T613;
  wire[32:0] T614;
  wire[94:0] T1287;
  wire T1288;
  wire[127:0] T615;
  wire[127:0] T1289;
  wire[23:0] T616;
  wire[7:0] T617;
  wire[7:0] T618;
  wire[127:0] T619;
  wire[127:0] T1290;
  wire[24:0] T620;
  wire[24:0] T621;
  wire[102:0] T1291;
  wire T1292;
  wire[127:0] T622;
  wire[127:0] T1293;
  wire[15:0] T623;
  wire[7:0] T624;
  wire[7:0] T625;
  wire[127:0] T626;
  wire[127:0] T1294;
  wire[16:0] T627;
  wire[16:0] T628;
  wire[110:0] T1295;
  wire T1296;
  wire[127:0] T629;
  wire[127:0] T1297;
  wire[7:0] T630;
  wire[7:0] T631;
  wire[7:0] T632;
  wire[127:0] T633;
  wire[127:0] T1298;
  wire[8:0] T634;
  wire[8:0] T635;
  wire[118:0] T1299;
  wire T1300;
  reg [31:0] regIntR_0;
  wire[31:0] T636;
  wire[31:0] T1301;
  wire[31:0] T637;
  wire T638;
  wire T639;
  reg [31:0] regIntR_1;
  wire[31:0] T640;
  wire[31:0] T1302;
  wire[31:0] T641;
  wire T642;
  wire T643;
  reg [31:0] regIntR_2;
  wire[31:0] T644;
  wire[31:0] T1303;
  wire[31:0] T645;
  wire T646;
  wire T647;
  reg [31:0] regIntR_3;
  wire[31:0] T648;
  wire[31:0] T1304;
  wire[31:0] T649;
  wire T650;
  wire T651;
  reg [31:0] regIntR_4;
  wire[31:0] T652;
  wire[31:0] T1305;
  wire[31:0] T653;
  wire T654;
  wire T655;
  reg [31:0] regIntR_5;
  wire[31:0] T656;
  wire[31:0] T1306;
  wire[31:0] T657;
  wire T658;
  wire T659;
  reg [31:0] regIntR_6;
  wire[31:0] T660;
  wire[31:0] T1307;
  wire[31:0] T661;
  wire T662;
  wire T663;
  reg [31:0] regIntR_7;
  wire[31:0] T664;
  wire[31:0] T1308;
  wire[31:0] T665;
  wire T666;
  wire T667;
  reg [31:0] regIntR_8;
  wire[31:0] T668;
  wire[31:0] T1309;
  wire[31:0] T669;
  wire T670;
  wire T671;
  reg [31:0] regIntR_9;
  wire[31:0] T672;
  wire[31:0] T1310;
  wire[31:0] T673;
  wire T674;
  wire T675;
  reg [31:0] regIntR_10;
  wire[31:0] T676;
  wire[31:0] T1311;
  wire[31:0] T677;
  wire T678;
  wire T679;
  reg [31:0] regIntR_11;
  wire[31:0] T680;
  wire[31:0] T1312;
  wire[31:0] T681;
  wire T682;
  wire T683;
  reg [31:0] regIntR_12;
  wire[31:0] T684;
  wire[31:0] T1313;
  wire[31:0] T685;
  wire T686;
  wire T687;
  reg [31:0] regIntR_13;
  wire[31:0] T688;
  wire[31:0] T1314;
  wire[31:0] T689;
  wire T690;
  wire T691;
  reg [31:0] regIntR_14;
  wire[31:0] T692;
  wire[31:0] T1315;
  wire[31:0] T693;
  wire T694;
  wire T695;
  wire T696;
  wire[2:0] len;
  wire[2:0] T697;
  wire T698;
  wire T699;
  wire sof;
  wire[63:0] tx1Output;
  wire[63:0] T1316;
  wire[64:0] T700;
  wire[64:0] T1317;
  wire[63:0] T701;
  wire[7:0] T702;
  wire[64:0] T703;
  wire[64:0] T704;
  wire[64:0] T705;
  wire[64:0] T1318;
  wire[63:0] T706;
  wire[63:0] T1319;
  wire[55:0] T707;
  wire[7:0] T708;
  wire[63:0] T709;
  wire[63:0] T1320;
  wire[56:0] T710;
  wire[56:0] T711;
  wire[6:0] T1321;
  wire T1322;
  wire[63:0] T712;
  wire[63:0] T1323;
  wire[47:0] T713;
  wire[7:0] T714;
  wire[63:0] T715;
  wire[63:0] T1324;
  wire[48:0] T716;
  wire[48:0] T717;
  wire[14:0] T1325;
  wire T1326;
  wire[63:0] T718;
  wire[63:0] T1327;
  wire[39:0] T719;
  wire[7:0] T720;
  wire[63:0] T721;
  wire[63:0] T1328;
  wire[40:0] T722;
  wire[40:0] T723;
  wire[22:0] T1329;
  wire T1330;
  wire[63:0] T724;
  wire[63:0] T1331;
  wire[31:0] T725;
  wire[7:0] T726;
  wire[63:0] T727;
  wire[63:0] T1332;
  wire[32:0] T728;
  wire[32:0] T729;
  wire[30:0] T1333;
  wire T1334;
  wire[63:0] T730;
  wire[63:0] T1335;
  wire[23:0] T731;
  wire[7:0] T732;
  wire[63:0] T733;
  wire[63:0] T1336;
  wire[24:0] T734;
  wire[24:0] T735;
  wire[38:0] T1337;
  wire T1338;
  wire[63:0] T736;
  wire[63:0] T1339;
  wire[15:0] T737;
  wire[7:0] T738;
  wire[63:0] T739;
  wire[63:0] T1340;
  wire[16:0] T740;
  wire[16:0] T741;
  wire[46:0] T1341;
  wire T1342;
  wire[63:0] T742;
  wire[63:0] T1343;
  wire[7:0] T743;
  wire[7:0] T744;
  wire[63:0] T745;
  wire[63:0] T1344;
  wire[8:0] T746;
  wire[8:0] T747;
  wire[54:0] T1345;
  wire T1346;
  wire[31:0] T748;
  wire[31:0] T749;
  wire[31:0] T750;
  wire[31:0] T751;
  reg [31:0] regIntW_0;
  wire[31:0] T752;
  wire[31:0] T1347;
  wire[31:0] T753;
  reg [31:0] regIntW_1;
  wire[31:0] T754;
  wire[31:0] T1348;
  wire[31:0] T755;
  wire T756;
  wire[3:0] T757;
  wire[3:0] T1349;
  wire[31:0] T758;
  reg [31:0] regIntW_2;
  wire[31:0] T759;
  wire[31:0] T1350;
  wire[31:0] T760;
  reg [31:0] regIntW_3;
  wire[31:0] T761;
  wire[31:0] T1351;
  wire[31:0] T762;
  wire T763;
  wire T764;
  wire[31:0] T765;
  wire[31:0] T766;
  reg [31:0] regIntW_4;
  wire[31:0] T767;
  wire[31:0] T1352;
  wire[31:0] T768;
  reg [31:0] regIntW_5;
  wire[31:0] T769;
  wire[31:0] T1353;
  wire[31:0] T770;
  wire T771;
  wire[31:0] T772;
  reg [31:0] regIntW_6;
  wire[31:0] T773;
  wire[31:0] T1354;
  wire[31:0] T774;
  reg [31:0] regIntW_7;
  wire[31:0] T775;
  wire[31:0] T1355;
  wire[31:0] T776;
  wire T777;
  wire T778;
  wire T779;
  wire[31:0] T780;
  wire[31:0] T781;
  wire[31:0] T782;
  reg [31:0] regIntW_8;
  wire[31:0] T783;
  wire[31:0] T1356;
  wire[31:0] T784;
  reg [31:0] regIntW_9;
  wire[31:0] T785;
  wire[31:0] T1357;
  wire[31:0] T786;
  wire T787;
  wire[31:0] T788;
  reg [31:0] regIntW_10;
  wire[31:0] T789;
  wire[31:0] T1358;
  wire[31:0] T790;
  reg [31:0] regIntW_11;
  wire[31:0] T791;
  wire[31:0] T1359;
  wire[31:0] T792;
  wire T793;
  wire T794;
  wire[31:0] T795;
  wire[31:0] T796;
  reg [31:0] regIntW_12;
  wire[31:0] T797;
  wire[31:0] T1360;
  wire[31:0] T798;
  reg [31:0] regIntW_13;
  wire[31:0] T799;
  wire[31:0] T1361;
  wire[31:0] T800;
  wire T801;
  wire[31:0] T802;
  reg [31:0] regIntW_14;
  wire[31:0] T803;
  wire[31:0] T1362;
  wire[31:0] T804;
  reg [31:0] regIntW_15;
  wire[31:0] T805;
  wire[31:0] T1363;
  wire[31:0] T1364;
  wire[22:0] T806;
  wire[6:0] error;
  wire[6:0] T807;
  reg  userErr;
  wire T1365;
  wire T808;
  wire T809;
  wire errRst;
  wire[5:0] T810;
  reg  dirOutFull;
  wire T1366;
  wire T811;
  wire T812;
  wire T813;
  wire[4:0] T814;
  reg  txFifoFull;
  wire T1367;
  wire T815;
  wire T816;
  wire T817;
  wire[3:0] T818;
  reg  fifoFull;
  wire T1368;
  wire T819;
  wire T820;
  wire T821;
  wire[2:0] T822;
  reg  crcFail;
  wire T1369;
  wire T823;
  wire T824;
  wire[1:0] T825;
  reg  pktDrop;
  wire T1370;
  wire T826;
  wire T827;
  reg  rx1Err;
  wire T1371;
  wire T828;
  wire T829;
  wire[15:0] T830;
  wire[11:0] T831;
  wire T832;
  wire T833;
  wire T834;
  wire T835;
  wire[63:0] stripper_io_out_data;
  wire[2:0] stripper_io_out_len;
  wire stripper_io_out_vld;
  wire[7:0] combiner_io_dataOut_7;
  wire[7:0] combiner_io_dataOut_6;
  wire[7:0] combiner_io_dataOut_5;
  wire[7:0] combiner_io_dataOut_4;
  wire[7:0] combiner_io_dataOut_3;
  wire[7:0] combiner_io_dataOut_2;
  wire[7:0] combiner_io_dataOut_1;
  wire[7:0] combiner_io_dataOut_0;
  wire combiner_io_vldOut;
  wire fifo_io_enq_ready;
  wire fifo_io_deq_valid;
  wire[7:0] fifo_io_deq_bits_7;
  wire[7:0] fifo_io_deq_bits_6;
  wire[7:0] fifo_io_deq_bits_5;
  wire[7:0] fifo_io_deq_bits_4;
  wire[7:0] fifo_io_deq_bits_3;
  wire[7:0] fifo_io_deq_bits_2;
  wire[7:0] fifo_io_deq_bits_1;
  wire[7:0] fifo_io_deq_bits_0;
  wire directOutputFifo_io_enq_ready;
  wire directOutputFifo_io_deq_valid;
  wire directOutputFifo_io_deq_bits_7;
  wire directOutputFifo_io_deq_bits_6;
  wire directOutputFifo_io_deq_bits_5;
  wire directOutputFifo_io_deq_bits_4;
  wire directOutputFifo_io_deq_bits_3;
  wire directOutputFifo_io_deq_bits_2;
  wire directOutputFifo_io_deq_bits_1;
  wire directOutputFifo_io_deq_bits_0;
  wire[5:0] directOutputFifo_io_count;
  wire outToBuffer_io_dataIn_ready;
  wire outToBuffer_io_dataOut_valid;
  wire outToBuffer_io_dataOut_bits_63;
  wire outToBuffer_io_dataOut_bits_62;
  wire outToBuffer_io_dataOut_bits_61;
  wire outToBuffer_io_dataOut_bits_60;
  wire outToBuffer_io_dataOut_bits_59;
  wire outToBuffer_io_dataOut_bits_58;
  wire outToBuffer_io_dataOut_bits_57;
  wire outToBuffer_io_dataOut_bits_56;
  wire outToBuffer_io_dataOut_bits_55;
  wire outToBuffer_io_dataOut_bits_54;
  wire outToBuffer_io_dataOut_bits_53;
  wire outToBuffer_io_dataOut_bits_52;
  wire outToBuffer_io_dataOut_bits_51;
  wire outToBuffer_io_dataOut_bits_50;
  wire outToBuffer_io_dataOut_bits_49;
  wire outToBuffer_io_dataOut_bits_48;
  wire outToBuffer_io_dataOut_bits_47;
  wire outToBuffer_io_dataOut_bits_46;
  wire outToBuffer_io_dataOut_bits_45;
  wire outToBuffer_io_dataOut_bits_44;
  wire outToBuffer_io_dataOut_bits_43;
  wire outToBuffer_io_dataOut_bits_42;
  wire outToBuffer_io_dataOut_bits_41;
  wire outToBuffer_io_dataOut_bits_40;
  wire outToBuffer_io_dataOut_bits_39;
  wire outToBuffer_io_dataOut_bits_38;
  wire outToBuffer_io_dataOut_bits_37;
  wire outToBuffer_io_dataOut_bits_36;
  wire outToBuffer_io_dataOut_bits_35;
  wire outToBuffer_io_dataOut_bits_34;
  wire outToBuffer_io_dataOut_bits_33;
  wire outToBuffer_io_dataOut_bits_32;
  wire outToBuffer_io_dataOut_bits_31;
  wire outToBuffer_io_dataOut_bits_30;
  wire outToBuffer_io_dataOut_bits_29;
  wire outToBuffer_io_dataOut_bits_28;
  wire outToBuffer_io_dataOut_bits_27;
  wire outToBuffer_io_dataOut_bits_26;
  wire outToBuffer_io_dataOut_bits_25;
  wire outToBuffer_io_dataOut_bits_24;
  wire outToBuffer_io_dataOut_bits_23;
  wire outToBuffer_io_dataOut_bits_22;
  wire outToBuffer_io_dataOut_bits_21;
  wire outToBuffer_io_dataOut_bits_20;
  wire outToBuffer_io_dataOut_bits_19;
  wire outToBuffer_io_dataOut_bits_18;
  wire outToBuffer_io_dataOut_bits_17;
  wire outToBuffer_io_dataOut_bits_16;
  wire outToBuffer_io_dataOut_bits_15;
  wire outToBuffer_io_dataOut_bits_14;
  wire outToBuffer_io_dataOut_bits_13;
  wire outToBuffer_io_dataOut_bits_12;
  wire outToBuffer_io_dataOut_bits_11;
  wire outToBuffer_io_dataOut_bits_10;
  wire outToBuffer_io_dataOut_bits_9;
  wire outToBuffer_io_dataOut_bits_8;
  wire outToBuffer_io_dataOut_bits_7;
  wire outToBuffer_io_dataOut_bits_6;
  wire outToBuffer_io_dataOut_bits_5;
  wire outToBuffer_io_dataOut_bits_4;
  wire outToBuffer_io_dataOut_bits_3;
  wire outToBuffer_io_dataOut_bits_2;
  wire outToBuffer_io_dataOut_bits_1;
  wire outToBuffer_io_dataOut_bits_0;
  wire fifoTxOut_io_enq_ready;
  wire fifoTxOut_io_deq_valid;
  wire[7:0] fifoTxOut_io_deq_bits_7;
  wire[7:0] fifoTxOut_io_deq_bits_6;
  wire[7:0] fifoTxOut_io_deq_bits_5;
  wire[7:0] fifoTxOut_io_deq_bits_4;
  wire[7:0] fifoTxOut_io_deq_bits_3;
  wire[7:0] fifoTxOut_io_deq_bits_2;
  wire[7:0] fifoTxOut_io_deq_bits_1;
  wire[7:0] fifoTxOut_io_deq_bits_0;
  wire[5:0] fifoTxOut_io_count;
  wire userMod_io_dataIn_ready;
  wire[31:0] userMod_io_regOut_14;
  wire[31:0] userMod_io_regOut_13;
  wire[31:0] userMod_io_regOut_12;
  wire[31:0] userMod_io_regOut_11;
  wire[31:0] userMod_io_regOut_10;
  wire[31:0] userMod_io_regOut_9;
  wire[31:0] userMod_io_regOut_8;
  wire[31:0] userMod_io_regOut_7;
  wire[31:0] userMod_io_regOut_6;
  wire[31:0] userMod_io_regOut_5;
  wire[31:0] userMod_io_regOut_4;
  wire[31:0] userMod_io_regOut_3;
  wire[31:0] userMod_io_regOut_2;
  wire[31:0] userMod_io_regOut_1;
  wire[31:0] userMod_io_regOut_0;
  wire userMod_io_regOutEn;
  wire[18:0] userMod_io_memAddr;
  wire userMod_io_error;
  wire userMod_io_dataOut_valid;
  wire[7:0] userMod_io_dataOut_bits;

`ifndef SYNTHESIS
// synthesis translate_off
  integer initvar;
  initial begin
    #0.002;
    controlReg = {1{$random}};
    sending = {1{$random}};
    segmentCounter = {1{$random}};
    buffer_7 = {1{$random}};
    buffer_6 = {1{$random}};
    buffer_5 = {1{$random}};
    buffer_4 = {1{$random}};
    buffer_3 = {1{$random}};
    buffer_2 = {1{$random}};
    buffer_1 = {1{$random}};
    buffer_0 = {1{$random}};
    buffer_15 = {1{$random}};
    buffer_14 = {1{$random}};
    buffer_13 = {1{$random}};
    buffer_12 = {1{$random}};
    buffer_11 = {1{$random}};
    buffer_10 = {1{$random}};
    buffer_9 = {1{$random}};
    buffer_8 = {1{$random}};
    buffer_23 = {1{$random}};
    buffer_22 = {1{$random}};
    buffer_21 = {1{$random}};
    buffer_20 = {1{$random}};
    buffer_19 = {1{$random}};
    buffer_18 = {1{$random}};
    buffer_17 = {1{$random}};
    buffer_16 = {1{$random}};
    buffer_31 = {1{$random}};
    buffer_30 = {1{$random}};
    buffer_29 = {1{$random}};
    buffer_28 = {1{$random}};
    buffer_27 = {1{$random}};
    buffer_26 = {1{$random}};
    buffer_25 = {1{$random}};
    buffer_24 = {1{$random}};
    buffer_39 = {1{$random}};
    buffer_38 = {1{$random}};
    buffer_37 = {1{$random}};
    buffer_36 = {1{$random}};
    buffer_35 = {1{$random}};
    buffer_34 = {1{$random}};
    buffer_33 = {1{$random}};
    buffer_32 = {1{$random}};
    buffer_47 = {1{$random}};
    buffer_46 = {1{$random}};
    buffer_45 = {1{$random}};
    buffer_44 = {1{$random}};
    buffer_43 = {1{$random}};
    buffer_42 = {1{$random}};
    buffer_41 = {1{$random}};
    buffer_40 = {1{$random}};
    buffer_55 = {1{$random}};
    buffer_54 = {1{$random}};
    buffer_53 = {1{$random}};
    buffer_52 = {1{$random}};
    buffer_51 = {1{$random}};
    buffer_50 = {1{$random}};
    buffer_49 = {1{$random}};
    buffer_48 = {1{$random}};
    buffer_63 = {1{$random}};
    buffer_62 = {1{$random}};
    buffer_61 = {1{$random}};
    buffer_60 = {1{$random}};
    buffer_59 = {1{$random}};
    buffer_58 = {1{$random}};
    buffer_57 = {1{$random}};
    buffer_56 = {1{$random}};
    bufferVld = {1{$random}};
    buffCount = {1{$random}};
    for (initvar = 0; initvar < 128; initvar = initvar+1)
      userMem[initvar] = {4{$random}};
    regIntR_0 = {1{$random}};
    regIntR_1 = {1{$random}};
    regIntR_2 = {1{$random}};
    regIntR_3 = {1{$random}};
    regIntR_4 = {1{$random}};
    regIntR_5 = {1{$random}};
    regIntR_6 = {1{$random}};
    regIntR_7 = {1{$random}};
    regIntR_8 = {1{$random}};
    regIntR_9 = {1{$random}};
    regIntR_10 = {1{$random}};
    regIntR_11 = {1{$random}};
    regIntR_12 = {1{$random}};
    regIntR_13 = {1{$random}};
    regIntR_14 = {1{$random}};
    regIntW_0 = {1{$random}};
    regIntW_1 = {1{$random}};
    regIntW_2 = {1{$random}};
    regIntW_3 = {1{$random}};
    regIntW_4 = {1{$random}};
    regIntW_5 = {1{$random}};
    regIntW_6 = {1{$random}};
    regIntW_7 = {1{$random}};
    regIntW_8 = {1{$random}};
    regIntW_9 = {1{$random}};
    regIntW_10 = {1{$random}};
    regIntW_11 = {1{$random}};
    regIntW_12 = {1{$random}};
    regIntW_13 = {1{$random}};
    regIntW_14 = {1{$random}};
    regIntW_15 = {1{$random}};
    userErr = {1{$random}};
    dirOutFull = {1{$random}};
    txFifoFull = {1{$random}};
    fifoFull = {1{$random}};
    crcFail = {1{$random}};
    pktDrop = {1{$random}};
    rx1Err = {1{$random}};
  end
// synthesis translate_on
`endif

`ifndef SYNTHESIS
// synthesis translate_off
  assign T1 = {1{$random}};
  assign T636 = {1{$random}};
  assign T640 = {1{$random}};
  assign T644 = {1{$random}};
  assign T648 = {1{$random}};
  assign T652 = {1{$random}};
  assign T656 = {1{$random}};
  assign T660 = {1{$random}};
  assign T664 = {1{$random}};
  assign T668 = {1{$random}};
  assign T672 = {1{$random}};
  assign T676 = {1{$random}};
  assign T680 = {1{$random}};
  assign T684 = {1{$random}};
  assign T688 = {1{$random}};
  assign T692 = {1{$random}};
  assign T752 = {1{$random}};
  assign T754 = {1{$random}};
  assign T759 = {1{$random}};
  assign T761 = {1{$random}};
  assign T767 = {1{$random}};
  assign T769 = {1{$random}};
  assign T773 = {1{$random}};
  assign T775 = {1{$random}};
  assign T783 = {1{$random}};
  assign T785 = {1{$random}};
  assign T789 = {1{$random}};
  assign T791 = {1{$random}};
  assign T797 = {1{$random}};
  assign T799 = {1{$random}};
  assign T803 = {1{$random}};
  assign T805 = {1{$random}};
// synthesis translate_on
`endif
  assign T836 = T837 | rst;
  assign T837 = controlReg[1];
  assign T838 = rst ? T1 : T2;
  assign T2 = T3 ? reg_w_data : controlReg;
  assign T3 = reg_w_en & T4;
  assign T4 = T5[15];
  assign T5 = 1'h1 << T6;
  assign T6 = T839;
  assign T839 = reg_w_addr[3:0];
  assign T0 = T7 | fifoDrain;
  assign fifoDrain = controlReg[2];
  assign T7 = tx1_ack_usr & sending;
  assign T840 = rst ? 1'h0 : T8;
  assign T8 = T11 ? 1'h0 : T9;
  assign T9 = sending | T10;
  assign T10 = 6'hc <= fifoTxOut_io_count;
  assign T11 = T12 | fifoDrain;
  assign T12 = T13 & tx1_ack_usr;
  assign T13 = eof & tx1_vld_usr;
  assign eof = 4'hc <= segmentCounter;
  assign T841 = rst ? 4'h0 : T14;
  assign T14 = fifoDrain ? 4'h0 : T15;
  assign T15 = T19 ? 4'h0 : T16;
  assign T16 = T18 ? T17 : segmentCounter;
  assign T17 = segmentCounter + 4'h1;
  assign T18 = fifoTxOut_io_deq_valid & T0;
  assign T19 = T18 & eof;
  assign bufferByte_0 = T842;
  assign T842 = T20[7:0];
  assign T20 = T25 | T21;
  assign T21 = T843 & T22;
  assign T22 = 9'h80;
  assign T843 = T23 ? 9'h1ff : 9'h0;
  assign T23 = T24;
  assign T24 = buffer_7;
  assign T25 = T844 & T26;
  assign T26 = ~ T22;
  assign T844 = {1'h0, T27};
  assign T27 = T32 | T28;
  assign T28 = T845 & T29;
  assign T29 = 8'h40;
  assign T845 = T30 ? 8'hff : 8'h0;
  assign T30 = T31;
  assign T31 = buffer_6;
  assign T32 = T34 & T33;
  assign T33 = ~ T29;
  assign T34 = T39 | T846;
  assign T846 = {T848, T35};
  assign T35 = T847 & T36;
  assign T36 = 7'h20;
  assign T847 = T37 ? 7'h7f : 7'h0;
  assign T37 = T38;
  assign T38 = buffer_5;
  assign T848 = T35[6];
  assign T39 = T41 & T849;
  assign T849 = {T850, T40};
  assign T40 = ~ T36;
  assign T850 = T40[6];
  assign T41 = T46 | T851;
  assign T851 = {T853, T42};
  assign T42 = T852 & T43;
  assign T43 = 6'h10;
  assign T852 = T44 ? 6'h3f : 6'h0;
  assign T44 = T45;
  assign T45 = buffer_4;
  assign T853 = T854 ? 2'h3 : 2'h0;
  assign T854 = T42[5];
  assign T46 = T48 & T855;
  assign T855 = {T856, T47};
  assign T47 = ~ T43;
  assign T856 = T857 ? 2'h3 : 2'h0;
  assign T857 = T47[5];
  assign T48 = T53 | T858;
  assign T858 = {T860, T49};
  assign T49 = T859 & T50;
  assign T50 = 5'h8;
  assign T859 = T51 ? 5'h1f : 5'h0;
  assign T51 = T52;
  assign T52 = buffer_3;
  assign T860 = T861 ? 3'h7 : 3'h0;
  assign T861 = T49[4];
  assign T53 = T55 & T862;
  assign T862 = {T863, T54};
  assign T54 = ~ T50;
  assign T863 = T864 ? 3'h7 : 3'h0;
  assign T864 = T54[4];
  assign T55 = T60 | T865;
  assign T865 = {T867, T56};
  assign T56 = T866 & T57;
  assign T57 = 4'h4;
  assign T866 = T58 ? 4'hf : 4'h0;
  assign T58 = T59;
  assign T59 = buffer_2;
  assign T867 = T868 ? 4'hf : 4'h0;
  assign T868 = T56[3];
  assign T60 = T62 & T869;
  assign T869 = {T870, T61};
  assign T61 = ~ T57;
  assign T870 = T871 ? 4'hf : 4'h0;
  assign T871 = T61[3];
  assign T62 = T67 | T872;
  assign T872 = {T874, T63};
  assign T63 = T873 & T64;
  assign T64 = 3'h2;
  assign T873 = T65 ? 3'h7 : 3'h0;
  assign T65 = T66;
  assign T66 = buffer_1;
  assign T874 = T875 ? 5'h1f : 5'h0;
  assign T875 = T63[2];
  assign T67 = T69 & T876;
  assign T876 = {T877, T68};
  assign T68 = ~ T64;
  assign T877 = T878 ? 5'h1f : 5'h0;
  assign T878 = T68[2];
  assign T69 = T74 | T879;
  assign T879 = {T881, T70};
  assign T70 = T880 & T71;
  assign T71 = 2'h1;
  assign T880 = T72 ? 2'h3 : 2'h0;
  assign T72 = T73;
  assign T73 = buffer_0;
  assign T881 = T882 ? 6'h3f : 6'h0;
  assign T882 = T70[1];
  assign T74 = 8'h0 & T883;
  assign T883 = {T884, T75};
  assign T75 = ~ T71;
  assign T884 = T885 ? 6'h3f : 6'h0;
  assign T885 = T75[1];
  assign bufferByte_1 = T886;
  assign T886 = T76[7:0];
  assign T76 = T81 | T77;
  assign T77 = T887 & T78;
  assign T78 = 9'h80;
  assign T887 = T79 ? 9'h1ff : 9'h0;
  assign T79 = T80;
  assign T80 = buffer_15;
  assign T81 = T888 & T82;
  assign T82 = ~ T78;
  assign T888 = {1'h0, T83};
  assign T83 = T88 | T84;
  assign T84 = T889 & T85;
  assign T85 = 8'h40;
  assign T889 = T86 ? 8'hff : 8'h0;
  assign T86 = T87;
  assign T87 = buffer_14;
  assign T88 = T90 & T89;
  assign T89 = ~ T85;
  assign T90 = T95 | T890;
  assign T890 = {T892, T91};
  assign T91 = T891 & T92;
  assign T92 = 7'h20;
  assign T891 = T93 ? 7'h7f : 7'h0;
  assign T93 = T94;
  assign T94 = buffer_13;
  assign T892 = T91[6];
  assign T95 = T97 & T893;
  assign T893 = {T894, T96};
  assign T96 = ~ T92;
  assign T894 = T96[6];
  assign T97 = T102 | T895;
  assign T895 = {T897, T98};
  assign T98 = T896 & T99;
  assign T99 = 6'h10;
  assign T896 = T100 ? 6'h3f : 6'h0;
  assign T100 = T101;
  assign T101 = buffer_12;
  assign T897 = T898 ? 2'h3 : 2'h0;
  assign T898 = T98[5];
  assign T102 = T104 & T899;
  assign T899 = {T900, T103};
  assign T103 = ~ T99;
  assign T900 = T901 ? 2'h3 : 2'h0;
  assign T901 = T103[5];
  assign T104 = T109 | T902;
  assign T902 = {T904, T105};
  assign T105 = T903 & T106;
  assign T106 = 5'h8;
  assign T903 = T107 ? 5'h1f : 5'h0;
  assign T107 = T108;
  assign T108 = buffer_11;
  assign T904 = T905 ? 3'h7 : 3'h0;
  assign T905 = T105[4];
  assign T109 = T111 & T906;
  assign T906 = {T907, T110};
  assign T110 = ~ T106;
  assign T907 = T908 ? 3'h7 : 3'h0;
  assign T908 = T110[4];
  assign T111 = T116 | T909;
  assign T909 = {T911, T112};
  assign T112 = T910 & T113;
  assign T113 = 4'h4;
  assign T910 = T114 ? 4'hf : 4'h0;
  assign T114 = T115;
  assign T115 = buffer_10;
  assign T911 = T912 ? 4'hf : 4'h0;
  assign T912 = T112[3];
  assign T116 = T118 & T913;
  assign T913 = {T914, T117};
  assign T117 = ~ T113;
  assign T914 = T915 ? 4'hf : 4'h0;
  assign T915 = T117[3];
  assign T118 = T123 | T916;
  assign T916 = {T918, T119};
  assign T119 = T917 & T120;
  assign T120 = 3'h2;
  assign T917 = T121 ? 3'h7 : 3'h0;
  assign T121 = T122;
  assign T122 = buffer_9;
  assign T918 = T919 ? 5'h1f : 5'h0;
  assign T919 = T119[2];
  assign T123 = T125 & T920;
  assign T920 = {T921, T124};
  assign T124 = ~ T120;
  assign T921 = T922 ? 5'h1f : 5'h0;
  assign T922 = T124[2];
  assign T125 = T130 | T923;
  assign T923 = {T925, T126};
  assign T126 = T924 & T127;
  assign T127 = 2'h1;
  assign T924 = T128 ? 2'h3 : 2'h0;
  assign T128 = T129;
  assign T129 = buffer_8;
  assign T925 = T926 ? 6'h3f : 6'h0;
  assign T926 = T126[1];
  assign T130 = 8'h0 & T927;
  assign T927 = {T928, T131};
  assign T131 = ~ T127;
  assign T928 = T929 ? 6'h3f : 6'h0;
  assign T929 = T131[1];
  assign bufferByte_2 = T930;
  assign T930 = T132[7:0];
  assign T132 = T137 | T133;
  assign T133 = T931 & T134;
  assign T134 = 9'h80;
  assign T931 = T135 ? 9'h1ff : 9'h0;
  assign T135 = T136;
  assign T136 = buffer_23;
  assign T137 = T932 & T138;
  assign T138 = ~ T134;
  assign T932 = {1'h0, T139};
  assign T139 = T144 | T140;
  assign T140 = T933 & T141;
  assign T141 = 8'h40;
  assign T933 = T142 ? 8'hff : 8'h0;
  assign T142 = T143;
  assign T143 = buffer_22;
  assign T144 = T146 & T145;
  assign T145 = ~ T141;
  assign T146 = T151 | T934;
  assign T934 = {T936, T147};
  assign T147 = T935 & T148;
  assign T148 = 7'h20;
  assign T935 = T149 ? 7'h7f : 7'h0;
  assign T149 = T150;
  assign T150 = buffer_21;
  assign T936 = T147[6];
  assign T151 = T153 & T937;
  assign T937 = {T938, T152};
  assign T152 = ~ T148;
  assign T938 = T152[6];
  assign T153 = T158 | T939;
  assign T939 = {T941, T154};
  assign T154 = T940 & T155;
  assign T155 = 6'h10;
  assign T940 = T156 ? 6'h3f : 6'h0;
  assign T156 = T157;
  assign T157 = buffer_20;
  assign T941 = T942 ? 2'h3 : 2'h0;
  assign T942 = T154[5];
  assign T158 = T160 & T943;
  assign T943 = {T944, T159};
  assign T159 = ~ T155;
  assign T944 = T945 ? 2'h3 : 2'h0;
  assign T945 = T159[5];
  assign T160 = T165 | T946;
  assign T946 = {T948, T161};
  assign T161 = T947 & T162;
  assign T162 = 5'h8;
  assign T947 = T163 ? 5'h1f : 5'h0;
  assign T163 = T164;
  assign T164 = buffer_19;
  assign T948 = T949 ? 3'h7 : 3'h0;
  assign T949 = T161[4];
  assign T165 = T167 & T950;
  assign T950 = {T951, T166};
  assign T166 = ~ T162;
  assign T951 = T952 ? 3'h7 : 3'h0;
  assign T952 = T166[4];
  assign T167 = T172 | T953;
  assign T953 = {T955, T168};
  assign T168 = T954 & T169;
  assign T169 = 4'h4;
  assign T954 = T170 ? 4'hf : 4'h0;
  assign T170 = T171;
  assign T171 = buffer_18;
  assign T955 = T956 ? 4'hf : 4'h0;
  assign T956 = T168[3];
  assign T172 = T174 & T957;
  assign T957 = {T958, T173};
  assign T173 = ~ T169;
  assign T958 = T959 ? 4'hf : 4'h0;
  assign T959 = T173[3];
  assign T174 = T179 | T960;
  assign T960 = {T962, T175};
  assign T175 = T961 & T176;
  assign T176 = 3'h2;
  assign T961 = T177 ? 3'h7 : 3'h0;
  assign T177 = T178;
  assign T178 = buffer_17;
  assign T962 = T963 ? 5'h1f : 5'h0;
  assign T963 = T175[2];
  assign T179 = T181 & T964;
  assign T964 = {T965, T180};
  assign T180 = ~ T176;
  assign T965 = T966 ? 5'h1f : 5'h0;
  assign T966 = T180[2];
  assign T181 = T186 | T967;
  assign T967 = {T969, T182};
  assign T182 = T968 & T183;
  assign T183 = 2'h1;
  assign T968 = T184 ? 2'h3 : 2'h0;
  assign T184 = T185;
  assign T185 = buffer_16;
  assign T969 = T970 ? 6'h3f : 6'h0;
  assign T970 = T182[1];
  assign T186 = 8'h0 & T971;
  assign T971 = {T972, T187};
  assign T187 = ~ T183;
  assign T972 = T973 ? 6'h3f : 6'h0;
  assign T973 = T187[1];
  assign bufferByte_3 = T974;
  assign T974 = T188[7:0];
  assign T188 = T193 | T189;
  assign T189 = T975 & T190;
  assign T190 = 9'h80;
  assign T975 = T191 ? 9'h1ff : 9'h0;
  assign T191 = T192;
  assign T192 = buffer_31;
  assign T193 = T976 & T194;
  assign T194 = ~ T190;
  assign T976 = {1'h0, T195};
  assign T195 = T200 | T196;
  assign T196 = T977 & T197;
  assign T197 = 8'h40;
  assign T977 = T198 ? 8'hff : 8'h0;
  assign T198 = T199;
  assign T199 = buffer_30;
  assign T200 = T202 & T201;
  assign T201 = ~ T197;
  assign T202 = T207 | T978;
  assign T978 = {T980, T203};
  assign T203 = T979 & T204;
  assign T204 = 7'h20;
  assign T979 = T205 ? 7'h7f : 7'h0;
  assign T205 = T206;
  assign T206 = buffer_29;
  assign T980 = T203[6];
  assign T207 = T209 & T981;
  assign T981 = {T982, T208};
  assign T208 = ~ T204;
  assign T982 = T208[6];
  assign T209 = T214 | T983;
  assign T983 = {T985, T210};
  assign T210 = T984 & T211;
  assign T211 = 6'h10;
  assign T984 = T212 ? 6'h3f : 6'h0;
  assign T212 = T213;
  assign T213 = buffer_28;
  assign T985 = T986 ? 2'h3 : 2'h0;
  assign T986 = T210[5];
  assign T214 = T216 & T987;
  assign T987 = {T988, T215};
  assign T215 = ~ T211;
  assign T988 = T989 ? 2'h3 : 2'h0;
  assign T989 = T215[5];
  assign T216 = T221 | T990;
  assign T990 = {T992, T217};
  assign T217 = T991 & T218;
  assign T218 = 5'h8;
  assign T991 = T219 ? 5'h1f : 5'h0;
  assign T219 = T220;
  assign T220 = buffer_27;
  assign T992 = T993 ? 3'h7 : 3'h0;
  assign T993 = T217[4];
  assign T221 = T223 & T994;
  assign T994 = {T995, T222};
  assign T222 = ~ T218;
  assign T995 = T996 ? 3'h7 : 3'h0;
  assign T996 = T222[4];
  assign T223 = T228 | T997;
  assign T997 = {T999, T224};
  assign T224 = T998 & T225;
  assign T225 = 4'h4;
  assign T998 = T226 ? 4'hf : 4'h0;
  assign T226 = T227;
  assign T227 = buffer_26;
  assign T999 = T1000 ? 4'hf : 4'h0;
  assign T1000 = T224[3];
  assign T228 = T230 & T1001;
  assign T1001 = {T1002, T229};
  assign T229 = ~ T225;
  assign T1002 = T1003 ? 4'hf : 4'h0;
  assign T1003 = T229[3];
  assign T230 = T235 | T1004;
  assign T1004 = {T1006, T231};
  assign T231 = T1005 & T232;
  assign T232 = 3'h2;
  assign T1005 = T233 ? 3'h7 : 3'h0;
  assign T233 = T234;
  assign T234 = buffer_25;
  assign T1006 = T1007 ? 5'h1f : 5'h0;
  assign T1007 = T231[2];
  assign T235 = T237 & T1008;
  assign T1008 = {T1009, T236};
  assign T236 = ~ T232;
  assign T1009 = T1010 ? 5'h1f : 5'h0;
  assign T1010 = T236[2];
  assign T237 = T242 | T1011;
  assign T1011 = {T1013, T238};
  assign T238 = T1012 & T239;
  assign T239 = 2'h1;
  assign T1012 = T240 ? 2'h3 : 2'h0;
  assign T240 = T241;
  assign T241 = buffer_24;
  assign T1013 = T1014 ? 6'h3f : 6'h0;
  assign T1014 = T238[1];
  assign T242 = 8'h0 & T1015;
  assign T1015 = {T1016, T243};
  assign T243 = ~ T239;
  assign T1016 = T1017 ? 6'h3f : 6'h0;
  assign T1017 = T243[1];
  assign bufferByte_4 = T1018;
  assign T1018 = T244[7:0];
  assign T244 = T249 | T245;
  assign T245 = T1019 & T246;
  assign T246 = 9'h80;
  assign T1019 = T247 ? 9'h1ff : 9'h0;
  assign T247 = T248;
  assign T248 = buffer_39;
  assign T249 = T1020 & T250;
  assign T250 = ~ T246;
  assign T1020 = {1'h0, T251};
  assign T251 = T256 | T252;
  assign T252 = T1021 & T253;
  assign T253 = 8'h40;
  assign T1021 = T254 ? 8'hff : 8'h0;
  assign T254 = T255;
  assign T255 = buffer_38;
  assign T256 = T258 & T257;
  assign T257 = ~ T253;
  assign T258 = T263 | T1022;
  assign T1022 = {T1024, T259};
  assign T259 = T1023 & T260;
  assign T260 = 7'h20;
  assign T1023 = T261 ? 7'h7f : 7'h0;
  assign T261 = T262;
  assign T262 = buffer_37;
  assign T1024 = T259[6];
  assign T263 = T265 & T1025;
  assign T1025 = {T1026, T264};
  assign T264 = ~ T260;
  assign T1026 = T264[6];
  assign T265 = T270 | T1027;
  assign T1027 = {T1029, T266};
  assign T266 = T1028 & T267;
  assign T267 = 6'h10;
  assign T1028 = T268 ? 6'h3f : 6'h0;
  assign T268 = T269;
  assign T269 = buffer_36;
  assign T1029 = T1030 ? 2'h3 : 2'h0;
  assign T1030 = T266[5];
  assign T270 = T272 & T1031;
  assign T1031 = {T1032, T271};
  assign T271 = ~ T267;
  assign T1032 = T1033 ? 2'h3 : 2'h0;
  assign T1033 = T271[5];
  assign T272 = T277 | T1034;
  assign T1034 = {T1036, T273};
  assign T273 = T1035 & T274;
  assign T274 = 5'h8;
  assign T1035 = T275 ? 5'h1f : 5'h0;
  assign T275 = T276;
  assign T276 = buffer_35;
  assign T1036 = T1037 ? 3'h7 : 3'h0;
  assign T1037 = T273[4];
  assign T277 = T279 & T1038;
  assign T1038 = {T1039, T278};
  assign T278 = ~ T274;
  assign T1039 = T1040 ? 3'h7 : 3'h0;
  assign T1040 = T278[4];
  assign T279 = T284 | T1041;
  assign T1041 = {T1043, T280};
  assign T280 = T1042 & T281;
  assign T281 = 4'h4;
  assign T1042 = T282 ? 4'hf : 4'h0;
  assign T282 = T283;
  assign T283 = buffer_34;
  assign T1043 = T1044 ? 4'hf : 4'h0;
  assign T1044 = T280[3];
  assign T284 = T286 & T1045;
  assign T1045 = {T1046, T285};
  assign T285 = ~ T281;
  assign T1046 = T1047 ? 4'hf : 4'h0;
  assign T1047 = T285[3];
  assign T286 = T291 | T1048;
  assign T1048 = {T1050, T287};
  assign T287 = T1049 & T288;
  assign T288 = 3'h2;
  assign T1049 = T289 ? 3'h7 : 3'h0;
  assign T289 = T290;
  assign T290 = buffer_33;
  assign T1050 = T1051 ? 5'h1f : 5'h0;
  assign T1051 = T287[2];
  assign T291 = T293 & T1052;
  assign T1052 = {T1053, T292};
  assign T292 = ~ T288;
  assign T1053 = T1054 ? 5'h1f : 5'h0;
  assign T1054 = T292[2];
  assign T293 = T298 | T1055;
  assign T1055 = {T1057, T294};
  assign T294 = T1056 & T295;
  assign T295 = 2'h1;
  assign T1056 = T296 ? 2'h3 : 2'h0;
  assign T296 = T297;
  assign T297 = buffer_32;
  assign T1057 = T1058 ? 6'h3f : 6'h0;
  assign T1058 = T294[1];
  assign T298 = 8'h0 & T1059;
  assign T1059 = {T1060, T299};
  assign T299 = ~ T295;
  assign T1060 = T1061 ? 6'h3f : 6'h0;
  assign T1061 = T299[1];
  assign bufferByte_5 = T1062;
  assign T1062 = T300[7:0];
  assign T300 = T305 | T301;
  assign T301 = T1063 & T302;
  assign T302 = 9'h80;
  assign T1063 = T303 ? 9'h1ff : 9'h0;
  assign T303 = T304;
  assign T304 = buffer_47;
  assign T305 = T1064 & T306;
  assign T306 = ~ T302;
  assign T1064 = {1'h0, T307};
  assign T307 = T312 | T308;
  assign T308 = T1065 & T309;
  assign T309 = 8'h40;
  assign T1065 = T310 ? 8'hff : 8'h0;
  assign T310 = T311;
  assign T311 = buffer_46;
  assign T312 = T314 & T313;
  assign T313 = ~ T309;
  assign T314 = T319 | T1066;
  assign T1066 = {T1068, T315};
  assign T315 = T1067 & T316;
  assign T316 = 7'h20;
  assign T1067 = T317 ? 7'h7f : 7'h0;
  assign T317 = T318;
  assign T318 = buffer_45;
  assign T1068 = T315[6];
  assign T319 = T321 & T1069;
  assign T1069 = {T1070, T320};
  assign T320 = ~ T316;
  assign T1070 = T320[6];
  assign T321 = T326 | T1071;
  assign T1071 = {T1073, T322};
  assign T322 = T1072 & T323;
  assign T323 = 6'h10;
  assign T1072 = T324 ? 6'h3f : 6'h0;
  assign T324 = T325;
  assign T325 = buffer_44;
  assign T1073 = T1074 ? 2'h3 : 2'h0;
  assign T1074 = T322[5];
  assign T326 = T328 & T1075;
  assign T1075 = {T1076, T327};
  assign T327 = ~ T323;
  assign T1076 = T1077 ? 2'h3 : 2'h0;
  assign T1077 = T327[5];
  assign T328 = T333 | T1078;
  assign T1078 = {T1080, T329};
  assign T329 = T1079 & T330;
  assign T330 = 5'h8;
  assign T1079 = T331 ? 5'h1f : 5'h0;
  assign T331 = T332;
  assign T332 = buffer_43;
  assign T1080 = T1081 ? 3'h7 : 3'h0;
  assign T1081 = T329[4];
  assign T333 = T335 & T1082;
  assign T1082 = {T1083, T334};
  assign T334 = ~ T330;
  assign T1083 = T1084 ? 3'h7 : 3'h0;
  assign T1084 = T334[4];
  assign T335 = T340 | T1085;
  assign T1085 = {T1087, T336};
  assign T336 = T1086 & T337;
  assign T337 = 4'h4;
  assign T1086 = T338 ? 4'hf : 4'h0;
  assign T338 = T339;
  assign T339 = buffer_42;
  assign T1087 = T1088 ? 4'hf : 4'h0;
  assign T1088 = T336[3];
  assign T340 = T342 & T1089;
  assign T1089 = {T1090, T341};
  assign T341 = ~ T337;
  assign T1090 = T1091 ? 4'hf : 4'h0;
  assign T1091 = T341[3];
  assign T342 = T347 | T1092;
  assign T1092 = {T1094, T343};
  assign T343 = T1093 & T344;
  assign T344 = 3'h2;
  assign T1093 = T345 ? 3'h7 : 3'h0;
  assign T345 = T346;
  assign T346 = buffer_41;
  assign T1094 = T1095 ? 5'h1f : 5'h0;
  assign T1095 = T343[2];
  assign T347 = T349 & T1096;
  assign T1096 = {T1097, T348};
  assign T348 = ~ T344;
  assign T1097 = T1098 ? 5'h1f : 5'h0;
  assign T1098 = T348[2];
  assign T349 = T354 | T1099;
  assign T1099 = {T1101, T350};
  assign T350 = T1100 & T351;
  assign T351 = 2'h1;
  assign T1100 = T352 ? 2'h3 : 2'h0;
  assign T352 = T353;
  assign T353 = buffer_40;
  assign T1101 = T1102 ? 6'h3f : 6'h0;
  assign T1102 = T350[1];
  assign T354 = 8'h0 & T1103;
  assign T1103 = {T1104, T355};
  assign T355 = ~ T351;
  assign T1104 = T1105 ? 6'h3f : 6'h0;
  assign T1105 = T355[1];
  assign bufferByte_6 = T1106;
  assign T1106 = T356[7:0];
  assign T356 = T361 | T357;
  assign T357 = T1107 & T358;
  assign T358 = 9'h80;
  assign T1107 = T359 ? 9'h1ff : 9'h0;
  assign T359 = T360;
  assign T360 = buffer_55;
  assign T361 = T1108 & T362;
  assign T362 = ~ T358;
  assign T1108 = {1'h0, T363};
  assign T363 = T368 | T364;
  assign T364 = T1109 & T365;
  assign T365 = 8'h40;
  assign T1109 = T366 ? 8'hff : 8'h0;
  assign T366 = T367;
  assign T367 = buffer_54;
  assign T368 = T370 & T369;
  assign T369 = ~ T365;
  assign T370 = T375 | T1110;
  assign T1110 = {T1112, T371};
  assign T371 = T1111 & T372;
  assign T372 = 7'h20;
  assign T1111 = T373 ? 7'h7f : 7'h0;
  assign T373 = T374;
  assign T374 = buffer_53;
  assign T1112 = T371[6];
  assign T375 = T377 & T1113;
  assign T1113 = {T1114, T376};
  assign T376 = ~ T372;
  assign T1114 = T376[6];
  assign T377 = T382 | T1115;
  assign T1115 = {T1117, T378};
  assign T378 = T1116 & T379;
  assign T379 = 6'h10;
  assign T1116 = T380 ? 6'h3f : 6'h0;
  assign T380 = T381;
  assign T381 = buffer_52;
  assign T1117 = T1118 ? 2'h3 : 2'h0;
  assign T1118 = T378[5];
  assign T382 = T384 & T1119;
  assign T1119 = {T1120, T383};
  assign T383 = ~ T379;
  assign T1120 = T1121 ? 2'h3 : 2'h0;
  assign T1121 = T383[5];
  assign T384 = T389 | T1122;
  assign T1122 = {T1124, T385};
  assign T385 = T1123 & T386;
  assign T386 = 5'h8;
  assign T1123 = T387 ? 5'h1f : 5'h0;
  assign T387 = T388;
  assign T388 = buffer_51;
  assign T1124 = T1125 ? 3'h7 : 3'h0;
  assign T1125 = T385[4];
  assign T389 = T391 & T1126;
  assign T1126 = {T1127, T390};
  assign T390 = ~ T386;
  assign T1127 = T1128 ? 3'h7 : 3'h0;
  assign T1128 = T390[4];
  assign T391 = T396 | T1129;
  assign T1129 = {T1131, T392};
  assign T392 = T1130 & T393;
  assign T393 = 4'h4;
  assign T1130 = T394 ? 4'hf : 4'h0;
  assign T394 = T395;
  assign T395 = buffer_50;
  assign T1131 = T1132 ? 4'hf : 4'h0;
  assign T1132 = T392[3];
  assign T396 = T398 & T1133;
  assign T1133 = {T1134, T397};
  assign T397 = ~ T393;
  assign T1134 = T1135 ? 4'hf : 4'h0;
  assign T1135 = T397[3];
  assign T398 = T403 | T1136;
  assign T1136 = {T1138, T399};
  assign T399 = T1137 & T400;
  assign T400 = 3'h2;
  assign T1137 = T401 ? 3'h7 : 3'h0;
  assign T401 = T402;
  assign T402 = buffer_49;
  assign T1138 = T1139 ? 5'h1f : 5'h0;
  assign T1139 = T399[2];
  assign T403 = T405 & T1140;
  assign T1140 = {T1141, T404};
  assign T404 = ~ T400;
  assign T1141 = T1142 ? 5'h1f : 5'h0;
  assign T1142 = T404[2];
  assign T405 = T410 | T1143;
  assign T1143 = {T1145, T406};
  assign T406 = T1144 & T407;
  assign T407 = 2'h1;
  assign T1144 = T408 ? 2'h3 : 2'h0;
  assign T408 = T409;
  assign T409 = buffer_48;
  assign T1145 = T1146 ? 6'h3f : 6'h0;
  assign T1146 = T406[1];
  assign T410 = 8'h0 & T1147;
  assign T1147 = {T1148, T411};
  assign T411 = ~ T407;
  assign T1148 = T1149 ? 6'h3f : 6'h0;
  assign T1149 = T411[1];
  assign bufferByte_7 = T1150;
  assign T1150 = T412[7:0];
  assign T412 = T417 | T413;
  assign T413 = T1151 & T414;
  assign T414 = 9'h80;
  assign T1151 = T415 ? 9'h1ff : 9'h0;
  assign T415 = T416;
  assign T416 = buffer_63;
  assign T417 = T1152 & T418;
  assign T418 = ~ T414;
  assign T1152 = {1'h0, T419};
  assign T419 = T424 | T420;
  assign T420 = T1153 & T421;
  assign T421 = 8'h40;
  assign T1153 = T422 ? 8'hff : 8'h0;
  assign T422 = T423;
  assign T423 = buffer_62;
  assign T424 = T426 & T425;
  assign T425 = ~ T421;
  assign T426 = T431 | T1154;
  assign T1154 = {T1156, T427};
  assign T427 = T1155 & T428;
  assign T428 = 7'h20;
  assign T1155 = T429 ? 7'h7f : 7'h0;
  assign T429 = T430;
  assign T430 = buffer_61;
  assign T1156 = T427[6];
  assign T431 = T433 & T1157;
  assign T1157 = {T1158, T432};
  assign T432 = ~ T428;
  assign T1158 = T432[6];
  assign T433 = T438 | T1159;
  assign T1159 = {T1161, T434};
  assign T434 = T1160 & T435;
  assign T435 = 6'h10;
  assign T1160 = T436 ? 6'h3f : 6'h0;
  assign T436 = T437;
  assign T437 = buffer_60;
  assign T1161 = T1162 ? 2'h3 : 2'h0;
  assign T1162 = T434[5];
  assign T438 = T440 & T1163;
  assign T1163 = {T1164, T439};
  assign T439 = ~ T435;
  assign T1164 = T1165 ? 2'h3 : 2'h0;
  assign T1165 = T439[5];
  assign T440 = T445 | T1166;
  assign T1166 = {T1168, T441};
  assign T441 = T1167 & T442;
  assign T442 = 5'h8;
  assign T1167 = T443 ? 5'h1f : 5'h0;
  assign T443 = T444;
  assign T444 = buffer_59;
  assign T1168 = T1169 ? 3'h7 : 3'h0;
  assign T1169 = T441[4];
  assign T445 = T447 & T1170;
  assign T1170 = {T1171, T446};
  assign T446 = ~ T442;
  assign T1171 = T1172 ? 3'h7 : 3'h0;
  assign T1172 = T446[4];
  assign T447 = T452 | T1173;
  assign T1173 = {T1175, T448};
  assign T448 = T1174 & T449;
  assign T449 = 4'h4;
  assign T1174 = T450 ? 4'hf : 4'h0;
  assign T450 = T451;
  assign T451 = buffer_58;
  assign T1175 = T1176 ? 4'hf : 4'h0;
  assign T1176 = T448[3];
  assign T452 = T454 & T1177;
  assign T1177 = {T1178, T453};
  assign T453 = ~ T449;
  assign T1178 = T1179 ? 4'hf : 4'h0;
  assign T1179 = T453[3];
  assign T454 = T459 | T1180;
  assign T1180 = {T1182, T455};
  assign T455 = T1181 & T456;
  assign T456 = 3'h2;
  assign T1181 = T457 ? 3'h7 : 3'h0;
  assign T457 = T458;
  assign T458 = buffer_57;
  assign T1182 = T1183 ? 5'h1f : 5'h0;
  assign T1183 = T455[2];
  assign T459 = T461 & T1184;
  assign T1184 = {T1185, T460};
  assign T460 = ~ T456;
  assign T1185 = T1186 ? 5'h1f : 5'h0;
  assign T1186 = T460[2];
  assign T461 = T466 | T1187;
  assign T1187 = {T1189, T462};
  assign T462 = T1188 & T463;
  assign T463 = 2'h1;
  assign T1188 = T464 ? 2'h3 : 2'h0;
  assign T464 = T465;
  assign T465 = buffer_56;
  assign T1189 = T1190 ? 6'h3f : 6'h0;
  assign T1190 = T462[1];
  assign T466 = 8'h0 & T1191;
  assign T1191 = {T1192, T467};
  assign T467 = ~ T463;
  assign T1192 = T1193 ? 6'h3f : 6'h0;
  assign T1193 = T467[1];
  assign flush = T468 | fifoDrain;
  assign T468 = T469 & directOutputFifo_io_deq_valid;
  assign T469 = 7'h63 <= buffCount;
  assign T1194 = rst ? 7'h0 : T470;
  assign T470 = flush ? 7'h0 : T471;
  assign T471 = T473 ? T472 : buffCount;
  assign T472 = buffCount + 7'h1;
  assign T473 = outToBuffer_io_dataIn_ready & directOutputFifo_io_deq_valid;
  assign vecDataOut_0 = T474;
  assign T474 = userMod_io_dataOut_bits[0];
  assign vecDataOut_1 = T475;
  assign T475 = userMod_io_dataOut_bits[1];
  assign vecDataOut_2 = T476;
  assign T476 = userMod_io_dataOut_bits[2];
  assign vecDataOut_3 = T477;
  assign T477 = userMod_io_dataOut_bits[3];
  assign vecDataOut_4 = T478;
  assign T478 = userMod_io_dataOut_bits[4];
  assign vecDataOut_5 = T479;
  assign T479 = userMod_io_dataOut_bits[5];
  assign vecDataOut_6 = T480;
  assign T480 = userMod_io_dataOut_bits[6];
  assign vecDataOut_7 = T481;
  assign T481 = userMod_io_dataOut_bits[7];
  assign T482 = stripper_io_out_data[7:0];
  assign T483 = stripper_io_out_data[15:8];
  assign T484 = stripper_io_out_data[23:16];
  assign T485 = stripper_io_out_data[31:24];
  assign T486 = stripper_io_out_data[39:32];
  assign T487 = stripper_io_out_data[47:40];
  assign T488 = stripper_io_out_data[55:48];
  assign T489 = stripper_io_out_data[63:56];
  assign T1195 = T490[127:0];
  assign T490 = T528 | T1196;
  assign T1196 = {1'h0, T491};
  assign T491 = T492 << 7'h78;
  assign T492 = T493 & 8'hff;
  assign T493 = T494[127:120];
  assign T494 = userMem[T1239];
  assign T1197 = {112'h0, mem_w_en};
  assign T1198 = T1236 | T1199;
  assign T1199 = T1202 & T1200;
  assign T1200 = {112'h0, T1201};
  assign T1201 = ~ mem_w_en;
  assign T1202 = T1203;
  assign T1203 = {T1221, T1204};
  assign T1204 = {T1214, T1205};
  assign T1205 = {T1211, T1206};
  assign T1206 = {T1210, T1207};
  assign T1207 = T1208[7:0];
  assign T1208 = userMem[T1209];
  assign T1209 = mem_w_addr[6:0];
  assign T1210 = T1208[15:8];
  assign T1211 = {T1213, T1212};
  assign T1212 = T1208[23:16];
  assign T1213 = T1208[31:24];
  assign T1214 = {T1218, T1215};
  assign T1215 = {T1217, T1216};
  assign T1216 = T1208[39:32];
  assign T1217 = T1208[47:40];
  assign T1218 = {T1220, T1219};
  assign T1219 = T1208[55:48];
  assign T1220 = T1208[63:56];
  assign T1221 = {T1229, T1222};
  assign T1222 = {T1226, T1223};
  assign T1223 = {T1225, T1224};
  assign T1224 = T1208[71:64];
  assign T1225 = T1208[79:72];
  assign T1226 = {T1228, T1227};
  assign T1227 = T1208[87:80];
  assign T1228 = T1208[95:88];
  assign T1229 = {T1233, T1230};
  assign T1230 = {T1232, T1231};
  assign T1231 = T1208[103:96];
  assign T1232 = T1208[111:104];
  assign T1233 = {T1235, T1234};
  assign T1234 = T1208[119:112];
  assign T1235 = T1208[127:120];
  assign T1236 = T496 & T1237;
  assign T1237 = {112'h0, mem_w_en};
  assign T496 = T497;
  assign T497 = {T513, T498};
  assign T498 = {T506, T499};
  assign T499 = {T503, T500};
  assign T500 = {memWData_1, memWData_0};
  assign memWData_0 = T501;
  assign T501 = mem_w_data[7:0];
  assign memWData_1 = T502;
  assign T502 = mem_w_data[15:8];
  assign T503 = {memWData_3, memWData_2};
  assign memWData_2 = T504;
  assign T504 = mem_w_data[23:16];
  assign memWData_3 = T505;
  assign T505 = mem_w_data[31:24];
  assign T506 = {T510, T507};
  assign T507 = {memWData_5, memWData_4};
  assign memWData_4 = T508;
  assign T508 = mem_w_data[39:32];
  assign memWData_5 = T509;
  assign T509 = mem_w_data[47:40];
  assign T510 = {memWData_7, memWData_6};
  assign memWData_6 = T511;
  assign T511 = mem_w_data[55:48];
  assign memWData_7 = T512;
  assign T512 = mem_w_data[63:56];
  assign T513 = {T521, T514};
  assign T514 = {T518, T515};
  assign T515 = {memWData_9, memWData_8};
  assign memWData_8 = T516;
  assign T516 = mem_w_data[71:64];
  assign memWData_9 = T517;
  assign T517 = mem_w_data[79:72];
  assign T518 = {memWData_11, memWData_10};
  assign memWData_10 = T519;
  assign T519 = mem_w_data[87:80];
  assign memWData_11 = T520;
  assign T520 = mem_w_data[95:88];
  assign T521 = {T525, T522};
  assign T522 = {memWData_13, memWData_12};
  assign memWData_12 = T523;
  assign T523 = mem_w_data[103:96];
  assign memWData_13 = T524;
  assign T524 = mem_w_data[111:104];
  assign T525 = {memWData_15, memWData_14};
  assign memWData_14 = T526;
  assign T526 = mem_w_data[119:112];
  assign memWData_15 = T527;
  assign T527 = mem_w_data[127:120];
  assign T1238 = mem_w_addr[6:0];
  assign T1239 = userMod_io_memAddr[6:0];
  assign T528 = T1240 & T529;
  assign T529 = ~ T530;
  assign T530 = 129'hff000000000000000000000000000000;
  assign T1240 = {1'h0, T531};
  assign T531 = T535 | T1241;
  assign T1241 = {8'h0, T532};
  assign T532 = T533 << 7'h70;
  assign T533 = T534 & 8'hff;
  assign T534 = T494[119:112];
  assign T535 = T538 & T1242;
  assign T1242 = {T1243, T536};
  assign T536 = ~ T537;
  assign T537 = 121'hff0000000000000000000000000000;
  assign T1243 = T1244 ? 7'h7f : 7'h0;
  assign T1244 = T536[120];
  assign T538 = T542 | T1245;
  assign T1245 = {16'h0, T539};
  assign T539 = T540 << 7'h68;
  assign T540 = T541 & 8'hff;
  assign T541 = T494[111:104];
  assign T542 = T545 & T1246;
  assign T1246 = {T1247, T543};
  assign T543 = ~ T544;
  assign T544 = 113'hff00000000000000000000000000;
  assign T1247 = T1248 ? 15'h7fff : 15'h0;
  assign T1248 = T543[112];
  assign T545 = T549 | T1249;
  assign T1249 = {24'h0, T546};
  assign T546 = T547 << 7'h60;
  assign T547 = T548 & 8'hff;
  assign T548 = T494[103:96];
  assign T549 = T552 & T1250;
  assign T1250 = {T1251, T550};
  assign T550 = ~ T551;
  assign T551 = 105'hff000000000000000000000000;
  assign T1251 = T1252 ? 23'h7fffff : 23'h0;
  assign T1252 = T550[104];
  assign T552 = T556 | T1253;
  assign T1253 = {32'h0, T553};
  assign T553 = T554 << 7'h58;
  assign T554 = T555 & 8'hff;
  assign T555 = T494[95:88];
  assign T556 = T559 & T1254;
  assign T1254 = {T1255, T557};
  assign T557 = ~ T558;
  assign T558 = 97'hff0000000000000000000000;
  assign T1255 = T1256 ? 31'h7fffffff : 31'h0;
  assign T1256 = T557[96];
  assign T559 = T563 | T1257;
  assign T1257 = {40'h0, T560};
  assign T560 = T561 << 7'h50;
  assign T561 = T562 & 8'hff;
  assign T562 = T494[87:80];
  assign T563 = T566 & T1258;
  assign T1258 = {T1259, T564};
  assign T564 = ~ T565;
  assign T565 = 89'hff00000000000000000000;
  assign T1259 = T1260 ? 39'h7fffffffff : 39'h0;
  assign T1260 = T564[88];
  assign T566 = T570 | T1261;
  assign T1261 = {48'h0, T567};
  assign T567 = T568 << 7'h48;
  assign T568 = T569 & 8'hff;
  assign T569 = T494[79:72];
  assign T570 = T573 & T1262;
  assign T1262 = {T1263, T571};
  assign T571 = ~ T572;
  assign T572 = 81'hff000000000000000000;
  assign T1263 = T1264 ? 47'h7fffffffffff : 47'h0;
  assign T1264 = T571[80];
  assign T573 = T577 | T1265;
  assign T1265 = {56'h0, T574};
  assign T574 = T575 << 7'h40;
  assign T575 = T576 & 8'hff;
  assign T576 = T494[71:64];
  assign T577 = T580 & T1266;
  assign T1266 = {T1267, T578};
  assign T578 = ~ T579;
  assign T579 = 73'hff0000000000000000;
  assign T1267 = T1268 ? 55'h7fffffffffffff : 55'h0;
  assign T1268 = T578[72];
  assign T580 = T584 | T1269;
  assign T1269 = {64'h0, T581};
  assign T581 = T582 << 6'h38;
  assign T582 = T583 & 8'hff;
  assign T583 = T494[63:56];
  assign T584 = T587 & T1270;
  assign T1270 = {T1271, T585};
  assign T585 = ~ T586;
  assign T586 = 65'hff00000000000000;
  assign T1271 = T1272 ? 63'h7fffffffffffffff : 63'h0;
  assign T1272 = T585[64];
  assign T587 = T591 | T1273;
  assign T1273 = {72'h0, T588};
  assign T588 = T589 << 6'h30;
  assign T589 = T590 & 8'hff;
  assign T590 = T494[55:48];
  assign T591 = T594 & T1274;
  assign T1274 = {T1275, T592};
  assign T592 = ~ T593;
  assign T593 = 57'hff000000000000;
  assign T1275 = T1276 ? 71'h7fffffffffffffffff : 71'h0;
  assign T1276 = T592[56];
  assign T594 = T598 | T1277;
  assign T1277 = {80'h0, T595};
  assign T595 = T596 << 6'h28;
  assign T596 = T597 & 8'hff;
  assign T597 = T494[47:40];
  assign T598 = T601 & T1278;
  assign T1278 = {T1279, T599};
  assign T599 = ~ T600;
  assign T600 = 49'hff0000000000;
  assign T1279 = T1280 ? 79'h7fffffffffffffffffff : 79'h0;
  assign T1280 = T599[48];
  assign T601 = T605 | T1281;
  assign T1281 = {88'h0, T602};
  assign T602 = T603 << 6'h20;
  assign T603 = T604 & 8'hff;
  assign T604 = T494[39:32];
  assign T605 = T608 & T1282;
  assign T1282 = {T1283, T606};
  assign T606 = ~ T607;
  assign T607 = 41'hff00000000;
  assign T1283 = T1284 ? 87'h7fffffffffffffffffffff : 87'h0;
  assign T1284 = T606[40];
  assign T608 = T612 | T1285;
  assign T1285 = {96'h0, T609};
  assign T609 = T610 << 5'h18;
  assign T610 = T611 & 8'hff;
  assign T611 = T494[31:24];
  assign T612 = T615 & T1286;
  assign T1286 = {T1287, T613};
  assign T613 = ~ T614;
  assign T614 = 33'hff000000;
  assign T1287 = T1288 ? 95'h7fffffffffffffffffffffff : 95'h0;
  assign T1288 = T613[32];
  assign T615 = T619 | T1289;
  assign T1289 = {104'h0, T616};
  assign T616 = T617 << 5'h10;
  assign T617 = T618 & 8'hff;
  assign T618 = T494[23:16];
  assign T619 = T622 & T1290;
  assign T1290 = {T1291, T620};
  assign T620 = ~ T621;
  assign T621 = 25'hff0000;
  assign T1291 = T1292 ? 103'h7fffffffffffffffffffffffff : 103'h0;
  assign T1292 = T620[24];
  assign T622 = T626 | T1293;
  assign T1293 = {112'h0, T623};
  assign T623 = T624 << 4'h8;
  assign T624 = T625 & 8'hff;
  assign T625 = T494[15:8];
  assign T626 = T629 & T1294;
  assign T1294 = {T1295, T627};
  assign T627 = ~ T628;
  assign T628 = 17'hff00;
  assign T1295 = T1296 ? 111'h7fffffffffffffffffffffffffff : 111'h0;
  assign T1296 = T627[16];
  assign T629 = T633 | T1297;
  assign T1297 = {120'h0, T630};
  assign T630 = T631 << 1'h0;
  assign T631 = T632 & 8'hff;
  assign T632 = T494[7:0];
  assign T633 = 128'h0 & T1298;
  assign T1298 = {T1299, T634};
  assign T634 = ~ T635;
  assign T635 = 9'hff;
  assign T1299 = T1300 ? 119'h7fffffffffffffffffffffffffffff : 119'h0;
  assign T1300 = T634[8];
  assign T1301 = rst ? T636 : T637;
  assign T637 = T638 ? reg_w_data : regIntR_0;
  assign T638 = reg_w_en & T639;
  assign T639 = T5[0];
  assign T1302 = rst ? T640 : T641;
  assign T641 = T642 ? reg_w_data : regIntR_1;
  assign T642 = reg_w_en & T643;
  assign T643 = T5[1];
  assign T1303 = rst ? T644 : T645;
  assign T645 = T646 ? reg_w_data : regIntR_2;
  assign T646 = reg_w_en & T647;
  assign T647 = T5[2];
  assign T1304 = rst ? T648 : T649;
  assign T649 = T650 ? reg_w_data : regIntR_3;
  assign T650 = reg_w_en & T651;
  assign T651 = T5[3];
  assign T1305 = rst ? T652 : T653;
  assign T653 = T654 ? reg_w_data : regIntR_4;
  assign T654 = reg_w_en & T655;
  assign T655 = T5[4];
  assign T1306 = rst ? T656 : T657;
  assign T657 = T658 ? reg_w_data : regIntR_5;
  assign T658 = reg_w_en & T659;
  assign T659 = T5[5];
  assign T1307 = rst ? T660 : T661;
  assign T661 = T662 ? reg_w_data : regIntR_6;
  assign T662 = reg_w_en & T663;
  assign T663 = T5[6];
  assign T1308 = rst ? T664 : T665;
  assign T665 = T666 ? reg_w_data : regIntR_7;
  assign T666 = reg_w_en & T667;
  assign T667 = T5[7];
  assign T1309 = rst ? T668 : T669;
  assign T669 = T670 ? reg_w_data : regIntR_8;
  assign T670 = reg_w_en & T671;
  assign T671 = T5[8];
  assign T1310 = rst ? T672 : T673;
  assign T673 = T674 ? reg_w_data : regIntR_9;
  assign T674 = reg_w_en & T675;
  assign T675 = T5[9];
  assign T1311 = rst ? T676 : T677;
  assign T677 = T678 ? reg_w_data : regIntR_10;
  assign T678 = reg_w_en & T679;
  assign T679 = T5[10];
  assign T1312 = rst ? T680 : T681;
  assign T681 = T682 ? reg_w_data : regIntR_11;
  assign T682 = reg_w_en & T683;
  assign T683 = T5[11];
  assign T1313 = rst ? T684 : T685;
  assign T685 = T686 ? reg_w_data : regIntR_12;
  assign T686 = reg_w_en & T687;
  assign T687 = T5[12];
  assign T1314 = rst ? T688 : T689;
  assign T689 = T690 ? reg_w_data : regIntR_13;
  assign T690 = reg_w_en & T691;
  assign T691 = T5[13];
  assign T1315 = rst ? T692 : T693;
  assign T693 = T694 ? reg_w_data : regIntR_14;
  assign T694 = reg_w_en & T695;
  assign T695 = T5[14];
  assign rx3_buffer_host = 6'h0;
  assign rx3_match_host = 8'h0;
  assign rx3_timestamp_host = rx3_timestamp_usr;
  assign rx3_crc_fail_host = rx3_crc_fail_usr;
  assign rx3_pkt_drop_host = rx3_pkt_drop_usr;
  assign rx3_err_host = rx3_err_usr;
  assign rx3_vld_host = rx3_vld_usr;
  assign rx3_len_host = rx3_len_usr;
  assign rx3_eof_host = rx3_eof_usr;
  assign rx3_sof_host = rx3_sof_usr;
  assign rx3_data_host = rx3_data_usr;
  assign tx3_ack_host = tx3_ack_usr;
  assign tx3_vld_usr = tx3_vld_host;
  assign tx3_len_usr = tx3_len_host;
  assign tx3_eof_usr = tx3_eof_host;
  assign tx3_sof_usr = tx3_sof_host;
  assign tx3_data_usr = tx3_data_host;
  assign rx2_buffer_host = 6'h0;
  assign rx2_match_host = 8'h0;
  assign rx2_timestamp_host = rx2_timestamp_usr;
  assign rx2_crc_fail_host = rx2_crc_fail_usr;
  assign rx2_pkt_drop_host = rx2_pkt_drop_usr;
  assign rx2_err_host = rx2_err_usr;
  assign rx2_vld_host = rx2_vld_usr;
  assign rx2_len_host = rx2_len_usr;
  assign rx2_eof_host = rx2_eof_usr;
  assign rx2_sof_host = rx2_sof_usr;
  assign rx2_data_host = rx2_data_usr;
  assign tx2_ack_host = tx2_ack_usr;
  assign tx2_vld_usr = tx2_vld_host;
  assign tx2_len_usr = tx2_len_host;
  assign tx2_eof_usr = tx2_eof_host;
  assign tx2_sof_usr = tx2_sof_host;
  assign tx2_data_usr = tx2_data_host;
  assign rx1_buffer_host = 6'h0;
  assign rx1_match_host = 8'h0;
  assign rx1_timestamp_host = rx1_timestamp_usr;
  assign rx1_crc_fail_host = rx1_crc_fail_usr;
  assign rx1_pkt_drop_host = rx1_pkt_drop_usr;
  assign rx1_err_host = rx1_err_usr;
  assign rx1_vld_host = rx1_vld_usr;
  assign rx1_len_host = rx1_len_usr;
  assign rx1_eof_host = rx1_eof_usr;
  assign rx1_sof_host = rx1_sof_usr;
  assign rx1_data_host = rx1_data_usr;
  assign tx1_ack_host = tx1_ack_usr;
  assign tx1_vld_usr = T696;
  assign T696 = fifoTxOut_io_deq_valid & sending;
  assign tx1_len_usr = len;
  assign len = T697;
  assign T697 = eof ? 3'h4 : 3'h0;
  assign tx1_eof_usr = T698;
  assign T698 = eof & tx1_vld_usr;
  assign tx1_sof_usr = T699;
  assign T699 = sof & tx1_vld_usr;
  assign sof = segmentCounter == 4'h0;
  assign tx1_data_usr = tx1Output;
  assign tx1Output = T1316;
  assign T1316 = T700[63:0];
  assign T700 = T703 | T1317;
  assign T1317 = {1'h0, T701};
  assign T701 = T702 << 6'h38;
  assign T702 = fifoTxOut_io_deq_bits_7 & 8'hff;
  assign T703 = T1318 & T704;
  assign T704 = ~ T705;
  assign T705 = 65'hff00000000000000;
  assign T1318 = {1'h0, T706};
  assign T706 = T709 | T1319;
  assign T1319 = {8'h0, T707};
  assign T707 = T708 << 6'h30;
  assign T708 = fifoTxOut_io_deq_bits_6 & 8'hff;
  assign T709 = T712 & T1320;
  assign T1320 = {T1321, T710};
  assign T710 = ~ T711;
  assign T711 = 57'hff000000000000;
  assign T1321 = T1322 ? 7'h7f : 7'h0;
  assign T1322 = T710[56];
  assign T712 = T715 | T1323;
  assign T1323 = {16'h0, T713};
  assign T713 = T714 << 6'h28;
  assign T714 = fifoTxOut_io_deq_bits_5 & 8'hff;
  assign T715 = T718 & T1324;
  assign T1324 = {T1325, T716};
  assign T716 = ~ T717;
  assign T717 = 49'hff0000000000;
  assign T1325 = T1326 ? 15'h7fff : 15'h0;
  assign T1326 = T716[48];
  assign T718 = T721 | T1327;
  assign T1327 = {24'h0, T719};
  assign T719 = T720 << 6'h20;
  assign T720 = fifoTxOut_io_deq_bits_4 & 8'hff;
  assign T721 = T724 & T1328;
  assign T1328 = {T1329, T722};
  assign T722 = ~ T723;
  assign T723 = 41'hff00000000;
  assign T1329 = T1330 ? 23'h7fffff : 23'h0;
  assign T1330 = T722[40];
  assign T724 = T727 | T1331;
  assign T1331 = {32'h0, T725};
  assign T725 = T726 << 5'h18;
  assign T726 = fifoTxOut_io_deq_bits_3 & 8'hff;
  assign T727 = T730 & T1332;
  assign T1332 = {T1333, T728};
  assign T728 = ~ T729;
  assign T729 = 33'hff000000;
  assign T1333 = T1334 ? 31'h7fffffff : 31'h0;
  assign T1334 = T728[32];
  assign T730 = T733 | T1335;
  assign T1335 = {40'h0, T731};
  assign T731 = T732 << 5'h10;
  assign T732 = fifoTxOut_io_deq_bits_2 & 8'hff;
  assign T733 = T736 & T1336;
  assign T1336 = {T1337, T734};
  assign T734 = ~ T735;
  assign T735 = 25'hff0000;
  assign T1337 = T1338 ? 39'h7fffffffff : 39'h0;
  assign T1338 = T734[24];
  assign T736 = T739 | T1339;
  assign T1339 = {48'h0, T737};
  assign T737 = T738 << 4'h8;
  assign T738 = fifoTxOut_io_deq_bits_1 & 8'hff;
  assign T739 = T742 & T1340;
  assign T1340 = {T1341, T740};
  assign T740 = ~ T741;
  assign T741 = 17'hff00;
  assign T1341 = T1342 ? 47'h7fffffffffff : 47'h0;
  assign T1342 = T740[16];
  assign T742 = T745 | T1343;
  assign T1343 = {56'h0, T743};
  assign T743 = T744 << 1'h0;
  assign T744 = fifoTxOut_io_deq_bits_0 & 8'hff;
  assign T745 = 64'h0 & T1344;
  assign T1344 = {T1345, T746};
  assign T746 = ~ T747;
  assign T747 = 9'hff;
  assign T1345 = T1346 ? 55'h7fffffffffffff : 55'h0;
  assign T1346 = T746[8];
  assign rx0_buffer_host = 6'h0;
  assign rx0_match_host = 8'h0;
  assign rx0_timestamp_host = rx0_timestamp_usr;
  assign rx0_crc_fail_host = rx0_crc_fail_usr;
  assign rx0_pkt_drop_host = rx0_pkt_drop_usr;
  assign rx0_err_host = rx0_err_usr;
  assign rx0_vld_host = rx0_vld_usr;
  assign rx0_len_host = rx0_len_usr;
  assign rx0_eof_host = rx0_eof_usr;
  assign rx0_sof_host = rx0_sof_usr;
  assign rx0_data_host = rx0_data_usr;
  assign tx0_ack_host = tx0_ack_usr;
  assign tx0_vld_usr = tx0_vld_host;
  assign tx0_len_usr = tx0_len_host;
  assign tx0_eof_usr = tx0_eof_host;
  assign tx0_sof_usr = tx0_sof_host;
  assign tx0_data_usr = tx0_data_host;
  assign reg_r_data = T748;
  assign T748 = T835 ? T780 : T749;
  assign T749 = T779 ? T765 : T750;
  assign T750 = T764 ? T758 : T751;
  assign T751 = T756 ? regIntW_1 : regIntW_0;
  assign T1347 = rst ? T752 : T753;
  assign T753 = userMod_io_regOutEn ? userMod_io_regOut_0 : regIntW_0;
  assign T1348 = rst ? T754 : T755;
  assign T755 = userMod_io_regOutEn ? userMod_io_regOut_1 : regIntW_1;
  assign T756 = T757[0];
  assign T757 = T1349;
  assign T1349 = reg_r_addr[3:0];
  assign T758 = T763 ? regIntW_3 : regIntW_2;
  assign T1350 = rst ? T759 : T760;
  assign T760 = userMod_io_regOutEn ? userMod_io_regOut_2 : regIntW_2;
  assign T1351 = rst ? T761 : T762;
  assign T762 = userMod_io_regOutEn ? userMod_io_regOut_3 : regIntW_3;
  assign T763 = T757[0];
  assign T764 = T757[1];
  assign T765 = T778 ? T772 : T766;
  assign T766 = T771 ? regIntW_5 : regIntW_4;
  assign T1352 = rst ? T767 : T768;
  assign T768 = userMod_io_regOutEn ? userMod_io_regOut_4 : regIntW_4;
  assign T1353 = rst ? T769 : T770;
  assign T770 = userMod_io_regOutEn ? userMod_io_regOut_5 : regIntW_5;
  assign T771 = T757[0];
  assign T772 = T777 ? regIntW_7 : regIntW_6;
  assign T1354 = rst ? T773 : T774;
  assign T774 = userMod_io_regOutEn ? userMod_io_regOut_6 : regIntW_6;
  assign T1355 = rst ? T775 : T776;
  assign T776 = userMod_io_regOutEn ? userMod_io_regOut_7 : regIntW_7;
  assign T777 = T757[0];
  assign T778 = T757[1];
  assign T779 = T757[2];
  assign T780 = T834 ? T795 : T781;
  assign T781 = T794 ? T788 : T782;
  assign T782 = T787 ? regIntW_9 : regIntW_8;
  assign T1356 = rst ? T783 : T784;
  assign T784 = userMod_io_regOutEn ? userMod_io_regOut_8 : regIntW_8;
  assign T1357 = rst ? T785 : T786;
  assign T786 = userMod_io_regOutEn ? userMod_io_regOut_9 : regIntW_9;
  assign T787 = T757[0];
  assign T788 = T793 ? regIntW_11 : regIntW_10;
  assign T1358 = rst ? T789 : T790;
  assign T790 = userMod_io_regOutEn ? userMod_io_regOut_10 : regIntW_10;
  assign T1359 = rst ? T791 : T792;
  assign T792 = userMod_io_regOutEn ? userMod_io_regOut_11 : regIntW_11;
  assign T793 = T757[0];
  assign T794 = T757[1];
  assign T795 = T833 ? T802 : T796;
  assign T796 = T801 ? regIntW_13 : regIntW_12;
  assign T1360 = rst ? T797 : T798;
  assign T798 = userMod_io_regOutEn ? userMod_io_regOut_12 : regIntW_12;
  assign T1361 = rst ? T799 : T800;
  assign T800 = userMod_io_regOutEn ? userMod_io_regOut_13 : regIntW_13;
  assign T801 = T757[0];
  assign T802 = T832 ? regIntW_15 : regIntW_14;
  assign T1362 = rst ? T803 : T804;
  assign T804 = userMod_io_regOutEn ? userMod_io_regOut_14 : regIntW_14;
  assign T1363 = rst ? T805 : T1364;
  assign T1364 = {9'h0, T806};
  assign T806 = {T830, error};
  assign error = T807;
  assign T807 = {T810, userErr};
  assign T1365 = rst ? 1'h0 : T808;
  assign T808 = errRst ? 1'h0 : T809;
  assign T809 = userErr | userMod_io_error;
  assign errRst = controlReg[0];
  assign T810 = {T814, dirOutFull};
  assign T1366 = rst ? 1'h0 : T811;
  assign T811 = errRst ? 1'h0 : T812;
  assign T812 = dirOutFull | T813;
  assign T813 = directOutputFifo_io_enq_ready ^ 1'h1;
  assign T814 = {T818, txFifoFull};
  assign T1367 = rst ? 1'h0 : T815;
  assign T815 = errRst ? 1'h0 : T816;
  assign T816 = txFifoFull | T817;
  assign T817 = fifoTxOut_io_enq_ready ^ 1'h1;
  assign T818 = {T822, fifoFull};
  assign T1368 = rst ? 1'h0 : T819;
  assign T819 = errRst ? 1'h0 : T820;
  assign T820 = fifoFull | T821;
  assign T821 = fifo_io_enq_ready ^ 1'h1;
  assign T822 = {T825, crcFail};
  assign T1369 = rst ? 1'h0 : T823;
  assign T823 = errRst ? 1'h0 : T824;
  assign T824 = crcFail | rx1_crc_fail_usr;
  assign T825 = {rx1Err, pktDrop};
  assign T1370 = rst ? 1'h0 : T826;
  assign T826 = errRst ? 1'h0 : T827;
  assign T827 = pktDrop | rx1_pkt_drop_usr;
  assign T1371 = rst ? 1'h0 : T828;
  assign T828 = errRst ? 1'h0 : T829;
  assign T829 = rx1Err | rx1_err_usr;
  assign T830 = {T831, segmentCounter};
  assign T831 = {directOutputFifo_io_count, fifoTxOut_io_count};
  assign T832 = T757[0];
  assign T833 = T757[1];
  assign T834 = T757[2];
  assign T835 = T757[3];
  SimpleAdderSuite_SimpleAdder_1 userMod(.clk(clk), .reset(T836),
       .io_dataIn_ready( userMod_io_dataIn_ready ),
       .io_dataIn_valid( fifo_io_deq_valid ),
       .io_dataIn_bits_7( fifo_io_deq_bits_7 ),
       .io_dataIn_bits_6( fifo_io_deq_bits_6 ),
       .io_dataIn_bits_5( fifo_io_deq_bits_5 ),
       .io_dataIn_bits_4( fifo_io_deq_bits_4 ),
       .io_dataIn_bits_3( fifo_io_deq_bits_3 ),
       .io_dataIn_bits_2( fifo_io_deq_bits_2 ),
       .io_dataIn_bits_1( fifo_io_deq_bits_1 ),
       .io_dataIn_bits_0( fifo_io_deq_bits_0 ),
       .io_regIn_14( regIntR_14 ),
       .io_regIn_13( regIntR_13 ),
       .io_regIn_12( regIntR_12 ),
       .io_regIn_11( regIntR_11 ),
       .io_regIn_10( regIntR_10 ),
       .io_regIn_9( regIntR_9 ),
       .io_regIn_8( regIntR_8 ),
       .io_regIn_7( regIntR_7 ),
       .io_regIn_6( regIntR_6 ),
       .io_regIn_5( regIntR_5 ),
       .io_regIn_4( regIntR_4 ),
       .io_regIn_3( regIntR_3 ),
       .io_regIn_2( regIntR_2 ),
       .io_regIn_1( regIntR_1 ),
       .io_regIn_0( regIntR_0 ),
       .io_regOut_14( userMod_io_regOut_14 ),
       .io_regOut_13( userMod_io_regOut_13 ),
       .io_regOut_12( userMod_io_regOut_12 ),
       .io_regOut_11( userMod_io_regOut_11 ),
       .io_regOut_10( userMod_io_regOut_10 ),
       .io_regOut_9( userMod_io_regOut_9 ),
       .io_regOut_8( userMod_io_regOut_8 ),
       .io_regOut_7( userMod_io_regOut_7 ),
       .io_regOut_6( userMod_io_regOut_6 ),
       .io_regOut_5( userMod_io_regOut_5 ),
       .io_regOut_4( userMod_io_regOut_4 ),
       .io_regOut_3( userMod_io_regOut_3 ),
       .io_regOut_2( userMod_io_regOut_2 ),
       .io_regOut_1( userMod_io_regOut_1 ),
       .io_regOut_0( userMod_io_regOut_0 ),
       .io_regOutEn( userMod_io_regOutEn ),
       .io_memAddr( userMod_io_memAddr ),
       .io_memData( T1195 ),
       .io_error( userMod_io_error ),
       .io_dataOut_ready( directOutputFifo_io_enq_ready ),
       .io_dataOut_valid( userMod_io_dataOut_valid ),
       .io_dataOut_bits( userMod_io_dataOut_bits )
  );
  StripCrc stripper(.clk(clk), .reset(rst),
       .io_in_data( rx1_data_usr ),
       .io_in_sof( rx1_sof_usr ),
       .io_in_eof( rx1_eof_usr ),
       .io_in_len( rx1_len_usr ),
       .io_in_vld( rx1_vld_usr ),
       .io_out_data( stripper_io_out_data ),
       //.io_out_sof(  )
       //.io_out_eof(  )
       .io_out_len( stripper_io_out_len ),
       .io_out_vld( stripper_io_out_vld )
  );
  DataCombiner combiner(.clk(clk), .reset(rst),
       .io_dataIn_7( T489 ),
       .io_dataIn_6( T488 ),
       .io_dataIn_5( T487 ),
       .io_dataIn_4( T486 ),
       .io_dataIn_3( T485 ),
       .io_dataIn_2( T484 ),
       .io_dataIn_1( T483 ),
       .io_dataIn_0( T482 ),
       .io_vld( stripper_io_out_vld ),
       .io_len( stripper_io_out_len ),
       .io_dataOut_7( combiner_io_dataOut_7 ),
       .io_dataOut_6( combiner_io_dataOut_6 ),
       .io_dataOut_5( combiner_io_dataOut_5 ),
       .io_dataOut_4( combiner_io_dataOut_4 ),
       .io_dataOut_3( combiner_io_dataOut_3 ),
       .io_dataOut_2( combiner_io_dataOut_2 ),
       .io_dataOut_1( combiner_io_dataOut_1 ),
       .io_dataOut_0( combiner_io_dataOut_0 ),
       .io_vldOut( combiner_io_vldOut )
  );
  Queue_0 fifo(.clk(clk), .reset(rst),
       .io_enq_ready( fifo_io_enq_ready ),
       .io_enq_valid( combiner_io_vldOut ),
       .io_enq_bits_7( combiner_io_dataOut_7 ),
       .io_enq_bits_6( combiner_io_dataOut_6 ),
       .io_enq_bits_5( combiner_io_dataOut_5 ),
       .io_enq_bits_4( combiner_io_dataOut_4 ),
       .io_enq_bits_3( combiner_io_dataOut_3 ),
       .io_enq_bits_2( combiner_io_dataOut_2 ),
       .io_enq_bits_1( combiner_io_dataOut_1 ),
       .io_enq_bits_0( combiner_io_dataOut_0 ),
       .io_deq_ready( userMod_io_dataIn_ready ),
       .io_deq_valid( fifo_io_deq_valid ),
       .io_deq_bits_7( fifo_io_deq_bits_7 ),
       .io_deq_bits_6( fifo_io_deq_bits_6 ),
       .io_deq_bits_5( fifo_io_deq_bits_5 ),
       .io_deq_bits_4( fifo_io_deq_bits_4 ),
       .io_deq_bits_3( fifo_io_deq_bits_3 ),
       .io_deq_bits_2( fifo_io_deq_bits_2 ),
       .io_deq_bits_1( fifo_io_deq_bits_1 ),
       .io_deq_bits_0( fifo_io_deq_bits_0 )
       //.io_count(  )
  );
  Queue_1 directOutputFifo(.clk(clk), .reset(rst),
       .io_enq_ready( directOutputFifo_io_enq_ready ),
       .io_enq_valid( userMod_io_dataOut_valid ),
       .io_enq_bits_7( vecDataOut_7 ),
       .io_enq_bits_6( vecDataOut_6 ),
       .io_enq_bits_5( vecDataOut_5 ),
       .io_enq_bits_4( vecDataOut_4 ),
       .io_enq_bits_3( vecDataOut_3 ),
       .io_enq_bits_2( vecDataOut_2 ),
       .io_enq_bits_1( vecDataOut_1 ),
       .io_enq_bits_0( vecDataOut_0 ),
       .io_deq_ready( outToBuffer_io_dataIn_ready ),
       .io_deq_valid( directOutputFifo_io_deq_valid ),
       .io_deq_bits_7( directOutputFifo_io_deq_bits_7 ),
       .io_deq_bits_6( directOutputFifo_io_deq_bits_6 ),
       .io_deq_bits_5( directOutputFifo_io_deq_bits_5 ),
       .io_deq_bits_4( directOutputFifo_io_deq_bits_4 ),
       .io_deq_bits_3( directOutputFifo_io_deq_bits_3 ),
       .io_deq_bits_2( directOutputFifo_io_deq_bits_2 ),
       .io_deq_bits_1( directOutputFifo_io_deq_bits_1 ),
       .io_deq_bits_0( directOutputFifo_io_deq_bits_0 ),
       .io_count( directOutputFifo_io_count )
  );
  Serializer outToBuffer(.clk(clk), .reset(rst),
       .io_dataIn_ready( outToBuffer_io_dataIn_ready ),
       .io_dataIn_valid( directOutputFifo_io_deq_valid ),
       .io_dataIn_bits_7( directOutputFifo_io_deq_bits_7 ),
       .io_dataIn_bits_6( directOutputFifo_io_deq_bits_6 ),
       .io_dataIn_bits_5( directOutputFifo_io_deq_bits_5 ),
       .io_dataIn_bits_4( directOutputFifo_io_deq_bits_4 ),
       .io_dataIn_bits_3( directOutputFifo_io_deq_bits_3 ),
       .io_dataIn_bits_2( directOutputFifo_io_deq_bits_2 ),
       .io_dataIn_bits_1( directOutputFifo_io_deq_bits_1 ),
       .io_dataIn_bits_0( directOutputFifo_io_deq_bits_0 ),
       .io_flush( flush ),
       .io_dataOut_valid( outToBuffer_io_dataOut_valid ),
       .io_dataOut_bits_63( outToBuffer_io_dataOut_bits_63 ),
       .io_dataOut_bits_62( outToBuffer_io_dataOut_bits_62 ),
       .io_dataOut_bits_61( outToBuffer_io_dataOut_bits_61 ),
       .io_dataOut_bits_60( outToBuffer_io_dataOut_bits_60 ),
       .io_dataOut_bits_59( outToBuffer_io_dataOut_bits_59 ),
       .io_dataOut_bits_58( outToBuffer_io_dataOut_bits_58 ),
       .io_dataOut_bits_57( outToBuffer_io_dataOut_bits_57 ),
       .io_dataOut_bits_56( outToBuffer_io_dataOut_bits_56 ),
       .io_dataOut_bits_55( outToBuffer_io_dataOut_bits_55 ),
       .io_dataOut_bits_54( outToBuffer_io_dataOut_bits_54 ),
       .io_dataOut_bits_53( outToBuffer_io_dataOut_bits_53 ),
       .io_dataOut_bits_52( outToBuffer_io_dataOut_bits_52 ),
       .io_dataOut_bits_51( outToBuffer_io_dataOut_bits_51 ),
       .io_dataOut_bits_50( outToBuffer_io_dataOut_bits_50 ),
       .io_dataOut_bits_49( outToBuffer_io_dataOut_bits_49 ),
       .io_dataOut_bits_48( outToBuffer_io_dataOut_bits_48 ),
       .io_dataOut_bits_47( outToBuffer_io_dataOut_bits_47 ),
       .io_dataOut_bits_46( outToBuffer_io_dataOut_bits_46 ),
       .io_dataOut_bits_45( outToBuffer_io_dataOut_bits_45 ),
       .io_dataOut_bits_44( outToBuffer_io_dataOut_bits_44 ),
       .io_dataOut_bits_43( outToBuffer_io_dataOut_bits_43 ),
       .io_dataOut_bits_42( outToBuffer_io_dataOut_bits_42 ),
       .io_dataOut_bits_41( outToBuffer_io_dataOut_bits_41 ),
       .io_dataOut_bits_40( outToBuffer_io_dataOut_bits_40 ),
       .io_dataOut_bits_39( outToBuffer_io_dataOut_bits_39 ),
       .io_dataOut_bits_38( outToBuffer_io_dataOut_bits_38 ),
       .io_dataOut_bits_37( outToBuffer_io_dataOut_bits_37 ),
       .io_dataOut_bits_36( outToBuffer_io_dataOut_bits_36 ),
       .io_dataOut_bits_35( outToBuffer_io_dataOut_bits_35 ),
       .io_dataOut_bits_34( outToBuffer_io_dataOut_bits_34 ),
       .io_dataOut_bits_33( outToBuffer_io_dataOut_bits_33 ),
       .io_dataOut_bits_32( outToBuffer_io_dataOut_bits_32 ),
       .io_dataOut_bits_31( outToBuffer_io_dataOut_bits_31 ),
       .io_dataOut_bits_30( outToBuffer_io_dataOut_bits_30 ),
       .io_dataOut_bits_29( outToBuffer_io_dataOut_bits_29 ),
       .io_dataOut_bits_28( outToBuffer_io_dataOut_bits_28 ),
       .io_dataOut_bits_27( outToBuffer_io_dataOut_bits_27 ),
       .io_dataOut_bits_26( outToBuffer_io_dataOut_bits_26 ),
       .io_dataOut_bits_25( outToBuffer_io_dataOut_bits_25 ),
       .io_dataOut_bits_24( outToBuffer_io_dataOut_bits_24 ),
       .io_dataOut_bits_23( outToBuffer_io_dataOut_bits_23 ),
       .io_dataOut_bits_22( outToBuffer_io_dataOut_bits_22 ),
       .io_dataOut_bits_21( outToBuffer_io_dataOut_bits_21 ),
       .io_dataOut_bits_20( outToBuffer_io_dataOut_bits_20 ),
       .io_dataOut_bits_19( outToBuffer_io_dataOut_bits_19 ),
       .io_dataOut_bits_18( outToBuffer_io_dataOut_bits_18 ),
       .io_dataOut_bits_17( outToBuffer_io_dataOut_bits_17 ),
       .io_dataOut_bits_16( outToBuffer_io_dataOut_bits_16 ),
       .io_dataOut_bits_15( outToBuffer_io_dataOut_bits_15 ),
       .io_dataOut_bits_14( outToBuffer_io_dataOut_bits_14 ),
       .io_dataOut_bits_13( outToBuffer_io_dataOut_bits_13 ),
       .io_dataOut_bits_12( outToBuffer_io_dataOut_bits_12 ),
       .io_dataOut_bits_11( outToBuffer_io_dataOut_bits_11 ),
       .io_dataOut_bits_10( outToBuffer_io_dataOut_bits_10 ),
       .io_dataOut_bits_9( outToBuffer_io_dataOut_bits_9 ),
       .io_dataOut_bits_8( outToBuffer_io_dataOut_bits_8 ),
       .io_dataOut_bits_7( outToBuffer_io_dataOut_bits_7 ),
       .io_dataOut_bits_6( outToBuffer_io_dataOut_bits_6 ),
       .io_dataOut_bits_5( outToBuffer_io_dataOut_bits_5 ),
       .io_dataOut_bits_4( outToBuffer_io_dataOut_bits_4 ),
       .io_dataOut_bits_3( outToBuffer_io_dataOut_bits_3 ),
       .io_dataOut_bits_2( outToBuffer_io_dataOut_bits_2 ),
       .io_dataOut_bits_1( outToBuffer_io_dataOut_bits_1 ),
       .io_dataOut_bits_0( outToBuffer_io_dataOut_bits_0 )
       //.io_flushed(  )
  );
  Queue_0 fifoTxOut(.clk(clk), .reset(rst),
       .io_enq_ready( fifoTxOut_io_enq_ready ),
       .io_enq_valid( bufferVld ),
       .io_enq_bits_7( bufferByte_7 ),
       .io_enq_bits_6( bufferByte_6 ),
       .io_enq_bits_5( bufferByte_5 ),
       .io_enq_bits_4( bufferByte_4 ),
       .io_enq_bits_3( bufferByte_3 ),
       .io_enq_bits_2( bufferByte_2 ),
       .io_enq_bits_1( bufferByte_1 ),
       .io_enq_bits_0( bufferByte_0 ),
       .io_deq_ready( T0 ),
       .io_deq_valid( fifoTxOut_io_deq_valid ),
       .io_deq_bits_7( fifoTxOut_io_deq_bits_7 ),
       .io_deq_bits_6( fifoTxOut_io_deq_bits_6 ),
       .io_deq_bits_5( fifoTxOut_io_deq_bits_5 ),
       .io_deq_bits_4( fifoTxOut_io_deq_bits_4 ),
       .io_deq_bits_3( fifoTxOut_io_deq_bits_3 ),
       .io_deq_bits_2( fifoTxOut_io_deq_bits_2 ),
       .io_deq_bits_1( fifoTxOut_io_deq_bits_1 ),
       .io_deq_bits_0( fifoTxOut_io_deq_bits_0 ),
       .io_count( fifoTxOut_io_count )
  );

  always @(posedge clk) begin
    if(rst) begin
      controlReg <= T1;
    end else if(T3) begin
      controlReg <= reg_w_data;
    end
    if(rst) begin
      sending <= 1'h0;
    end else if(T11) begin
      sending <= 1'h0;
    end else begin
      sending <= T9;
    end
    if(rst) begin
      segmentCounter <= 4'h0;
    end else if(fifoDrain) begin
      segmentCounter <= 4'h0;
    end else if(T19) begin
      segmentCounter <= 4'h0;
    end else if(T18) begin
      segmentCounter <= T17;
    end
    buffer_7 <= outToBuffer_io_dataOut_bits_7;
    buffer_6 <= outToBuffer_io_dataOut_bits_6;
    buffer_5 <= outToBuffer_io_dataOut_bits_5;
    buffer_4 <= outToBuffer_io_dataOut_bits_4;
    buffer_3 <= outToBuffer_io_dataOut_bits_3;
    buffer_2 <= outToBuffer_io_dataOut_bits_2;
    buffer_1 <= outToBuffer_io_dataOut_bits_1;
    buffer_0 <= outToBuffer_io_dataOut_bits_0;
    buffer_15 <= outToBuffer_io_dataOut_bits_15;
    buffer_14 <= outToBuffer_io_dataOut_bits_14;
    buffer_13 <= outToBuffer_io_dataOut_bits_13;
    buffer_12 <= outToBuffer_io_dataOut_bits_12;
    buffer_11 <= outToBuffer_io_dataOut_bits_11;
    buffer_10 <= outToBuffer_io_dataOut_bits_10;
    buffer_9 <= outToBuffer_io_dataOut_bits_9;
    buffer_8 <= outToBuffer_io_dataOut_bits_8;
    buffer_23 <= outToBuffer_io_dataOut_bits_23;
    buffer_22 <= outToBuffer_io_dataOut_bits_22;
    buffer_21 <= outToBuffer_io_dataOut_bits_21;
    buffer_20 <= outToBuffer_io_dataOut_bits_20;
    buffer_19 <= outToBuffer_io_dataOut_bits_19;
    buffer_18 <= outToBuffer_io_dataOut_bits_18;
    buffer_17 <= outToBuffer_io_dataOut_bits_17;
    buffer_16 <= outToBuffer_io_dataOut_bits_16;
    buffer_31 <= outToBuffer_io_dataOut_bits_31;
    buffer_30 <= outToBuffer_io_dataOut_bits_30;
    buffer_29 <= outToBuffer_io_dataOut_bits_29;
    buffer_28 <= outToBuffer_io_dataOut_bits_28;
    buffer_27 <= outToBuffer_io_dataOut_bits_27;
    buffer_26 <= outToBuffer_io_dataOut_bits_26;
    buffer_25 <= outToBuffer_io_dataOut_bits_25;
    buffer_24 <= outToBuffer_io_dataOut_bits_24;
    buffer_39 <= outToBuffer_io_dataOut_bits_39;
    buffer_38 <= outToBuffer_io_dataOut_bits_38;
    buffer_37 <= outToBuffer_io_dataOut_bits_37;
    buffer_36 <= outToBuffer_io_dataOut_bits_36;
    buffer_35 <= outToBuffer_io_dataOut_bits_35;
    buffer_34 <= outToBuffer_io_dataOut_bits_34;
    buffer_33 <= outToBuffer_io_dataOut_bits_33;
    buffer_32 <= outToBuffer_io_dataOut_bits_32;
    buffer_47 <= outToBuffer_io_dataOut_bits_47;
    buffer_46 <= outToBuffer_io_dataOut_bits_46;
    buffer_45 <= outToBuffer_io_dataOut_bits_45;
    buffer_44 <= outToBuffer_io_dataOut_bits_44;
    buffer_43 <= outToBuffer_io_dataOut_bits_43;
    buffer_42 <= outToBuffer_io_dataOut_bits_42;
    buffer_41 <= outToBuffer_io_dataOut_bits_41;
    buffer_40 <= outToBuffer_io_dataOut_bits_40;
    buffer_55 <= outToBuffer_io_dataOut_bits_55;
    buffer_54 <= outToBuffer_io_dataOut_bits_54;
    buffer_53 <= outToBuffer_io_dataOut_bits_53;
    buffer_52 <= outToBuffer_io_dataOut_bits_52;
    buffer_51 <= outToBuffer_io_dataOut_bits_51;
    buffer_50 <= outToBuffer_io_dataOut_bits_50;
    buffer_49 <= outToBuffer_io_dataOut_bits_49;
    buffer_48 <= outToBuffer_io_dataOut_bits_48;
    buffer_63 <= outToBuffer_io_dataOut_bits_63;
    buffer_62 <= outToBuffer_io_dataOut_bits_62;
    buffer_61 <= outToBuffer_io_dataOut_bits_61;
    buffer_60 <= outToBuffer_io_dataOut_bits_60;
    buffer_59 <= outToBuffer_io_dataOut_bits_59;
    buffer_58 <= outToBuffer_io_dataOut_bits_58;
    buffer_57 <= outToBuffer_io_dataOut_bits_57;
    buffer_56 <= outToBuffer_io_dataOut_bits_56;
    bufferVld <= outToBuffer_io_dataOut_valid;
    if(rst) begin
      buffCount <= 7'h0;
    end else if(flush) begin
      buffCount <= 7'h0;
    end else if(T473) begin
      buffCount <= T472;
    end
    if (1'h1)
      userMem[T1238] <= T1198;
    if(rst) begin
      regIntR_0 <= T636;
    end else if(T638) begin
      regIntR_0 <= reg_w_data;
    end
    if(rst) begin
      regIntR_1 <= T640;
    end else if(T642) begin
      regIntR_1 <= reg_w_data;
    end
    if(rst) begin
      regIntR_2 <= T644;
    end else if(T646) begin
      regIntR_2 <= reg_w_data;
    end
    if(rst) begin
      regIntR_3 <= T648;
    end else if(T650) begin
      regIntR_3 <= reg_w_data;
    end
    if(rst) begin
      regIntR_4 <= T652;
    end else if(T654) begin
      regIntR_4 <= reg_w_data;
    end
    if(rst) begin
      regIntR_5 <= T656;
    end else if(T658) begin
      regIntR_5 <= reg_w_data;
    end
    if(rst) begin
      regIntR_6 <= T660;
    end else if(T662) begin
      regIntR_6 <= reg_w_data;
    end
    if(rst) begin
      regIntR_7 <= T664;
    end else if(T666) begin
      regIntR_7 <= reg_w_data;
    end
    if(rst) begin
      regIntR_8 <= T668;
    end else if(T670) begin
      regIntR_8 <= reg_w_data;
    end
    if(rst) begin
      regIntR_9 <= T672;
    end else if(T674) begin
      regIntR_9 <= reg_w_data;
    end
    if(rst) begin
      regIntR_10 <= T676;
    end else if(T678) begin
      regIntR_10 <= reg_w_data;
    end
    if(rst) begin
      regIntR_11 <= T680;
    end else if(T682) begin
      regIntR_11 <= reg_w_data;
    end
    if(rst) begin
      regIntR_12 <= T684;
    end else if(T686) begin
      regIntR_12 <= reg_w_data;
    end
    if(rst) begin
      regIntR_13 <= T688;
    end else if(T690) begin
      regIntR_13 <= reg_w_data;
    end
    if(rst) begin
      regIntR_14 <= T692;
    end else if(T694) begin
      regIntR_14 <= reg_w_data;
    end
    if(rst) begin
      regIntW_0 <= T752;
    end else if(userMod_io_regOutEn) begin
      regIntW_0 <= userMod_io_regOut_0;
    end
    if(rst) begin
      regIntW_1 <= T754;
    end else if(userMod_io_regOutEn) begin
      regIntW_1 <= userMod_io_regOut_1;
    end
    if(rst) begin
      regIntW_2 <= T759;
    end else if(userMod_io_regOutEn) begin
      regIntW_2 <= userMod_io_regOut_2;
    end
    if(rst) begin
      regIntW_3 <= T761;
    end else if(userMod_io_regOutEn) begin
      regIntW_3 <= userMod_io_regOut_3;
    end
    if(rst) begin
      regIntW_4 <= T767;
    end else if(userMod_io_regOutEn) begin
      regIntW_4 <= userMod_io_regOut_4;
    end
    if(rst) begin
      regIntW_5 <= T769;
    end else if(userMod_io_regOutEn) begin
      regIntW_5 <= userMod_io_regOut_5;
    end
    if(rst) begin
      regIntW_6 <= T773;
    end else if(userMod_io_regOutEn) begin
      regIntW_6 <= userMod_io_regOut_6;
    end
    if(rst) begin
      regIntW_7 <= T775;
    end else if(userMod_io_regOutEn) begin
      regIntW_7 <= userMod_io_regOut_7;
    end
    if(rst) begin
      regIntW_8 <= T783;
    end else if(userMod_io_regOutEn) begin
      regIntW_8 <= userMod_io_regOut_8;
    end
    if(rst) begin
      regIntW_9 <= T785;
    end else if(userMod_io_regOutEn) begin
      regIntW_9 <= userMod_io_regOut_9;
    end
    if(rst) begin
      regIntW_10 <= T789;
    end else if(userMod_io_regOutEn) begin
      regIntW_10 <= userMod_io_regOut_10;
    end
    if(rst) begin
      regIntW_11 <= T791;
    end else if(userMod_io_regOutEn) begin
      regIntW_11 <= userMod_io_regOut_11;
    end
    if(rst) begin
      regIntW_12 <= T797;
    end else if(userMod_io_regOutEn) begin
      regIntW_12 <= userMod_io_regOut_12;
    end
    if(rst) begin
      regIntW_13 <= T799;
    end else if(userMod_io_regOutEn) begin
      regIntW_13 <= userMod_io_regOut_13;
    end
    if(rst) begin
      regIntW_14 <= T803;
    end else if(userMod_io_regOutEn) begin
      regIntW_14 <= userMod_io_regOut_14;
    end
    if(rst) begin
      regIntW_15 <= T805;
    end else begin
      regIntW_15 <= T1364;
    end
    if(rst) begin
      userErr <= 1'h0;
    end else if(errRst) begin
      userErr <= 1'h0;
    end else begin
      userErr <= T809;
    end
    if(rst) begin
      dirOutFull <= 1'h0;
    end else if(errRst) begin
      dirOutFull <= 1'h0;
    end else begin
      dirOutFull <= T812;
    end
    if(rst) begin
      txFifoFull <= 1'h0;
    end else if(errRst) begin
      txFifoFull <= 1'h0;
    end else begin
      txFifoFull <= T816;
    end
    if(rst) begin
      fifoFull <= 1'h0;
    end else if(errRst) begin
      fifoFull <= 1'h0;
    end else begin
      fifoFull <= T820;
    end
    if(rst) begin
      crcFail <= 1'h0;
    end else if(errRst) begin
      crcFail <= 1'h0;
    end else begin
      crcFail <= T824;
    end
    if(rst) begin
      pktDrop <= 1'h0;
    end else if(errRst) begin
      pktDrop <= 1'h0;
    end else begin
      pktDrop <= T827;
    end
    if(rst) begin
      rx1Err <= 1'h0;
    end else if(errRst) begin
      rx1Err <= 1'h0;
    end else begin
      rx1Err <= T829;
    end
  end
endmodule

