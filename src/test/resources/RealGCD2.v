`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif

module RealGCD2(
  input   clock,
  input   reset,
  output  io_in_ready,
  input   io_in_valid,
  input  [15:0] io_in_bits_a,
  input  [15:0] io_in_bits_b,
  output  io_out_valid,
  output [15:0] io_out_bits
);
  reg [15:0] x;
  reg [31:0] GEN_9;
  reg [15:0] y;
  reg [31:0] GEN_10;
  reg  p;
  reg [31:0] GEN_11;
  reg [15:0] ti;
  reg [31:0] GEN_12;
  wire [16:0] _T_38;
  wire [15:0] _T_39;
  wire  _T_41;
  wire  _T_44;
  wire [15:0] GEN_0;
  wire [15:0] GEN_1;
  wire  GEN_2;
  wire  _T_46;
  wire [15:0] GEN_3;
  wire [15:0] GEN_4;
  wire  _T_48;
  wire [16:0] _T_49;
  wire [15:0] _T_50;
  wire [15:0] GEN_5;
  wire [15:0] GEN_6;
  wire [15:0] GEN_7;
  wire  _T_52;
  wire  _T_53;
  wire  GEN_8;
  assign io_in_ready = _T_41;
  assign io_out_valid = _T_53;
  assign io_out_bits = x;
  assign _T_38 = ti + 16'h1;
  assign _T_39 = _T_38[15:0];
  assign _T_41 = p == 1'h0;
  assign _T_44 = io_in_valid & _T_41;
  assign GEN_0 = _T_44 ? io_in_bits_a : x;
  assign GEN_1 = _T_44 ? io_in_bits_b : y;
  assign GEN_2 = _T_44 ? 1'h1 : p;
  assign _T_46 = x > y;
  assign GEN_3 = _T_46 ? y : GEN_0;
  assign GEN_4 = _T_46 ? x : GEN_1;
  assign _T_48 = _T_46 == 1'h0;
  assign _T_49 = y - x;
  assign _T_50 = _T_49[15:0];
  assign GEN_5 = _T_48 ? _T_50 : GEN_4;
  assign GEN_6 = p ? GEN_3 : GEN_0;
  assign GEN_7 = p ? GEN_5 : GEN_1;
  assign _T_52 = y == 16'h0;
  assign _T_53 = _T_52 & p;
  assign GEN_8 = io_out_valid ? 1'h0 : GEN_2;
`ifdef RANDOMIZE
  integer initvar;
  initial begin
    `ifndef verilator
      #0.002 begin end
    `endif
  `ifdef RANDOMIZE_REG_INIT
  GEN_9 = {1{$random}};
  x = GEN_9[15:0];
  `endif
  `ifdef RANDOMIZE_REG_INIT
  GEN_10 = {1{$random}};
  y = GEN_10[15:0];
  `endif
  `ifdef RANDOMIZE_REG_INIT
  GEN_11 = {1{$random}};
  p = GEN_11[0:0];
  `endif
  `ifdef RANDOMIZE_REG_INIT
  GEN_12 = {1{$random}};
  ti = GEN_12[15:0];
  `endif
  end
`endif
  always @(posedge clock) begin
    if (p) begin
      if (_T_46) begin
        x <= y;
      end else begin
        if (_T_44) begin
          x <= io_in_bits_a;
        end
      end
    end else begin
      if (_T_44) begin
        x <= io_in_bits_a;
      end
    end
    if (p) begin
      if (_T_48) begin
        y <= _T_50;
      end else begin
        if (_T_46) begin
          y <= x;
        end else begin
          if (_T_44) begin
            y <= io_in_bits_b;
          end
        end
      end
    end else begin
      if (_T_44) begin
        y <= io_in_bits_b;
      end
    end
    if (reset) begin
      p <= 1'h0;
    end else begin
      if (io_out_valid) begin
        p <= 1'h0;
      end else begin
        if (_T_44) begin
          p <= 1'h1;
        end
      end
    end
    if (reset) begin
      ti <= 16'h0;
    end else begin
      ti <= _T_39;
    end
  end
endmodule
