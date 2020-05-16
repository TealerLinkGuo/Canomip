// Generator : SpinalHDL v1.4.0    git head : ecb5a80b713566f417ea3ea061f9969e73770a7f
// Date      : 16/05/2020, 23:07:59
// Component : test



module test (
  input      [63:0]   io_a,
  output     [63:0]   io_b,
  input               clk 
);
  wire                _zz_1_;
  wire                _zz_2_;
  wire       [11:0]   _zz_3_;
  wire                _zz_4_;
  wire                _zz_5_;
  wire       [31:0]   _zz_6_;
  wire       [11:0]   _zz_7_;
  wire       [63:0]   abc_doa;
  wire       [31:0]   abc1_doa;

  EG4_csr_reg_bram_64 abc ( 
    .clka     (clk            ), //i
    .wea      (_zz_1_         ), //i
    .cea      (_zz_2_         ), //i
    .dia      (io_a[63:0]     ), //i
    .addra    (_zz_3_[11:0]   ), //i
    .doa      (abc_doa[63:0]  )  //o
  );
  EG4_csr_reg_bram abc1 ( 
    .clka     (clk             ), //i
    .wea      (_zz_4_          ), //i
    .cea      (_zz_5_          ), //i
    .dia      (_zz_6_[31:0]    ), //i
    .addra    (_zz_7_[11:0]    ), //i
    .doa      (abc1_doa[31:0]  )  //o
  );
  assign _zz_1_ = 1'b0;
  assign _zz_2_ = 1'b0;
  assign _zz_3_ = 12'h0;
  assign io_b = abc_doa;
  assign _zz_4_ = 1'b0;
  assign _zz_5_ = 1'b0;
  assign _zz_6_ = io_a[31 : 0];
  assign _zz_7_ = 12'h0;

endmodule
