`include "state_d.v"
module fetch(clk, rst_n, run, halt, irout);
  input clk, rst_n, run, halt;
  output [15:0] cs;
  wire [1:0] cs;
  wire [11:0] pcout;
  wire [15:0] ramout;

  state state0(.clk(clk), .rst_n(rst_n), .run(run), .halt(halt), .q(cs));
  counter #(12) pc0(.clk(clk), .rst_n(rst_n), .load(1'b0), .inc(cs==`FETCH), .q(pcout));
  counter #(16) ir0(.clk(clk), .rst_n(rst_n), .load(cs==`FETCH), .inc(1'b0), .d(ramout), .q(pcout));
  ram ram0(.clk(clk), .load(1'b0), .addr(pcout), .q(ramout));
endmodule
