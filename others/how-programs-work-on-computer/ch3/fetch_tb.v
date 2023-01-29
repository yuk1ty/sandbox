module fetch_tb;
  reg clk, rst_n, run, halt;
  wire [15:0] irout;

  fetch fetch0(clk, rst_n, run, halt, irout);

  initial clk=0;
endmodule
