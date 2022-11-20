module dff(clk, d, q);
  input clk, d;
  output q;
  reg q;

  always @(posedge clk) q <= d;
endmodule
