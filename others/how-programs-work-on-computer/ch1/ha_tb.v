`timescale 1ns/1ns
module ha_tb;
  reg x, y;
  wire s, c;

  ha ha0(.x(x), .y(y), .s(s), .c(c));

  initial begin
    $dumpvars;
    x=0; y=0; #100
    x=0; y=1; #100
    x=1; y=0; #100
    x=1; y=1; #100
    $finish;
  end
endmodule
