module counter_tb;
  reg clk, rst_n, load, inc;
  reg [7:0] d;
  wire [7:0] q;

  counter #(8) counter0(clk, rst_n, load, inc, d, q);  

  initial begin
    $dumpvars;
    rst_n=0; load=0; inc=0; d=0; #100
    rst_n=1; inc=1; #200
    inc=0; load=1; d=123; #100 // write
    inc=1; load=0; #200
    inc=0; load=1; d=254; #100 // write
    inc=1; load=0; #300
    rst_n=0; #100
    $finish;
  end
endmodule
