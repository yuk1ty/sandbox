module state_tb;
  reg clk, rst_n, run, halt;
  wire [1:0] q;

  state state0(clk, rst_n, run, halt, q);

  initial clk=0;
  always #50 clk=~clk;

  initial begin
    $dumpvars;
    rst_n = 0; run = 0; halt = 0; #100
    rst_n = 1; run = 1; #100
    run = 0; #600
    halt = 1; #300
    $finish;
  end
endmodule
