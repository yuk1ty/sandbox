`include "alu_d.v"
module opstack_tb;
  reg clk, num, op;
  reg [15:0] x;
  wire [15:0] qtop;

  opstack opstack0(clk, num, op, x, qtop);

  initial clk=0;
  always #50 clk=~clk;

  initial begin
    $dumpvars;
    num=1; op=0; x=2; #100
    num=1; op=0; x=3; #100
    num=1; op=0; x=4; #100
    num=0; op=1; x=`MUL; #100
    num=0; op=1; x=`ADD; #100
    num=0; op=1; x=`NEG; #100
    num=1; op=0; x=5; #100
    num=0; op=1; x=`NEG; #100
    num=1; op=0; x=6; #100
    num=1; op=0; x=7; #100
    num=0; op=1; x=`GT; #100
    num=0; op=1; x=`OR; #100
    $finish;
  end
endmodule
