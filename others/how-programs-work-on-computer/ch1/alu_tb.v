`include "alu_d.v"
module alu_tb;
  reg signed [15:0] a, b;
  reg [4:0] f;
  wire signed [15:0] s;

  alu alu0(a, b, f, s);

  initial begin
    $dumpvars;
    a=5; b=-10;
    f=`ADD; #100;
    f=`SUB; #100;
    f=`MUL; #100;
    f=`SHL; #100;
    f=`SHR; #100;
    f=`BAND; #100;
    f=`BOR; #100;
    f=`BXOR; #100;
    f=`AND; #100;
    f=`OR; #100;
    f=`EQ; #100;
    f=`NE; #100;
    f=`GE; #100;
    f=`LE; #100;
    f=`GT; #100;
    f=`LT; #100;
    f=`NEG; #100;
    f=`BNOT; #100;
    f=`NOT; #100;
    $finish;
  end
endmodule
