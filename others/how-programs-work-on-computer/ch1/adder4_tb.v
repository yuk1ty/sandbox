module adder4_tb;
  reg [3:0] x, y;
  wire [4:0] s;

  adder4 adder4_0(x, y, s);

  initial begin
    $dumpvars;
    x = 0; y = 3; #100
    x = 3; y = 6; #100
    x = 4; y = 8; #100
    x = 5; y = 11; #100
    x = 7; y = 14; #100
    x = 9; y = 2; #100
    x = 12; y = 8; #100
    x = 13; y = 11; #100
    x = 15; y = 14; #100
    x = 13; y = 15; #100
    $finish;
  end
endmodule
