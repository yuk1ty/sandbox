module adder(x, y, s);
  parameter N=8;
  
  input [N-1:0] x, y;
  output [N:0] s;
  reg [N:0] s;

  always @(x, y) s=x+y;
endmodule
