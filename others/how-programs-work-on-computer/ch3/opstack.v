module opstack(clk, num, op, x, qtop);
  input clk, num, op;
  input [15:0] x;
  output [15:0] qtop;
  wire [15:0] qnext, s;
  wire load, push, pop;
  reg [15:0] d;

  alu alu0(.a(qtop), .b(qnext), .f(x[4:0]), .s(s));
  stack stack0(clk, load, push, pop, d, qtop, qnext);

  assign load=num|op;
  assign push=num;
  assign pop=op&~x[4];

  always @(num, op, x, s)
    if (num) d=x;
    else if (op) d=s;
    else d=16'hXXXX;
endmodule
