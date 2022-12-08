`include "alu_d.v"
module alu(a, b, f, s);
  input signed [15:0] a, b; // an input port with 16 bits
  input [4:0] f; // an input port with 4 bits
  output signed [15:0] s; // an output port with 16 bits
  reg signed [15:0] s; // a variable with 16 bits

  always @(a, b, f)
    case(f)
      `ADD  : s = b + a;
      `SUB  : s = b - a;
      `MUL  : s = b * a;
      `SHL  : s = b << a;
      `SHR  : s = b >> a;
      `BAND : s = b & a;
      `BOR  : s = b | a;
      `BXOR : s = b ^ a;
      `AND  : s = b && a;
      `OR   : s = b || a;
      `EQ   : s = b == a;
      `NE   : s = b != a;
      `GE   : s = b >= a;
      `LE   : s = b <= a;
      `GT   : s = b > a;
      `LT   : s = b < a;
      `NEG  : s = -a;
      `BNOT : s = ~a;
      `NOT  : s = !a;
      default : s = 16'hXXXX;
    endcase
endmodule
