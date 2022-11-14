module seg7(s, x);
  input [3:0] s;
  output [0:6] x;
  reg [0:6] x;

  always @(s)
    case(s)
      4'b0000: x=7'b1111110;
      4'b0001: x=7'b0110000;
      4'b0010: x=7'b1101101;
      4'b0011: x=7'b1111101;
      4'b0100: x=7'b0110011;
      4'b0101: x=7'b1011011;
      4'b0110: x=7'b1011111;
      4'b0111: x=7'b1110000;
      4'b1000: x=7'b1111111;
      4'b1001: x=7'b1111011;
      default: x=7'bXXXXXXX;
    endcase
endmodule
