module selector6(s, a, x);
  input [2:0] s;
  input [0:5] a;
  output x;
  reg x;

  always @(s, a)
    case (s)
      3'b000: x=a[0];
      3'b001: x=a[1];
      3'b010: x=a[2];
      3'b011: x=a[3];
      3'b100: x=a[4];
      3'b101: x=a[5];
      // default is needed. It cares so called "Don't care".
      default: x=1'bX;
    endcase
endmodule
