// This module implements 4 bit adder
module adder4(x, y, s);
  // [3:0] means that `x` is unsigned binary, its high-order bit
  // is x[3], and its low-order bit is x[0] (little endian). The
  // same for `y`.
  // The size is 4 bit.
  input [3:0] x, y;
  // [4:0] means `s` is an output port and its size is 4 bit.
  output [4:0] s;
  // `wire [4:0] s` is omitted but `s` is a wire as well.
  // `c` is a carrying counter and its size is 3 bit.
  wire [2:0] c;

  // The expression `1'b0` represents `0` of first bit.
  fa fa0(.x(1'b0), .y(x[0]), .z(y[0]), .s(s[0]), .c(c[0]));
  fa fa1(.x(c[0]), .y(x[1]), .z(y[1]), .s(s[1]), .c(c[1]));
  fa fa2(.x(c[1]), .y(x[2]), .z(y[2]), .s(s[2]), .c(c[2]));
  fa fa3(.x(c[2]), .y(x[3]), .z(y[3]), .s(s[3]), .c(s[4]));
endmodule
