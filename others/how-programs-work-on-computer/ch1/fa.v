// This module implements "full adder".
// This time, we generate a full adder by combining two instances (ha0, ha1).
module fa(x, y, z, s, c);
  input x, y, z;
  // s = x ^ y ^ z
  // c = (x & y) | (y & z) | (z & x)
  // `s` is XOR of three inputs
  // `c` becomes true when greater than equal two inputs are true
  output s, c;
  wire s0, c0, c1;

  ha ha0(.x(y), .y(z), .s(s0), .c(c0));
  ha ha1(.x(x), .y(s0), .s(s), .c(c1));

  assign c=c0|c1;
endmodule
