// `module` declares that the module name is `ha`.
// variables in brackets are the "port list" of its module.
// x, y, s, c represent that these are ports for input or output.
// This module implements "half adder".
module ha(x, y, s, c);
  // This input statement describes that `x` and `y` are input ports (the size is 1 bit),
  // which means these can import data from outside of the module.
  input x, y;
  // output statement describes that `s` and `c` are output ports (the size is 1 bit),
  // which means these can export data to outside of the module.
  output s, c;
  // wire statement declares that `x`, `y`, `s` and `c` are the "net" 
  // of the module (alternative naming: "wire").
  wire x, y, s, c;

  // assign statement means that the right hand value continues to
  // assign to the left hand value. This means, when the right hand value
  // changed, the value would assign the left hand immediately.
  assign s=x^y;
  assign c=x&y;
endmodule
