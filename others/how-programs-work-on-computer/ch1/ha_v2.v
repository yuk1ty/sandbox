module ha(x, y, s, c);
  input x, y;
  output s, c;
  // declares variables instead of using assign.
  reg s, c;

  // `@()` means "sensitivity list", which executes later statement (s)
  // according to change values of `x` and `y`.
  always @(x, y) s=x^y;
  always @(x, y) c=x&y;
endmodule
