module selector2(s, a, x);
  input s;
  input [0:1] a;
  output x;
  reg x;

  // TODO make a confirmation with 真偽表
  always @(s, a) x=(a[0]&~s)|(a[1]&s);
  // The above one equals to the following:
  // always @(s, a) if (s) x=a[1]; else x=a[0];
endmodule
