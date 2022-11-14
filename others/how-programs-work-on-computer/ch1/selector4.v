module selector4(s,a,x);
  input [1:0] s;
  input [0:3] a;
  output x;
  wire [0:1] c;

  selector2 selector2_0(.s(s[0]), .a(a[0:1]), .x(c[0]));
  selector2 selector2_0(.s(s[0]), .a(a[2:3]), .x(c[1]));
  selector2 selector2_0(.s(s[1]), .a(c), .x(x));
endmodule
