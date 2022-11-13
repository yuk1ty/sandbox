module selector2_tb;
  reg s;
  reg [0:1] a;
  wire x;

  selector2 selector2_0(s, a, x);

  initial begin
    $dumpvars;
    s=0; a=2'b00; #100
    s=0; a=2'b01; #100
    s=0; a=2'b10; #100
    s=0; a=2'b11; #100
    s=1; a=2'b00; #100
    s=1; a=2'b01; #100
    s=1; a=2'b10; #100
    s=1; a=2'b11; #100
    $finish;
  end
endmodule
