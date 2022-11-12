module fa_tb;
  reg x, y, z;
  wire s, c;

  fa fa0(x, y, z, s, c);

  initial begin
    $dumpvars;
    x=0; y=0; z=0; #100
    x=0; y=0; z=1; #100
    x=0; y=1; z=0; #100
    x=0; y=1; z=1; #100
    x=1; y=0; z=0; #100
    x=1; y=0; z=1; #100
    x=1; y=1; z=0; #100
    x=1; y=1; z=1; #100
    $finish;
  end
endmodule
