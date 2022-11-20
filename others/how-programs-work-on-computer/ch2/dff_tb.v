module dff_tb;
  reg clk, d;
  wire q;

  dff dff0(clk, d, q);

  initial clk=0;
  always #50 clk=~clk;

  initial begin
    $dumpvars;
    d=0; #200
    d=1; #200
    d=0; #200
    d=1; #200
    $finish;
  end
endmodule
