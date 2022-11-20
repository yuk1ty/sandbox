module ff_tb;
  reg clk, set_n, rst_n, e, d;
  wire q;

  ff ff0(clk, set_n, rst_n, e, d, q);

  initial clk=0;
  always #50 clk=~clk;

  initial begin
    $dumpvars;
    set_n=1; rst_n=1; e=0; d=1; #100
    rst_n=0; #100
    rst_n=1; #100
    e=1; #100
    d=0; #100
    e=0; #100
    set_n=0; #100
    set_n=1; #100
    e=1; #100
    d=1; #100
    rst_n=0; #100
    $finish;
  end
endmodule
