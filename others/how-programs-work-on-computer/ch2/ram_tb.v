module ram_tb;
  reg clk, load;
  reg [11:0] addr;
  reg [15:0] d;
  wire [15:0] q;

  ram #(12, 16) ram0(clk, load, addr, d, q);

  initial clk=0;
  always #50 clk=~clk;

  initial begin
    $dumpvars;
    load=0; addr=1; #100
    addr=2; #100
    addr=3; #100
    addr=4; #100
    load=1; addr=2; d=20; #100
    addr=3; d=30; #100
    addr=4; d=40; #100
    load=0; addr=1; #100
    addr=2; #100
    addr=3; #100
    addr=4; #100
    $finish;
  end
endmodule
