module stack_tb;
  reg clk, load, push, pop;
  reg [15:0] d;
  wire [15:0] qtop, qnext;

  stack stack0(clk, load, push, pop, d, qtop, qnext);

  initial clk=0;
  always #50 clk=~clk;

  initial begin
    $dumpvars;
    load=0; push=0; pop=0; d=0; #100
    load=1; push=1; d=16'h1234; #100
    d=16'h5678; #100
    d=16'h9ABC; #100
    d=16'hDEF0; #100
    load=0; push=0; #100
    pop=1; #500
    $finish;
  end
endmodule
