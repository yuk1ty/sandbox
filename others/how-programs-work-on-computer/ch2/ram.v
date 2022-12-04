module ram(clk, load, addr, d, q);
  parameter M=12, N=16;

  input clk, load;
  input [M-1:0] addr;
  input [N-1:0] d;
  output [N-1:0] q;
  reg [N-1:0] mem[0:2**M-1];

  assign q=mem[addr];

  always @(posedge clk) if (load) mem[addr] <= d;

  initial begin
    mem[0]=13;
    mem[1]=24;
    mem[2]=39;
    mem[3]=42;
  end
endmodule
