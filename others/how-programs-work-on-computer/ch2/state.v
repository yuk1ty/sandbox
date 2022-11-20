`include "state_d.v"
module state(clk, rst_n, run, halt, q);
  input clk, rst_n, run, halt;
  output [1:0] q;
  reg [1:0] q;

  always @(posedge clk, negedge rst_n)
    if (!rst_n) q <= `IDLE;
    else case (q)
      `IDLE: if (run) q <= `FETCH;
      `FETCH: q <= `EXEC;
      `EXEC: if (halt) q <= `IDLE; else q <= `FETCH;
      default: q <= 2'bXXl
    endcase
endmodule
