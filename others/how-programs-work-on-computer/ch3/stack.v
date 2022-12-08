// clk is clock.
// load, push and pop are input ports for controlling.
// d is an input port for reading data.
// qtop and qnext is output ports where users can load data.
module stack(clk, load, push, pop, d, qtop, qnext);
  parameter N=16;

  input clk, load, push, pop;
  input [N-1:0] d;
  output [N-1:0] qtop, qnext;
  reg [N-1:0] q[0:3];

  assign qtop=q[0];
  assign qnext=q[1];

  always @(posedge clk)
    begin
      if (load) q[0] <= d; else if (pop) q[0] <= q[1];
      if (push) q[1] <= q[0]; else if (pop) q[1] <= q[2];
      if (push) q[2] <= q[1]; else if (pop) q[2] <= q[3];
      if (push) q[3] <= q[2]; else if (pop) q[3] <= {N{1'bX}};
    end
endmodule
