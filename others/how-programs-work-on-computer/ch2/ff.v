// Making a flip-flop.
// `set` means "asynchronous set" input. `rst_n` means "asynchronously reset" input. 
// `e` means an enabling input.
module ff(clk, set_n, rst_n, e, d, q);
  input clk, set_n, rst_n, e, d; // input ports with 1 bit
  output q; // an output port with 1 bit
  reg q; // a variable with 1 bit

  // negedge means "立ち下がり", which is equivalent to changing a value from 1 to 0.
  always @(posedge clk, negedge set_n, negedge rst_n)
    // asynchronously set
    if (!set_n) q <= 1;
    // asynchronously reset
    else if (!rst_n) q <= 0;
    // write `d` to `q`
    else if (e) q <= d;
endmodule
