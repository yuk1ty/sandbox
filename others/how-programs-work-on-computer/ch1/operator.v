module operator;
  reg [7:0] x, y, z;
  initial begin
    x=13; y=3; z=5;
    // `$display` can print out the result
    $display("{x,y,z}=%b", {x, y, z});
  end
endmodule
