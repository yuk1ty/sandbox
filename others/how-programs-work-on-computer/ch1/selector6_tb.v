module selector6_tb;
  reg [2:0] s;
  reg [0:5] a;
  wire x;

  selector6 selector6_0(s, a, x);
  
  initial begin
    $dumpvars;
    a=6'b011010; s=3'b000; #100
    s=3'b001; #100 
    s=3'b010; #100 
    s=3'b011; #100 
    s=3'b100; #100 
    s=3'b101; #100 
    s=3'b110; #100 
    s=3'b111; #100 
    $finish;
  end
endmodule
