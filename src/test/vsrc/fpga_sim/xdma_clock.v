`define CLK_FREQ 1
module xdma_clock(
  input clock,
  input reset,
  input core_clock_enable,
  output core_clock
);

reg core_clock_r;
reg [7:0] clk_cnt;
// core_clock = clock / (2 * clk_cnt)
initial begin
  core_clock_r = 0;
  clk_cnt = 0;
end

// always @(posedge clock) begin
//   if (clk_cnt == `CLK_FREQ - 1) begin
//     clk_cnt <= 0;
//     core_clock_r <= ~core_clock_r;
//   end
//   else begin
//     clk_cnt <= clk_cnt + 1;
//   end
// end

// assign core_clock = core_clock_r & core_clock_enable;
assign core_clock = clock & core_clock_enable;
endmodule
