`include "DifftestMacros.v"

module XdmaSim(
  input wire clock,
  output reg cpu_clock,
  input wire [`CONFIG_DIFFTEST_BATCH_IO_WITDH - 1:0] data,
  input wire enable
);

initial begin
  cpu_clock = 0;
end
  always @(posedge clock) begin
    cpu_clock <= ~cpu_clock;
  end
endmodule