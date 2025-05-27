`include "DifftestMacros.v"
module xdma_ctrl #(
  parameter DATA_WIDTH = 16000,
  parameter AXIS_DATA_WIDTH = 512
)(
  input clock,
  input reset,
  input [`CONFIG_DIFFTEST_BATCH_IO_WITDH - 1:0] difftest_data,
  input difftest_enable,
  output core_clock_enable,
  output [AXIS_DATA_WIDTH - 1:0] axi_tdata,
  output [63:0] axi_tkeep,
  output axi_tlast,
  input  axi_tready,
  output axi_tvalid
);

// always @(posedge clock) begin
//   if (!reset & difftest_enable) begin
//       $display("Difftest0: %x", difftest_data[511:0]);
//       $display("Difftest1: %x", difftest_data[1023:512]);
//       $display("Difftest2: %x", difftest_data[1535:1024]);
//       $display("Difftest3: %x", difftest_data[2047:1536]);
//   end
// end
// wire [511:0] difftest_0 = difftest_data[511:0];
// wire [511:0] difftest_1 = difftest_data[1023:512];
// wire [511:0] difftest_2 = difftest_data[1535:1024];
// wire [511:0] difftest_3 = difftest_data[2047:1536];

// assign core_clock_enable = 1'b1;
// assign axi_tdata = difftest_data[511:0];
// assign axi_tkeep = 64'hffffffff_ffffffff;
// assign axi_tlast = 1'b0;
// assign axi_tvalid = difftest_enable;

import "DPI-C" function bit v_xdma_tready();
    localparam NUM_PACKETS_PER_BUFFER = 8; // one send packet num
    localparam AXIS_SEND_LEN = ((DATA_WIDTH + AXIS_DATA_WIDTH + 8 - 1) / AXIS_DATA_WIDTH) - 1;

    localparam IDLE = 3'b001;
    localparam TRANSFER = 3'b010;
    localparam DONE = 3'b100;

    reg [2:0] current_state, next_state;

    reg [DATA_WIDTH + 8 - 1:0] mix_data;
    reg [AXIS_DATA_WIDTH - 1:0]  reg_axi_tdata;
    reg [7:0] datalen;
    reg [7:0] data_num;
    reg [4:0] state;

    reg                  reg_axi_tvalid;
    reg                  reg_axi_tlast;
    reg                  reg_core_clock_enable;
    // Ping-Pong Buffers
    reg [DATA_WIDTH - 1:0] dual_buffer [1:0][0 : NUM_PACKETS_PER_BUFFER - 1];
    reg current_buffer; // 0 for buffer_0, 1 for buffer_1
    reg this_buffer; // need read buffer
    reg [3:0] wr_pkt_cnt;   // (0~7)
    reg [3:0] rd_pkt_cnt;   // (0~7)
    reg [1:0] buffer_valid;

    wire [AXIS_DATA_WIDTH - 1:0]first_data = {difftest_data[AXIS_DATA_WIDTH - 8 - 1:0], data_num};
    assign core_clock_enable = reg_core_clock_enable;
    assign axi_tdata = reg_axi_tdata;
    assign axi_tvalid = reg_axi_tvalid;
    assign axi_tkeep = 64'hffffffff_ffffffff;
    assign axi_tlast = reg_axi_tlast;

    wire both_full = buffer_valid[0] & buffer_valid[1];

    // asynchronous clock fetches the signal
    wire core_data_sampling_en = difftest_enable;

    wire can_send = this_buffer ? buffer_valid[1] : buffer_valid[0];
    wire can_cont_send = rd_pkt_cnt < NUM_PACKETS_PER_BUFFER;
    wire one_send_last = datalen == AXIS_SEND_LEN;

    always @(posedge clock) begin
        if (reset)
            current_state <= IDLE;
        else
            current_state <= next_state;
    end

/* verilator lint_off CASEINCOMPLETE */
    always @(*) begin
        case(current_state)
            IDLE:     next_state = can_send ? TRANSFER : IDLE;
            TRANSFER: next_state = (axi_tready && reg_axi_tvalid) ?
                                   (!can_cont_send & one_send_last ? DONE : TRANSFER) : TRANSFER;
            DONE:     next_state = IDLE;
            default:  next_state = IDLE;
        endcase
    end

    // data buffer
    always @(posedge clock) begin
        if (reset) begin
            current_buffer <= 0;
            wr_pkt_cnt <= 0;
            buffer_valid <= 'b00;
        end else begin
            if (difftest_enable & reg_core_clock_enable) begin
                dual_buffer[!current_buffer][wr_pkt_cnt] <= difftest_data;
                wr_pkt_cnt <= wr_pkt_cnt + 1'b1;
                if (wr_pkt_cnt == NUM_PACKETS_PER_BUFFER - 1) begin
                    buffer_valid[!current_buffer] <= 1;
                    wr_pkt_cnt <= 0;
                    current_buffer <= ~current_buffer; // Switch buffers
                end
            end
            if (next_state == DONE) begin // Releasing a buffer
                buffer_valid[this_buffer] <= 0;
            end
        end
    end

    always @(posedge clock) begin // Back pressure control
        if (reset) begin
            reg_core_clock_enable <= 1'b1;
        end else begin
            reg_core_clock_enable <= ~both_full & ~(wr_pkt_cnt == NUM_PACKETS_PER_BUFFER - 1 & difftest_enable);
        end
    end

    // data send to axis
    always @(posedge clock) begin
        if(reset) begin
            reg_axi_tvalid <= 0;
            reg_axi_tlast <= 0;
            datalen <= 0;
            data_num <= 0;
            this_buffer <= 1;
            rd_pkt_cnt <= 0;
        end else begin
            case(current_state)
            IDLE : begin
                if (can_send) begin
                    reg_axi_tdata <= {dual_buffer[this_buffer][0][AXIS_DATA_WIDTH - 8 - 1:0], data_num};
                    mix_data <= dual_buffer[this_buffer][0][DATA_WIDTH - 1:AXIS_DATA_WIDTH - 8];
                    reg_axi_tvalid <= 1;
                    data_num <= data_num + 1'b1;
                    rd_pkt_cnt <= 1;
                    datalen <= 1;
                end
            end
            TRANSFER: begin
                if(axi_tready && reg_axi_tvalid) begin
                    reg_axi_tdata <= mix_data[AXIS_DATA_WIDTH - 1:0];
                    if(!can_cont_send & one_send_last) begin
                        reg_axi_tlast <= 1;
                        rd_pkt_cnt <= 0;
                    end else if (can_cont_send & one_send_last) begin
                        mix_data <= {dual_buffer[this_buffer][rd_pkt_cnt], data_num};
                        data_num <= data_num + 1'b1;
                        rd_pkt_cnt <= rd_pkt_cnt + 1'b1;
                        datalen <= 0;
                    end else begin
                        datalen <= datalen + 1'b1;
                        mix_data <= mix_data >> AXIS_DATA_WIDTH;
                    end
                end
            end
            DONE: begin
                reg_axi_tvalid <= 0;
                reg_axi_tlast <= 0;
                datalen <= 0;
                this_buffer <= ~this_buffer;
                data_num <= 0;
            end
            endcase
        end
    end
/* verilator lint_on CASEINCOMPLETE */
endmodule