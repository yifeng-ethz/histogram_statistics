interface hist_csr_if #(int ADDR_W = 5) (input logic clk);
  logic [31:0]          readdata;
  logic                 read;
  logic [ADDR_W-1:0]    address;
  logic                 waitrequest;
  logic                 write;
  logic [31:0]          writedata;

  clocking drv_cb @(posedge clk);
    output read, address, write, writedata;
    input  readdata, waitrequest;
  endclocking

  clocking mon_cb @(posedge clk);
    input read, address, waitrequest, write, writedata, readdata;
  endclocking

  task automatic init_master();
    read      = 1'b0;
    address   = '0;
    write     = 1'b0;
    writedata = '0;
  endtask
endinterface

interface hist_bin_if #(int ADDR_W = 8) (input logic clk);
  logic [31:0]          readdata;
  logic                 read;
  logic [ADDR_W-1:0]    address;
  logic                 waitrequest;
  logic                 write;
  logic [31:0]          writedata;
  logic [ADDR_W-1:0]    burstcount;
  logic                 readdatavalid;
  logic                 writeresponsevalid;
  logic [1:0]           response;

  clocking drv_cb @(posedge clk);
    output read, address, write, writedata, burstcount;
    input  readdata, waitrequest, readdatavalid, writeresponsevalid, response;
  endclocking

  clocking mon_cb @(posedge clk);
    input read, address, waitrequest, write, writedata, burstcount;
    input readdata, readdatavalid, writeresponsevalid, response;
  endclocking

  task automatic init_master();
    read               = 1'b0;
    address            = '0;
    write              = 1'b0;
    writedata          = '0;
    burstcount         = 'd1;
  endtask
endinterface

interface hist_fill_if #(int DATA_W = 39, int CH_W = 4) (input logic clk);
  logic                 ready;
  logic                 valid;
  logic [DATA_W-1:0]    data;
  logic                 sop;
  logic                 eop;
  logic [CH_W-1:0]      channel;

  clocking drv_cb @(posedge clk);
    output valid, data, sop, eop, channel;
    input  ready;
  endclocking

  clocking mon_cb @(posedge clk);
    input ready, valid, data, sop, eop, channel;
  endclocking

  task automatic init_source();
    valid   = 1'b0;
    data    = '0;
    sop     = 1'b0;
    eop     = 1'b0;
    channel = '0;
  endtask
endinterface

interface hist_ctrl_if (input logic clk);
  logic [8:0] data;
  logic       valid;
  logic       ready;

  clocking drv_cb @(posedge clk);
    output data, valid;
    input  ready;
  endclocking

  clocking mon_cb @(posedge clk);
    input data, valid, ready;
  endclocking

  task automatic init_source();
    data  = '0;
    valid = 1'b0;
  endtask
endinterface

interface hist_debug_if #(int DATA_W = 16) (input logic clk);
  logic              valid;
  logic [DATA_W-1:0] data;

  clocking drv_cb @(posedge clk);
    output valid, data;
  endclocking

  clocking mon_cb @(posedge clk);
    input valid, data;
  endclocking

  task automatic init_source();
    valid = 1'b0;
    data  = '0;
  endtask
endinterface

interface hist_snoop_if #(int DATA_W = 39, int CH_W = 4) (input logic clk);
  logic                 ready;   // testbench drives
  logic                 valid;   // DUT drives (do NOT add drv_cb output)
  logic [DATA_W-1:0]    data;
  logic                 sop;
  logic                 eop;
  logic [CH_W-1:0]      channel;

  clocking mon_cb @(posedge clk);
    input ready, valid, data, sop, eop, channel;
  endclocking
endinterface

interface hist_probe_if #(
  int N_PORTS   = 8,
  int PORT_W    = 3,
  int BIN_W     = 8,
  int KICK_W    = 8,
  int QUEUE_W   = 9
) (input logic clk);
  logic                  rst;
  logic                  interval_reset;
  logic                  measure_clear_pulse;
  logic                  interval_pulse;
  logic                  active_bank;
  logic                  flushing;
  logic                  cfg_apply_pending;
  logic [N_PORTS-1:0]    ingress_accept;
  logic [N_PORTS-1:0]    fifo_write;
  logic [N_PORTS-1:0]    fifo_read;
  logic [N_PORTS-1:0]    fifo_empty;
  logic [N_PORTS-1:0]    fifo_full;
  logic [N_PORTS-1:0]    drop_pulse;
  logic                  arb_valid;
  logic [PORT_W-1:0]     arb_port;
  logic                  divider_valid;
  logic                  divider_underflow;
  logic                  divider_overflow;
  logic [BIN_W-1:0]      divider_bin_index;
  logic                  queue_hit_valid;
  logic [BIN_W-1:0]      queue_hit_bin;
  logic                  queue_drain_valid;
  logic                  queue_drain_ready;
  logic [BIN_W-1:0]      queue_drain_bin;
  logic [KICK_W-1:0]     queue_drain_count;
  logic [QUEUE_W-1:0]    queue_occupancy;
  logic [QUEUE_W-1:0]    queue_occupancy_max;
  logic [15:0]           queue_overflow_count;
  logic                  burst_active;
  logic                  read_bank_latched;

  clocking mon_cb @(posedge clk);
    input rst, interval_reset, measure_clear_pulse, interval_pulse;
    input active_bank, flushing, cfg_apply_pending;
    input ingress_accept, fifo_write, fifo_read, fifo_empty, fifo_full, drop_pulse;
    input arb_valid, arb_port;
    input divider_valid, divider_underflow, divider_overflow, divider_bin_index;
    input queue_hit_valid, queue_hit_bin;
    input queue_drain_valid, queue_drain_ready, queue_drain_bin, queue_drain_count;
    input queue_occupancy, queue_occupancy_max, queue_overflow_count;
    input burst_active, read_bank_latched;
  endclocking
endinterface
