// tb_histogram_statistics_v2.sv -- Standalone unit testbench for histogram_statistics_v2 IP.
//
// Direct task-based BFMs for AVMM CSR, AVMM hist_bin, and AVST fill_in ports.
// Reference-model scoreboard compares software histogram against DUT bin readback.
//
// Questa FSE compatible: no rand, no constraint, no covergroup, no DPI.
//
// Test selection via +TEST=<name> plusarg. Default: B01_smoke.
`timescale 1ns/1ps

module tb_histogram_statistics_v2;

  // ════════════════════════════════════════════════════════════════
  // Parameters -- match default generics of the DUT
  // ════════════════════════════════════════════════════════════════
  localparam int unsigned UPDATE_KEY_BIT_HI  = 29;
  localparam int unsigned UPDATE_KEY_BIT_LO  = 17;
  localparam string       UPDATE_KEY_REPR    = "UNSIGNED";
  localparam int unsigned FILTER_KEY_BIT_HI  = 38;
  localparam int unsigned FILTER_KEY_BIT_LO  = 35;
  localparam int unsigned SAR_TICK_WIDTH     = 32;
  localparam int unsigned SAR_KEY_WIDTH      = 16;
  localparam int unsigned N_BINS             = 256;
  localparam int unsigned MAX_COUNT_BITS     = 32;
  localparam int          DEF_LEFT_BOUND     = -1000;
  localparam int unsigned DEF_BIN_WIDTH      = 16;
  localparam int unsigned AVS_ADDR_WIDTH     = 8;
  localparam int unsigned N_PORTS            = 8;
  localparam int unsigned FIFO_ADDR_WIDTH     = 8;
  localparam int unsigned FIFO_DEPTH          = 1 << FIFO_ADDR_WIDTH;
  localparam int unsigned CHANNELS_PER_PORT  = 32;
  localparam int unsigned COAL_QUEUE_DEPTH   = 256;
  localparam int unsigned DEF_INTERVAL_CLKS  = 125_000_000;
  localparam int unsigned AVST_DATA_WIDTH    = 39;
  localparam int unsigned AVST_CHAN_WIDTH    = 4;
  localparam int unsigned VERSION_MAJOR       = 26;
  localparam int unsigned VERSION_MINOR       = 1;
  localparam int unsigned VERSION_PATCH       = 7;
  localparam int unsigned VERSION_BUILD       = 501;
  localparam int unsigned VERSION_DATE        = 20260501;

  localparam int unsigned CLK_PERIOD_NS     = 8;  // 125 MHz
  localparam int unsigned AVMM_TIMEOUT      = 1024;

  // ════════════════════════════════════════════════════════════════
  // CSR address constants
  // ════════════════════════════════════════════════════════════════
  localparam int unsigned CSR_UID            = 0;
  localparam int unsigned CSR_META           = 1;
  localparam int unsigned CSR_CONTROL        = 2;
  localparam int unsigned CSR_LEFT_BOUND     = 3;
  localparam int unsigned CSR_RIGHT_BOUND    = 4;
  localparam int unsigned CSR_BIN_WIDTH      = 5;
  localparam int unsigned CSR_KEY_FILTER     = 6;
  localparam int unsigned CSR_KEY_FILTER_V   = 7;
  localparam int unsigned CSR_UNDERFLOW      = 8;
  localparam int unsigned CSR_OVERFLOW       = 9;
  localparam int unsigned CSR_INTERVAL       = 10;
  localparam int unsigned CSR_BANK_STATUS    = 11;
  localparam int unsigned CSR_PORT_STATUS    = 12;
  localparam int unsigned CSR_TOTAL_HITS     = 13;
  localparam int unsigned CSR_DROPPED_HITS   = 14;
  localparam int unsigned CSR_COAL_STATUS    = 15;
  localparam int unsigned CSR_SCRATCH        = 16;
  localparam int unsigned CSR_LAST_INTERVAL_TOTAL_HITS   = 17;
  localparam int unsigned CSR_LAST_INTERVAL_DROPPED_HITS = 18;

  // ════════════════════════════════════════════════════════════════
  // Clock and reset
  // ════════════════════════════════════════════════════════════════
  logic i_clk = 1'b0;
  logic i_rst;
  logic i_interval_reset;

  always #(CLK_PERIOD_NS/2) i_clk = ~i_clk;

  // ════════════════════════════════════════════════════════════════
  // AVMM CSR interface
  // ════════════════════════════════════════════════════════════════
  logic [31:0] csr_readdata;
  logic        csr_read;
  logic  [4:0] csr_address;
  logic        csr_waitrequest;
  logic        csr_write;
  logic [31:0] csr_writedata;

  // ════════════════════════════════════════════════════════════════
  // AVMM hist_bin interface
  // ════════════════════════════════════════════════════════════════
  logic [31:0] bin_readdata;
  logic        bin_read;
  logic [AVS_ADDR_WIDTH-1:0] bin_address;
  logic        bin_waitrequest;
  logic        bin_write;
  logic [31:0] bin_writedata;
  logic [AVS_ADDR_WIDTH:0]   bin_burstcount;
  logic        bin_readdatavalid;
  logic        bin_writeresponsevalid;
  logic  [1:0] bin_response;

  // ════════════════════════════════════════════════════════════════
  // AVST fill_in ports (8 ports)
  // ════════════════════════════════════════════════════════════════
  logic [AVST_DATA_WIDTH-1:0] fill_data  [8];
  logic                       fill_valid [8];
  logic                       fill_sop   [8];
  logic                       fill_eop   [8];
  logic [AVST_CHAN_WIDTH-1:0]  fill_chan  [8];
  logic                       fill_ready [8];

  // AVST fill_out (snoop)
  logic                       fill_out_ready;
  logic                       fill_out_valid;
  logic [AVST_DATA_WIDTH-1:0] fill_out_data;
  logic                       fill_out_sop;
  logic                       fill_out_eop;
  logic [AVST_CHAN_WIDTH-1:0]  fill_out_chan;

  // AVST ctrl
  logic  [8:0] ctrl_data;
  logic        ctrl_valid;
  logic        ctrl_ready;

  // Debug interfaces
  logic        debug_valid [6];
  logic [15:0] debug_data  [6];

  // ════════════════════════════════════════════════════════════════
  // DUT instantiation
  // ════════════════════════════════════════════════════════════════
  histogram_statistics_v2 #(
    .UPDATE_KEY_BIT_HI         (UPDATE_KEY_BIT_HI),
    .UPDATE_KEY_BIT_LO         (UPDATE_KEY_BIT_LO),
    .UPDATE_KEY_REPRESENTATION (UPDATE_KEY_REPR),
    .FILTER_KEY_BIT_HI         (FILTER_KEY_BIT_HI),
    .FILTER_KEY_BIT_LO         (FILTER_KEY_BIT_LO),
    .SAR_TICK_WIDTH            (SAR_TICK_WIDTH),
    .SAR_KEY_WIDTH             (SAR_KEY_WIDTH),
    .N_BINS                    (N_BINS),
    .MAX_COUNT_BITS            (MAX_COUNT_BITS),
    .DEF_LEFT_BOUND            (DEF_LEFT_BOUND),
    .DEF_BIN_WIDTH             (DEF_BIN_WIDTH),
    .AVS_ADDR_WIDTH            (AVS_ADDR_WIDTH),
    .N_PORTS                   (N_PORTS),
    .FIFO_ADDR_WIDTH           (FIFO_ADDR_WIDTH),
    .CHANNELS_PER_PORT         (CHANNELS_PER_PORT),
    .COAL_QUEUE_DEPTH          (COAL_QUEUE_DEPTH),
    .ENABLE_PINGPONG           (1'b1),
    .DEF_INTERVAL_CLOCKS       (DEF_INTERVAL_CLKS),
    .AVST_DATA_WIDTH           (AVST_DATA_WIDTH),
    .AVST_CHANNEL_WIDTH        (AVST_CHAN_WIDTH),
    .N_DEBUG_INTERFACE         (6),
    .VERSION_MAJOR             (VERSION_MAJOR),
    .VERSION_MINOR             (VERSION_MINOR),
    .VERSION_PATCH             (VERSION_PATCH),
    .BUILD                     (VERSION_BUILD),
    .VERSION_DATE              (VERSION_DATE),
    .SNOOP_EN                  (1'b1),
    .ENABLE_PACKET             (1'b1),
    .DEBUG                     (0)
  ) dut (
    .avs_hist_bin_readdata           (bin_readdata),
    .avs_hist_bin_read               (bin_read),
    .avs_hist_bin_address            (bin_address),
    .avs_hist_bin_waitrequest        (bin_waitrequest),
    .avs_hist_bin_write              (bin_write),
    .avs_hist_bin_writedata          (bin_writedata),
    .avs_hist_bin_burstcount         (bin_burstcount),
    .avs_hist_bin_readdatavalid      (bin_readdatavalid),
    .avs_hist_bin_writeresponsevalid (bin_writeresponsevalid),
    .avs_hist_bin_response           (bin_response),

    .avs_csr_readdata                (csr_readdata),
    .avs_csr_read                    (csr_read),
    .avs_csr_address                 (csr_address),
    .avs_csr_waitrequest             (csr_waitrequest),
    .avs_csr_write                   (csr_write),
    .avs_csr_writedata               (csr_writedata),

    .asi_hist_fill_in_ready          (fill_ready[0]),
    .asi_hist_fill_in_valid          (fill_valid[0]),
    .asi_hist_fill_in_data           (fill_data[0]),
    .asi_hist_fill_in_startofpacket  (fill_sop[0]),
    .asi_hist_fill_in_endofpacket    (fill_eop[0]),
    .asi_hist_fill_in_channel        (fill_chan[0]),

    .asi_fill_in_1_ready             (fill_ready[1]),
    .asi_fill_in_1_valid             (fill_valid[1]),
    .asi_fill_in_1_data              (fill_data[1]),
    .asi_fill_in_1_startofpacket     (fill_sop[1]),
    .asi_fill_in_1_endofpacket       (fill_eop[1]),
    .asi_fill_in_1_channel           (fill_chan[1]),

    .asi_fill_in_2_ready             (fill_ready[2]),
    .asi_fill_in_2_valid             (fill_valid[2]),
    .asi_fill_in_2_data              (fill_data[2]),
    .asi_fill_in_2_startofpacket     (fill_sop[2]),
    .asi_fill_in_2_endofpacket       (fill_eop[2]),
    .asi_fill_in_2_channel           (fill_chan[2]),

    .asi_fill_in_3_ready             (fill_ready[3]),
    .asi_fill_in_3_valid             (fill_valid[3]),
    .asi_fill_in_3_data              (fill_data[3]),
    .asi_fill_in_3_startofpacket     (fill_sop[3]),
    .asi_fill_in_3_endofpacket       (fill_eop[3]),
    .asi_fill_in_3_channel           (fill_chan[3]),

    .asi_fill_in_4_ready             (fill_ready[4]),
    .asi_fill_in_4_valid             (fill_valid[4]),
    .asi_fill_in_4_data              (fill_data[4]),
    .asi_fill_in_4_startofpacket     (fill_sop[4]),
    .asi_fill_in_4_endofpacket       (fill_eop[4]),
    .asi_fill_in_4_channel           (fill_chan[4]),

    .asi_fill_in_5_ready             (fill_ready[5]),
    .asi_fill_in_5_valid             (fill_valid[5]),
    .asi_fill_in_5_data              (fill_data[5]),
    .asi_fill_in_5_startofpacket     (fill_sop[5]),
    .asi_fill_in_5_endofpacket       (fill_eop[5]),
    .asi_fill_in_5_channel           (fill_chan[5]),

    .asi_fill_in_6_ready             (fill_ready[6]),
    .asi_fill_in_6_valid             (fill_valid[6]),
    .asi_fill_in_6_data              (fill_data[6]),
    .asi_fill_in_6_startofpacket     (fill_sop[6]),
    .asi_fill_in_6_endofpacket       (fill_eop[6]),
    .asi_fill_in_6_channel           (fill_chan[6]),

    .asi_fill_in_7_ready             (fill_ready[7]),
    .asi_fill_in_7_valid             (fill_valid[7]),
    .asi_fill_in_7_data              (fill_data[7]),
    .asi_fill_in_7_startofpacket     (fill_sop[7]),
    .asi_fill_in_7_endofpacket       (fill_eop[7]),
    .asi_fill_in_7_channel           (fill_chan[7]),

    .aso_hist_fill_out_ready         (fill_out_ready),
    .aso_hist_fill_out_valid         (fill_out_valid),
    .aso_hist_fill_out_data          (fill_out_data),
    .aso_hist_fill_out_startofpacket (fill_out_sop),
    .aso_hist_fill_out_endofpacket   (fill_out_eop),
    .aso_hist_fill_out_channel       (fill_out_chan),

    .asi_ctrl_data                   (ctrl_data),
    .asi_ctrl_valid                  (ctrl_valid),
    .asi_ctrl_ready                  (ctrl_ready),

    .asi_debug_1_valid               (debug_valid[0]),
    .asi_debug_1_data                (debug_data[0]),
    .asi_debug_2_valid               (debug_valid[1]),
    .asi_debug_2_data                (debug_data[1]),
    .asi_debug_3_valid               (debug_valid[2]),
    .asi_debug_3_data                (debug_data[2]),
    .asi_debug_4_valid               (debug_valid[3]),
    .asi_debug_4_data                (debug_data[3]),
    .asi_debug_5_valid               (debug_valid[4]),
    .asi_debug_5_data                (debug_data[4]),
    .asi_debug_6_valid               (debug_valid[5]),
    .asi_debug_6_data                (debug_data[5]),

    .i_interval_reset                (i_interval_reset),
    .i_rst                           (i_rst),
    .i_clk                           (i_clk)
  );

  // ════════════════════════════════════════════════════════════════
  // Simple LCG PRNG (Questa FSE: no rand)
  // ════════════════════════════════════════════════════════════════
  int unsigned prng_state;

  function automatic int unsigned prng_next();
    prng_state = prng_state * 1103515245 + 12345;
    return prng_state;
  endfunction

  // ════════════════════════════════════════════════════════════════
  // Reference model (software histogram)
  // ════════════════════════════════════════════════════════════════
  int unsigned ref_bins [N_BINS];
  int unsigned ref_total_hits;
  int unsigned ref_dropped_hits;
  int unsigned ref_underflow;
  int unsigned ref_overflow;

  function automatic void ref_reset();
    for (int i = 0; i < N_BINS; i++) ref_bins[i] = 0;
    ref_total_hits  = 0;
    ref_dropped_hits = 0;
    ref_underflow   = 0;
    ref_overflow    = 0;
  endfunction

  // Current config shadow (set by test)
  int          cfg_left_bound;
  int unsigned cfg_bin_width;
  int unsigned cfg_n_bins;

  function automatic void ref_add_hit(int key);
    int bin_idx;
    if (key < cfg_left_bound) begin
      ref_underflow++;
      return;
    end
    if (cfg_bin_width == 0) begin
      // direct mode: each key maps to its offset from left_bound
      bin_idx = key - cfg_left_bound;
    end else begin
      bin_idx = (key - cfg_left_bound) / int'(cfg_bin_width);
    end
    if (bin_idx < 0 || bin_idx >= int'(cfg_n_bins)) begin
      ref_overflow++;
      return;
    end
    ref_bins[bin_idx]++;
    ref_total_hits++;
  endfunction

  function automatic void ref_add_hit_port(int key, int unsigned port_idx);
    ref_add_hit(key + int'(port_idx * CHANNELS_PER_PORT));
  endfunction

  // ════════════════════════════════════════════════════════════════
  // Error tracking
  // ════════════════════════════════════════════════════════════════
  int unsigned error_count;
  int unsigned pass_count;

  // ════════════════════════════════════════════════════════════════
  // AVMM CSR BFM tasks
  // ════════════════════════════════════════════════════════════════
  task automatic csr_write32(input int unsigned addr, input logic [31:0] data);
    @(posedge i_clk);
    csr_address   <= addr[4:0];
    csr_writedata <= data;
    csr_write     <= 1'b1;
    csr_read      <= 1'b0;
    @(posedge i_clk);
    csr_write     <= 1'b0;
    csr_address   <= '0;
    csr_writedata <= '0;
  endtask

  task automatic csr_read32(input int unsigned addr, output logic [31:0] data);
    @(posedge i_clk);
    csr_address <= addr[4:0];
    csr_read    <= 1'b1;
    csr_write   <= 1'b0;
    @(posedge i_clk);
    csr_read    <= 1'b0;
    csr_address <= '0;
    // readdata is registered, available next cycle
    @(posedge i_clk);
    data = csr_readdata;
  endtask

  // ════════════════════════════════════════════════════════════════
  // AVMM hist_bin BFM tasks
  // ════════════════════════════════════════════════════════════════
  task automatic bin_write32(input int unsigned addr, input logic [31:0] data);
    @(posedge i_clk);
    bin_address   <= addr[AVS_ADDR_WIDTH-1:0];
    bin_writedata <= data;
    bin_write     <= 1'b1;
    bin_read      <= 1'b0;
    bin_burstcount <= {{AVS_ADDR_WIDTH{1'b0}}, 1'b1};
    @(posedge i_clk);
    bin_write     <= 1'b0;
    bin_address   <= '0;
    bin_writedata <= '0;
    // wait for write response
    begin
      int unsigned cyc = 0;
      while (!bin_writeresponsevalid) begin
        @(posedge i_clk);
        cyc++;
        if (cyc > AVMM_TIMEOUT) begin
          $fatal(1, "bin_write32: write response timeout addr=%0d", addr);
        end
      end
    end
  endtask

  task automatic bin_read32(input int unsigned addr, output logic [31:0] data);
    @(posedge i_clk);
    bin_address    <= addr[AVS_ADDR_WIDTH-1:0];
    bin_read       <= 1'b1;
    bin_write      <= 1'b0;
    bin_burstcount <= {{AVS_ADDR_WIDTH{1'b0}}, 1'b1};
    @(posedge i_clk);
    bin_read    <= 1'b0;
    bin_address <= '0;
    begin
      int unsigned cyc = 0;
      while (!bin_readdatavalid) begin
        @(posedge i_clk);
        cyc++;
        if (cyc > AVMM_TIMEOUT) begin
          $fatal(1, "bin_read32: readdata timeout addr=%0d", addr);
        end
      end
    end
    data = bin_readdata;
  endtask

  // Burst read: read N consecutive addresses starting from addr
  task automatic bin_burst_read(
    input  int unsigned addr,
    input  int unsigned count,
    output logic [31:0] data_arr []
  );
    data_arr = new[count];
    for (int i = 0; i < count; i++) begin
      bin_read32(addr + i, data_arr[i]);
    end
  endtask

  // ════════════════════════════════════════════════════════════════
  // AVST fill_in BFM: inject a single hit on a given port
  // ════════════════════════════════════════════════════════════════
  task automatic inject_hit(
    input int unsigned port_idx,
    input logic [AVST_DATA_WIDTH-1:0] data,
    input logic [AVST_CHAN_WIDTH-1:0] channel = '0
  );
    int unsigned timeout_cyc;
    // Assert valid and data
    @(posedge i_clk);
    fill_data[port_idx]  <= data;
    fill_valid[port_idx] <= 1'b1;
    fill_sop[port_idx]   <= 1'b0;
    fill_eop[port_idx]   <= 1'b0;
    fill_chan[port_idx]   <= channel;
    // Hold valid until ready is asserted (Avalon-ST handshake)
    timeout_cyc = 0;
    forever begin
      @(posedge i_clk);
      if (fill_ready[port_idx]) break;
      timeout_cyc++;
      if (timeout_cyc > AVMM_TIMEOUT) begin
        $fatal(1, "inject_hit: ready timeout on port %0d", port_idx);
      end
    end
    // Deassert valid
    fill_valid[port_idx] <= 1'b0;
    fill_data[port_idx]  <= '0;
    fill_chan[port_idx]   <= '0;
  endtask

  task automatic inject_wire_burst(
    input int unsigned port_idx,
    input int unsigned n_hits
  );
    for (int unsigned i = 0; i < n_hits; i++) begin
      fill_data[port_idx]  <= make_hit_data(i % N_BINS);
      fill_valid[port_idx] <= 1'b1;
      fill_sop[port_idx]   <= 1'b0;
      fill_eop[port_idx]   <= 1'b0;
      fill_chan[port_idx]   <= '0;
      @(posedge i_clk);
      if (!fill_ready[port_idx]) begin
        $display("FAIL wire_burst: ready deasserted on port %0d at hit %0d", port_idx, i);
        error_count++;
      end
    end
    fill_valid[port_idx] <= 1'b0;
    fill_data[port_idx]  <= '0;
    fill_chan[port_idx]  <= '0;
    @(posedge i_clk);
  endtask

  task automatic inject_debug_sample(
    input int unsigned debug_idx,
    input logic [15:0] sample
  );
    @(posedge i_clk);
    debug_data[debug_idx]  <= sample;
    debug_valid[debug_idx] <= 1'b1;
    @(posedge i_clk);
    debug_valid[debug_idx] <= 1'b0;
    debug_data[debug_idx]  <= '0;
  endtask

  task automatic inject_debug_pair(
    input logic [15:0] sample0,
    input logic [15:0] sample1
  );
    @(posedge i_clk);
    debug_data[0]  <= sample0;
    debug_data[1]  <= sample1;
    debug_valid[0] <= 1'b1;
    debug_valid[1] <= 1'b1;
    @(posedge i_clk);
    debug_valid[0] <= 1'b0;
    debug_valid[1] <= 1'b0;
    debug_data[0]  <= '0;
    debug_data[1]  <= '0;
  endtask

  // Build a fill_in data word with a specific key value in the update_key field
  function automatic logic [AVST_DATA_WIDTH-1:0] make_hit_data(int key_val);
    logic [AVST_DATA_WIDTH-1:0] d;
    d = '0;
    // Place key_val into bits [UPDATE_KEY_BIT_HI:UPDATE_KEY_BIT_LO]
    for (int b = UPDATE_KEY_BIT_LO; b <= UPDATE_KEY_BIT_HI && b < AVST_DATA_WIDTH; b++)
      d[b] = key_val[b - UPDATE_KEY_BIT_LO];
    return d;
  endfunction

  // Build a fill_in data word with both a key and a filter field
  function automatic logic [AVST_DATA_WIDTH-1:0] make_hit_data_with_filter(
    int key_val,
    int unsigned filter_val
  );
    logic [AVST_DATA_WIDTH-1:0] d;
    d = make_hit_data(key_val);
    for (int b = FILTER_KEY_BIT_LO; b <= FILTER_KEY_BIT_HI && b < AVST_DATA_WIDTH; b++)
      d[b] = filter_val[b - FILTER_KEY_BIT_LO];
    return d;
  endfunction

  // ════════════════════════════════════════════════════════════════
  // Configuration helpers
  // ════════════════════════════════════════════════════════════════
  task automatic configure_default();
    // Use default generics: left_bound=-1000, bin_width=16, N_BINS=256
    // Right bound = -1000 + 16*256 = 3096
    cfg_left_bound = DEF_LEFT_BOUND;
    cfg_bin_width  = DEF_BIN_WIDTH;
    cfg_n_bins     = N_BINS;
    // Wait for initial clear to complete before configuring
    wait_initial_clear();
    // Write CSRs and apply
    csr_write32(CSR_LEFT_BOUND, 32'($signed(cfg_left_bound)));
    csr_write32(CSR_BIN_WIDTH,  cfg_bin_width);
    csr_write32(CSR_INTERVAL,   2000);  // short interval for testing
    // Apply config (bit 0 = 1, key_unsigned = bit 8)
    csr_write32(CSR_CONTROL, 32'h0000_0101);
    // Wait for cfg_apply_pending to assert and then clear (ingress empty -> immediate)
    repeat (10) @(posedge i_clk);
  endtask

  task automatic configure_custom(
    input int          left_bound,
    input int unsigned bin_width,
    input bit          key_unsigned = 1'b1,
    input bit          filter_enable = 1'b0,
    input bit          filter_reject = 1'b0,
    input int unsigned filter_key_val = 0,
    input int unsigned interval = 2000
  );
    logic [31:0] ctrl_word;
    cfg_left_bound = left_bound;
    cfg_bin_width  = bin_width;
    cfg_n_bins     = N_BINS;

    wait_initial_clear();
    csr_write32(CSR_LEFT_BOUND, 32'($signed(left_bound)));
    csr_write32(CSR_BIN_WIDTH,  bin_width);
    csr_write32(CSR_INTERVAL,   interval);
    if (filter_enable) begin
      csr_write32(CSR_KEY_FILTER_V, {filter_key_val[15:0], 16'h0000});
    end

    ctrl_word = 32'h0000_0001;  // apply
    ctrl_word[8]  = key_unsigned;
    ctrl_word[12] = filter_enable;
    ctrl_word[13] = filter_reject;
    csr_write32(CSR_CONTROL, ctrl_word);
    repeat (10) @(posedge i_clk);
  endtask

  task automatic configure_debug_mts_both(
    input int          left_bound,
    input int unsigned bin_width,
    input int unsigned interval = 2000,
    input bit          filter_enable = 1'b0,
    input bit          filter_reject = 1'b0,
    input int unsigned filter_key_low = FILTER_KEY_BIT_LO,
    input int unsigned filter_key_high = FILTER_KEY_BIT_HI,
    input int unsigned filter_key_val = 0
  );
    logic [31:0] key_loc_word;
    logic [31:0] ctrl_word;

    cfg_left_bound = left_bound;
    cfg_bin_width  = bin_width;
    cfg_n_bins     = N_BINS;

    wait_initial_clear();
    csr_write32(CSR_LEFT_BOUND, 32'($signed(left_bound)));
    csr_write32(CSR_BIN_WIDTH,  bin_width);
    csr_write32(CSR_INTERVAL,   interval);
    if (filter_enable) begin
      key_loc_word         = '0;
      key_loc_word[7:0]    = UPDATE_KEY_BIT_LO[7:0];
      key_loc_word[15:8]   = UPDATE_KEY_BIT_HI[7:0];
      key_loc_word[23:16]  = filter_key_low[7:0];
      key_loc_word[31:24]  = filter_key_high[7:0];
      csr_write32(CSR_KEY_FILTER, key_loc_word);
      csr_write32(CSR_KEY_FILTER_V, {filter_key_val[15:0], 16'h0000});
    end

    ctrl_word        = 32'h0000_0091;  // apply + signed mode -7
    ctrl_word[12]    = filter_enable;
    ctrl_word[13]    = filter_reject;
    csr_write32(CSR_CONTROL, ctrl_word);
    repeat (10) @(posedge i_clk);
  endtask

  // ════════════════════════════════════════════════════════════════
  // Comparison helper: read all bins and compare to reference
  // ════════════════════════════════════════════════════════════════
  task automatic check_bins(input string tag);
    logic [31:0] rd_data;
    int unsigned mismatch_count;
    mismatch_count = 0;

    for (int i = 0; i < N_BINS; i++) begin
      bin_read32(i, rd_data);
      if (rd_data != ref_bins[i]) begin
        $display("MISMATCH %s bin[%0d]: DUT=%0d REF=%0d", tag, i, rd_data, ref_bins[i]);
        mismatch_count++;
        if (mismatch_count > 10) begin
          $display("  ... (suppressing further mismatches)");
          break;
        end
      end
    end

    if (mismatch_count == 0) begin
      $display("PASS %s: all %0d bins match", tag, N_BINS);
      pass_count++;
    end else begin
      $display("FAIL %s: %0d bin mismatches", tag, mismatch_count);
      error_count++;
    end
  endtask

  task automatic check_csr(input string tag, input int unsigned addr, input int unsigned expected);
    logic [31:0] rd_data;
    csr_read32(addr, rd_data);
    if (rd_data == expected) begin
      $display("PASS %s: CSR[%0d]=0x%08h", tag, addr, rd_data);
      pass_count++;
    end else begin
      $display("FAIL %s: CSR[%0d]=0x%08h expected=0x%08h", tag, addr, rd_data, expected);
      error_count++;
    end
  endtask

  // ════════════════════════════════════════════════════════════════
  // Measure clear: write 0x00000000 to hist_bin addr 0
  // ════════════════════════════════════════════════════════════════
  task automatic do_clear();
    bin_write32(0, 32'h0000_0000);
    // Wait for clear to propagate through pipeline
    repeat (20) @(posedge i_clk);
  endtask

  // ════════════════════════════════════════════════════════════════
  // Wait for timer-driven bank swap (interval_pulse from pingpong_sram)
  // ════════════════════════════════════════════════════════════════
  task automatic wait_bank_swap();
    int unsigned timeout_cyc;
    timeout_cyc = 0;
    while (!dut.interval_pulse) begin
      @(posedge i_clk);
      timeout_cyc++;
      if (timeout_cyc > 100000) begin
        $fatal(1, "wait_bank_swap: timeout waiting for interval_pulse");
      end
    end
    // Wait for flush of the old bank to complete
    repeat (N_BINS + 50) @(posedge i_clk);
  endtask

  // Wait for initial clear (both banks) after reset
  task automatic wait_initial_clear();
    int unsigned timeout_cyc;
    timeout_cyc = 0;
    // Wait for clear_active to go low
    while (dut.flushing) begin
      @(posedge i_clk);
      timeout_cyc++;
      if (timeout_cyc > 10000) begin
        $fatal(1, "wait_initial_clear: timeout");
      end
    end
    repeat (5) @(posedge i_clk);
  endtask

  // ════════════════════════════════════════════════════════════════
  // Wait for pipeline drain (hits -> FIFO -> arbiter -> divider -> queue -> SRAM)
  // ════════════════════════════════════════════════════════════════
  task automatic wait_pipeline_drain(int unsigned extra_cycles = 100);
    repeat (N_PORTS * 10 + COAL_QUEUE_DEPTH + extra_cycles) @(posedge i_clk);
  endtask

  // ════════════════════════════════════════════════════════════════
  // TEST: B01_smoke
  // ════════════════════════════════════════════════════════════════
  task automatic test_B01_smoke();
    logic [AVST_DATA_WIDTH-1:0] hit_data;
    int key_val;

    $display("═══ B01_smoke ═══");
    ref_reset();
    configure_default();

    // Inject 1 hit at key = 0 -> bin index = (0 - (-1000))/16 = 62
    key_val  = 0;
    hit_data = make_hit_data(key_val);
    inject_hit(0, hit_data);
    ref_add_hit(key_val);
    wait_pipeline_drain();
    wait_bank_swap();
    check_bins("B01");
  endtask

  // ════════════════════════════════════════════════════════════════
  // TEST: B02_multi_port
  // ════════════════════════════════════════════════════════════════
  task automatic test_B02_multi_port();
    logic [AVST_DATA_WIDTH-1:0] hit_data;
    int key_val;

    $display("═══ B02_multi_port ═══");
    ref_reset();
    configure_default();

    // Inject 1 hit per port with distinct keys
    for (int p = 0; p < N_PORTS; p++) begin
      key_val  = p * 100;  // keys: 0, 100, 200, ..., 700
      hit_data = make_hit_data(key_val);
      $display("B02 DEBUG port%0d: key=%0d hit_data=0x%010h", p, key_val, hit_data);
      inject_hit(p, hit_data);
      ref_add_hit_port(key_val, p);
      // Observe key after ingress stage registers it
      @(posedge i_clk);
      @(posedge i_clk);
      $display("B02 DEBUG port%0d: stage_key=%0d stage_wr=%0b",
               p,
               $signed(dut.ingress_stage_key[p]),
               dut.ingress_stage_write_req[p]);
    end
    // Watch bin_divider output
    fork
      begin
        for (int w = 0; w < 200; w++) begin
          @(posedge i_clk);
          if (dut.pingpong_inst.i_upd_valid) begin
            $display("B02 DEBUG drain: bin=%0d count=%0d",
                     dut.pingpong_inst.i_upd_bin,
                     dut.pingpong_inst.i_upd_count);
          end
        end
      end
    join_none
    wait_pipeline_drain();
    check_csr("B02_total", CSR_TOTAL_HITS, ref_total_hits);
    wait_bank_swap();
    check_bins("B02");
  endtask

  // ════════════════════════════════════════════════════════════════
  // TEST: B03_csr_readback
  // ════════════════════════════════════════════════════════════════
  task automatic test_B03_csr_readback();
    logic [31:0] rd;

    $display("═══ B03_csr_readback ═══");

    // Write scratch, read back
    csr_write32(CSR_SCRATCH, 32'hDEAD_BEEF);
    csr_read32(CSR_SCRATCH, rd);
    if (rd == 32'hDEAD_BEEF) begin
      $display("PASS B03: scratch=0x%08h", rd);
      pass_count++;
    end else begin
      $display("FAIL B03: scratch=0x%08h expected=0xDEADBEEF", rd);
      error_count++;
    end

    // Write left_bound, read back
    csr_write32(CSR_LEFT_BOUND, 32'hFFFF_FC18);  // -1000
    csr_read32(CSR_LEFT_BOUND, rd);
    if (rd == 32'hFFFF_FC18) begin
      $display("PASS B03: left_bound=0x%08h", rd);
      pass_count++;
    end else begin
      $display("FAIL B03: left_bound=0x%08h expected=0xFFFFFC18", rd);
      error_count++;
    end
  endtask

  // ════════════════════════════════════════════════════════════════
  // TEST: B04_identity_header
  // ════════════════════════════════════════════════════════════════
  task automatic test_B04_version();
    logic [31:0] rd;
    logic [31:0] expected;

    $display("═══ B04_identity_header ═══");

    // Check UID (word 0) = ASCII "HIST" = 0x48495354
    csr_read32(CSR_UID, rd);
    if (rd == 32'h4849_5354) begin
      $display("PASS B04a: UID=0x%08h (HIST)", rd);
      pass_count++;
    end else begin
      $display("FAIL B04a: UID=0x%08h expected=0x48495354", rd);
      error_count++;
    end

    // Check META page 0 = VERSION
    csr_write32(CSR_META, 32'd0);
    csr_read32(CSR_META, rd);
    expected = {VERSION_MAJOR[7:0], VERSION_MINOR[7:0], VERSION_PATCH[3:0], VERSION_BUILD[11:0]};
    if (rd == expected) begin
      $display("PASS B04b: META[VERSION]=0x%08h", rd);
      pass_count++;
    end else begin
      $display("FAIL B04b: META[VERSION]=0x%08h expected=0x%08h", rd, expected);
      error_count++;
    end

    // Check META page 1 = DATE
    csr_write32(CSR_META, 32'd1);
    csr_read32(CSR_META, rd);
    if (rd == VERSION_DATE) begin
      $display("PASS B04c: META[DATE]=0x%08h (%0d)", rd, rd);
      pass_count++;
    end else begin
      $display("FAIL B04c: META[DATE]=0x%08h expected=%0d", rd, VERSION_DATE);
      error_count++;
    end
  endtask

  // ════════════════════════════════════════════════════════════════
  // TEST: B06_bin_mapping
  // ════════════════════════════════════════════════════════════════
  task automatic test_B06_bin_mapping();
    logic [AVST_DATA_WIDTH-1:0] hit_data;
    int key_val;

    $display("═══ B06_bin_mapping ═══");
    ref_reset();
    configure_custom(DEF_LEFT_BOUND, DEF_BIN_WIDTH, 1'b0, 1'b0, 1'b0, 0, 2000);

    // Inject hits at bin boundaries: left_bound + i*bin_width for i=0..9
    for (int i = 0; i < 10; i++) begin
      key_val  = DEF_LEFT_BOUND + i * DEF_BIN_WIDTH;
      hit_data = make_hit_data(key_val);
      inject_hit(0, hit_data);
      ref_add_hit(key_val);
      repeat (2) @(posedge i_clk);
    end
    wait_pipeline_drain();
    wait_bank_swap();
    check_bins("B06");
  endtask

  // ════════════════════════════════════════════════════════════════
  // TEST: B07_clear
  // ════════════════════════════════════════════════════════════════
  task automatic test_B07_clear();
    logic [AVST_DATA_WIDTH-1:0] hit_data;
    logic [31:0] rd;
    int mismatch;

    $display("═══ B07_clear ═══");
    ref_reset();
    configure_default();

    // Fill some bins
    for (int i = 0; i < 5; i++) begin
      hit_data = make_hit_data(i * 100);
      inject_hit(0, hit_data);
    end
    wait_pipeline_drain();

    // Clear
    do_clear();
    // Also trigger interval swap so we can read
    wait_bank_swap();

    // All bins should be zero
    mismatch = 0;
    for (int i = 0; i < N_BINS; i++) begin
      bin_read32(i, rd);
      if (rd != 0) begin
        $display("MISMATCH B07: bin[%0d]=%0d (expected 0 after clear)", i, rd);
        mismatch++;
        if (mismatch > 5) break;
      end
    end
    if (mismatch == 0) begin
      $display("PASS B07: all bins zero after clear");
      pass_count++;
    end else begin
      $display("FAIL B07: %0d bins non-zero after clear", mismatch);
      error_count++;
    end
  endtask

  // ════════════════════════════════════════════════════════════════
  // TEST: B08_interval_latched_counters
  // ════════════════════════════════════════════════════════════════
  task automatic test_B08_interval_latched_counters();
    logic [31:0] rd;

    $display("═══ B08_interval_latched_counters ═══");
    ref_reset();
    configure_custom(0, 1, 1'b1, 1'b0, 1'b0, 0, 2000);

    for (int i = 0; i < 4; i++) begin
      inject_hit(0, make_hit_data(i));
      ref_add_hit(i);
    end
    wait_pipeline_drain();

    check_csr("B08_live_total_before_interval", CSR_TOTAL_HITS, ref_total_hits);
    check_csr("B08_live_dropped_before_interval", CSR_DROPPED_HITS, 0);
    check_csr("B08_last_total_before_interval", CSR_LAST_INTERVAL_TOTAL_HITS, 0);
    check_csr("B08_last_dropped_before_interval", CSR_LAST_INTERVAL_DROPPED_HITS, 0);

    wait_bank_swap();

    check_csr("B08_last_total_after_interval", CSR_LAST_INTERVAL_TOTAL_HITS, ref_total_hits);
    check_csr("B08_last_dropped_after_interval", CSR_LAST_INTERVAL_DROPPED_HITS, 0);
    check_csr("B08_live_total_after_interval", CSR_TOTAL_HITS, 0);
    check_csr("B08_live_dropped_after_interval", CSR_DROPPED_HITS, 0);

    do_clear();
    csr_read32(CSR_LAST_INTERVAL_TOTAL_HITS, rd);
    if (rd == 0) begin
      $display("PASS B08_clear_last_total: last interval total cleared");
      pass_count++;
    end else begin
      $display("FAIL B08_clear_last_total: got=%0d expected=0", rd);
      error_count++;
    end

    csr_read32(CSR_LAST_INTERVAL_DROPPED_HITS, rd);
    if (rd == 0) begin
      $display("PASS B08_clear_last_dropped: last interval dropped cleared");
      pass_count++;
    end else begin
      $display("FAIL B08_clear_last_dropped: got=%0d expected=0", rd);
      error_count++;
    end
  endtask

  // ════════════════════════════════════════════════════════════════
  // TEST: B09_filter_pass
  // ════════════════════════════════════════════════════════════════
  task automatic test_B09_filter_pass();
    logic [AVST_DATA_WIDTH-1:0] hit_data;
    logic [31:0] rd;

    $display("═══ B09_filter_pass ═══");
    ref_reset();
    configure_custom(
      .left_bound    (DEF_LEFT_BOUND),
      .bin_width     (DEF_BIN_WIDTH),
      .key_unsigned  (1'b1),
      .filter_enable (1'b1),
      .filter_reject (1'b0),
      .filter_key_val(4'hA)  // filter_key = 0xA
    );

    // Inject hit WITH matching filter field
    hit_data = make_hit_data_with_filter(0, 4'hA);
    inject_hit(0, hit_data);
    ref_add_hit(0);

    // Inject hit WITHOUT matching filter field
    hit_data = make_hit_data_with_filter(100, 4'hB);
    inject_hit(0, hit_data);
    // This should be dropped (filter_pass = false)

    wait_pipeline_drain();
    check_csr("B09_total", CSR_TOTAL_HITS, 2);
    wait_bank_swap();
    check_bins("B09");
  endtask

  // ════════════════════════════════════════════════════════════════
  // TEST: B10_debug_mts_both
  // ════════════════════════════════════════════════════════════════
  task automatic test_B10_debug_mts_both();
    logic [31:0] rd;

    $display("═══ B10_debug_mts_both ═══");
    ref_reset();
    configure_debug_mts_both(0, 1, 2000);

    inject_debug_pair(16'd10, 16'd20);
    ref_add_hit(10);
    ref_add_hit(20);

    inject_debug_sample(0, 16'hFFFF);
    ref_add_hit(-1);

    inject_debug_sample(1, 16'd30);
    ref_add_hit(30);

    wait_pipeline_drain();
    check_csr("B10_total", CSR_TOTAL_HITS, ref_total_hits + ref_underflow);
    check_csr("B10_underflow", CSR_UNDERFLOW, ref_underflow);
    wait_bank_swap();
    check_bins("B10");

    bin_read32(10, rd);
    if (rd == 1) begin
      $display("PASS B10_no_port_offset: debug_2 kept raw key bins");
      pass_count++;
    end else begin
      $display("FAIL B10_no_port_offset: bin[10]=%0d expected 1", rd);
      error_count++;
    end
  endtask

  // ════════════════════════════════════════════════════════════════
  // TEST: B11_debug_mts_filter_source
  // ════════════════════════════════════════════════════════════════
  task automatic test_B11_debug_mts_filter_source();
    logic [31:0] rd;

    $display("═══ B11_debug_mts_filter_source ═══");
    ref_reset();
    configure_debug_mts_both(
      .left_bound      (0),
      .bin_width       (1),
      .interval        (2000),
      .filter_enable   (1'b1),
      .filter_reject   (1'b0),
      .filter_key_low  (16),
      .filter_key_high (23),
      .filter_key_val  (1)
    );

    inject_debug_pair(16'd10, 16'd20);
    ref_add_hit(20);

    inject_debug_pair(16'd30, 16'd40);
    ref_add_hit(40);

    wait_pipeline_drain();
    check_csr("B11_total_raw", CSR_TOTAL_HITS, 4);
    wait_bank_swap();
    check_bins("B11");

    bin_read32(10, rd);
    if (rd == 0) begin
      $display("PASS B11_filter_debug1: debug_1 sample was filtered out");
      pass_count++;
    end else begin
      $display("FAIL B11_filter_debug1: bin[10]=%0d expected 0", rd);
      error_count++;
    end

    bin_read32(20, rd);
    if (rd == 1) begin
      $display("PASS B11_filter_debug2: debug_2 sample was kept");
      pass_count++;
    end else begin
      $display("FAIL B11_filter_debug2: bin[20]=%0d expected 1", rd);
      error_count++;
    end
  endtask

  // ════════════════════════════════════════════════════════════════
  // TEST: E01_underflow
  // ════════════════════════════════════════════════════════════════
  task automatic test_E01_underflow();
    logic [AVST_DATA_WIDTH-1:0] hit_data;

    $display("═══ E01_underflow ═══");
    ref_reset();
    configure_custom(DEF_LEFT_BOUND, DEF_BIN_WIDTH, 1'b0, 1'b0, 1'b0, 0, 2000);

    // Inject hit with key far below left_bound
    hit_data = make_hit_data(DEF_LEFT_BOUND - 100);
    inject_hit(0, hit_data);
    ref_add_hit(DEF_LEFT_BOUND - 100);  // ref model tracks underflow
    wait_pipeline_drain();

    check_csr("E01_underflow", CSR_UNDERFLOW, ref_underflow);
  endtask

  // ════════════════════════════════════════════════════════════════
  // TEST: E02_overflow
  // ════════════════════════════════════════════════════════════════
  task automatic test_E02_overflow();
    logic [AVST_DATA_WIDTH-1:0] hit_data;
    int right_bound;

    $display("═══ E02_overflow ═══");
    ref_reset();
    configure_default();
    right_bound = DEF_LEFT_BOUND + DEF_BIN_WIDTH * N_BINS;

    // Inject hit with key at right_bound (should overflow)
    hit_data = make_hit_data(right_bound);
    inject_hit(0, hit_data);
    ref_add_hit(right_bound);
    wait_pipeline_drain();

    check_csr("E02_overflow", CSR_OVERFLOW, ref_overflow);
  endtask

  // ════════════════════════════════════════════════════════════════
  // TEST: E06_invalid_bounds
  // ════════════════════════════════════════════════════════════════
  task automatic test_E06_invalid_bounds();
    logic [31:0] rd;

    $display("═══ E06_invalid_bounds ═══");
    // Set bin_width=0 and right_bound <= left_bound
    csr_write32(CSR_LEFT_BOUND, 32'h0000_0064);  // 100
    csr_write32(CSR_RIGHT_BOUND, 32'h0000_0032); // 50 (less than left)
    csr_write32(CSR_BIN_WIDTH, 0);
    // Apply -- should set error
    csr_write32(CSR_CONTROL, 32'h0000_0101);  // apply + key_unsigned
    repeat (4) @(posedge i_clk);

    csr_read32(CSR_CONTROL, rd);
    if (rd[24] == 1'b1 && rd[31:28] == 4'h1) begin
      $display("PASS E06: error=1 error_info=1 as expected");
      pass_count++;
    end else begin
      $display("FAIL E06: control=0x%08h, expected error bit[24]=1, info[31:28]=1", rd);
      error_count++;
    end
  endtask

  // ════════════════════════════════════════════════════════════════
  // TEST: P01_sustained_fill
  // ════════════════════════════════════════════════════════════════
  task automatic test_P01_sustained_fill();
    logic [AVST_DATA_WIDTH-1:0] hit_data;
    int key_val;
    int unsigned n_hits;

    $display("═══ P01_sustained_fill ═══");
    ref_reset();
    configure_custom(DEF_LEFT_BOUND, DEF_BIN_WIDTH, 1'b0, 1'b0, 1'b0, 0, 20000);
    n_hits = 1000;

    for (int i = 0; i < n_hits; i++) begin
      // Distribute keys uniformly across valid range
      key_val  = DEF_LEFT_BOUND + (prng_next() % (DEF_BIN_WIDTH * N_BINS));
      hit_data = make_hit_data(key_val);
      inject_hit(0, hit_data);
      ref_add_hit(key_val);
      // Small gap to prevent FIFO overflow
      if ((i % 4) == 3) repeat (10) @(posedge i_clk);
    end
    wait_pipeline_drain(500);
    check_csr("P01_total", CSR_TOTAL_HITS, ref_total_hits);
    wait_bank_swap();
    check_bins("P01");
  endtask

  // ════════════════════════════════════════════════════════════════
  // TEST: P03_wire_burst_absorb
  // ════════════════════════════════════════════════════════════════
  task automatic test_P03_wire_burst_absorb();
    logic [31:0] total_hits;
    logic [31:0] dropped_hits;
    logic [31:0] port_status;
    int unsigned n_hits;

    $display("═══ P03_wire_burst_absorb ═══");
    ref_reset();
    configure_custom(0, 1, 1'b1, 1'b0, 1'b0, 0, 1_000_000);

    n_hits = 1024;
    inject_wire_burst(0, n_hits);
    wait_pipeline_drain(n_hits + 2048);

    csr_read32(CSR_TOTAL_HITS, total_hits);
    csr_read32(CSR_DROPPED_HITS, dropped_hits);
    csr_read32(CSR_PORT_STATUS, port_status);

    if (total_hits == n_hits) begin
      $display("PASS P03_total: total_hits=%0d", total_hits);
      pass_count++;
    end else begin
      $display("FAIL P03_total: total_hits=%0d expected=%0d", total_hits, n_hits);
      error_count++;
    end

    if (dropped_hits == 0) begin
      $display("PASS P03_dropped: line-rate single-port burst dropped 0 hits");
      pass_count++;
    end else begin
      $display("FAIL P03_dropped: dropped_hits=%0d expected=0 at one accepted hit per clock", dropped_hits);
      error_count++;
    end

    if (port_status[23:16] <= 8'd4) begin
      $display("PASS P03_port_status: fifo_level_max stayed bounded at line rate (status=0x%08h)", port_status);
      pass_count++;
    end else begin
      $display("FAIL P03_port_status: status=0x%08h expected max field <= 0x04", port_status);
      error_count++;
    end
  endtask

  // ════════════════════════════════════════════════════════════════
  // TEST: P04_all_channel_injection_frame
  // ════════════════════════════════════════════════════════════════
  task automatic test_P04_all_channel_injection_frame();
    logic [31:0] dropped_hits;
    logic [31:0] coal_status;

    $display("═══ P04_all_channel_injection_frame ═══");
    ref_reset();
    configure_custom(0, 1, 1'b1, 1'b0, 1'b0, 0, 5000);

    // One TDC injection frame can offer all 32 channels from each of 8 MuTRiG
    // ASICs. Drive one channel per port per clock to exercise the per-port
    // FIFO depth and the shared round-robin arbiter at the same time.
    for (int ch = 0; ch < CHANNELS_PER_PORT; ch++) begin
      for (int p = 0; p < N_PORTS; p++) begin
        fill_data[p]  <= make_hit_data(ch);
        fill_valid[p] <= 1'b1;
        fill_sop[p]   <= 1'b0;
        fill_eop[p]   <= 1'b0;
        fill_chan[p]  <= '0;
        ref_add_hit_port(ch, p);
      end
      @(posedge i_clk);
      for (int p = 0; p < N_PORTS; p++) begin
        if (!fill_ready[p]) begin
          $display("FAIL P04_ready: ready deasserted on port %0d channel %0d", p, ch);
          error_count++;
        end
      end
    end
    for (int p = 0; p < N_PORTS; p++) begin
      fill_valid[p] <= 1'b0;
      fill_data[p]  <= '0;
      fill_chan[p]  <= '0;
    end

    wait_pipeline_drain(2048);
    check_csr("P04_total", CSR_TOTAL_HITS, N_BINS);

    csr_read32(CSR_DROPPED_HITS, dropped_hits);
    if (dropped_hits == 0) begin
      $display("PASS P04_dropped: dropped_hits=0");
      pass_count++;
    end else begin
      $display("FAIL P04_dropped: dropped_hits=%0d expected=0", dropped_hits);
      error_count++;
    end

    csr_read32(CSR_COAL_STATUS, coal_status);
    if (coal_status[31:16] == 16'h0000) begin
      $display("PASS P04_coal_overflow: coal_status=0x%08h", coal_status);
      pass_count++;
    end else begin
      $display("FAIL P04_coal_overflow: coal_status=0x%08h expected overflow field 0", coal_status);
      error_count++;
    end

    wait_bank_swap();
    check_bins("P04");
  endtask

  // ════════════════════════════════════════════════════════════════
  // TEST: P02_all_ports_soak
  // ════════════════════════════════════════════════════════════════
  task automatic test_P02_all_ports_soak();
    logic [AVST_DATA_WIDTH-1:0] hit_data;
    int key_val;
    int unsigned n_hits_per_port;

    $display("═══ P02_all_ports_soak ═══");
    ref_reset();
    configure_custom(DEF_LEFT_BOUND, DEF_BIN_WIDTH, 1'b0, 1'b0, 1'b0, 0, 20000);
    n_hits_per_port = 100;

    for (int round = 0; round < n_hits_per_port; round++) begin
      for (int p = 0; p < N_PORTS; p++) begin
        key_val  = DEF_LEFT_BOUND + (prng_next() % (DEF_BIN_WIDTH * N_BINS));
        hit_data = make_hit_data(key_val);
        inject_hit(p, hit_data);
        ref_add_hit_port(key_val, p);
      end
      repeat (20) @(posedge i_clk);
    end
    wait_pipeline_drain(1000);
    check_csr("P02_total", CSR_TOTAL_HITS, n_hits_per_port * N_PORTS);
    wait_bank_swap();
    check_bins("P02");
  endtask

  // ════════════════════════════════════════════════════════════════
  // TEST: R01_reset_mid_fill
  // ════════════════════════════════════════════════════════════════
  task automatic test_R01_reset_mid_fill();
    logic [AVST_DATA_WIDTH-1:0] hit_data;
    logic [31:0] rd;
    int mismatch;

    $display("═══ R01_reset_mid_fill ═══");
    configure_default();

    // Inject several hits
    for (int i = 0; i < 5; i++) begin
      hit_data = make_hit_data(i * 100);
      inject_hit(0, hit_data);
    end

    // Assert reset mid-pipeline
    repeat (3) @(posedge i_clk);
    i_rst <= 1'b1;
    repeat (5) @(posedge i_clk);
    i_rst <= 1'b0;
    repeat (20) @(posedge i_clk);

    // Re-configure after reset
    ref_reset();
    configure_default();

    // Verify bins are clean
    wait_bank_swap();
    mismatch = 0;
    for (int i = 0; i < N_BINS; i++) begin
      bin_read32(i, rd);
      if (rd != 0) begin
        mismatch++;
        if (mismatch > 5) break;
      end
    end
    if (mismatch == 0) begin
      $display("PASS R01: clean state after reset");
      pass_count++;
    end else begin
      $display("FAIL R01: %0d bins non-zero after reset", mismatch);
      error_count++;
    end
  endtask

  // ════════════════════════════════════════════════════════════════
  // Signal initialization and reset
  // ════════════════════════════════════════════════════════════════
  task automatic do_reset();
    csr_read       <= 1'b0;
    csr_write      <= 1'b0;
    csr_address    <= '0;
    csr_writedata  <= '0;
    bin_read       <= 1'b0;
    bin_write      <= 1'b0;
    bin_address    <= '0;
    bin_writedata  <= '0;
    bin_burstcount <= {{AVS_ADDR_WIDTH{1'b0}}, 1'b1};
    for (int i = 0; i < 8; i++) begin
      fill_valid[i] <= 1'b0;
      fill_data[i]  <= '0;
      fill_sop[i]   <= 1'b0;
      fill_eop[i]   <= 1'b0;
      fill_chan[i]   <= '0;
    end
    fill_out_ready <= 1'b1;
    ctrl_data      <= 9'h000;
    ctrl_valid     <= 1'b0;
    for (int i = 0; i < 6; i++) begin
      debug_valid[i] <= 1'b0;
      debug_data[i]  <= '0;
    end
    i_interval_reset <= 1'b0;

    i_rst <= 1'b1;
    repeat (10) @(posedge i_clk);
    i_rst <= 1'b0;
    repeat (5) @(posedge i_clk);
  endtask

  // ════════════════════════════════════════════════════════════════
  // Main test dispatcher
  // ════════════════════════════════════════════════════════════════
  string test_name;

  initial begin
    int unsigned seed;

    error_count = 0;
    pass_count  = 0;

    if (!$value$plusargs("SEED=%d", seed)) seed = 42;
    prng_state = seed;

    if (!$value$plusargs("TEST=%s", test_name)) test_name = "B01_smoke";

    $display("════════════════════════════════════════════════════════");
    $display("  histogram_statistics_v2 standalone testbench");
    $display("  TEST=%s  SEED=%0d", test_name, seed);
    $display("════════════════════════════════════════════════════════");

    do_reset();

    case (test_name)
      "B01_smoke":       test_B01_smoke();
      "B02_multi_port":  test_B02_multi_port();
      "B03_csr_readback": test_B03_csr_readback();
      "B04_version":     test_B04_version();
      "B06_bin_mapping": test_B06_bin_mapping();
      "B07_clear":       test_B07_clear();
      "B08_interval_latched_counters": test_B08_interval_latched_counters();
      "B09_filter_pass": test_B09_filter_pass();
      "B10_debug_mts_both": test_B10_debug_mts_both();
      "B11_debug_mts_filter_source": test_B11_debug_mts_filter_source();
      "E01_underflow":   test_E01_underflow();
      "E02_overflow":    test_E02_overflow();
      "E06_invalid_bounds": test_E06_invalid_bounds();
      "P01_sustained_fill": test_P01_sustained_fill();
      "P03_wire_burst_absorb": test_P03_wire_burst_absorb();
      "P04_all_channel_injection_frame": test_P04_all_channel_injection_frame();
      "P02_all_ports_soak": test_P02_all_ports_soak();
      "R01_reset_mid_fill": test_R01_reset_mid_fill();
      "ALL": begin
        test_B01_smoke();       do_reset();
        test_B02_multi_port();  do_reset();
        test_B03_csr_readback();do_reset();
        test_B04_version();     do_reset();
        test_B06_bin_mapping(); do_reset();
        test_B07_clear();       do_reset();
        test_B08_interval_latched_counters(); do_reset();
        test_B09_filter_pass(); do_reset();
        test_B10_debug_mts_both(); do_reset();
        test_B11_debug_mts_filter_source(); do_reset();
        test_E01_underflow();   do_reset();
        test_E02_overflow();    do_reset();
        test_E06_invalid_bounds(); do_reset();
        test_P01_sustained_fill(); do_reset();
        test_P03_wire_burst_absorb(); do_reset();
        test_P04_all_channel_injection_frame(); do_reset();
        test_P02_all_ports_soak(); do_reset();
        test_R01_reset_mid_fill();
      end
      default: begin
        $display("ERROR: unknown test '%s'", test_name);
        error_count++;
      end
    endcase

    $display("════════════════════════════════════════════════════════");
    $display("  RESULT: %0d PASS, %0d FAIL", pass_count, error_count);
    if (error_count > 0)
      $display("  *** FAILURES DETECTED ***");
    else
      $display("  ALL TESTS PASSED");
    $display("════════════════════════════════════════════════════════");

    $finish;
  end

  // ════════════════════════════════════════════════════════════════
  // Global timeout watchdog
  // ════════════════════════════════════════════════════════════════
  initial begin
    #100ms;
    $display("FATAL: global timeout reached (100ms)");
    $fatal(1, "global timeout");
  end

endmodule
