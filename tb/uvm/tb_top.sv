`timescale 1ns/1ps

module tb_top;
  import uvm_pkg::*;
  import hist_env_pkg::*;

  localparam int UPDATE_KEY_BIT_HI   = 29;
  localparam int UPDATE_KEY_BIT_LO   = 17;
  localparam int FILTER_KEY_BIT_HI   = 38;
  localparam int FILTER_KEY_BIT_LO   = 35;
  localparam int SAR_TICK_WIDTH      = 32;
  localparam int SAR_KEY_WIDTH       = 16;
  localparam int N_BINS              = 256;
  localparam int MAX_COUNT_BITS      = 32;
  localparam int DEF_LEFT_BOUND      = -1000;
  localparam int DEF_BIN_WIDTH       = 16;
  localparam int AVS_ADDR_WIDTH      = 8;
  localparam int N_PORTS             = 8;
  localparam int CHANNELS_PER_PORT   = 32;
  localparam int COAL_QUEUE_DEPTH    = 256;
  localparam int DEF_INTERVAL_CLOCKS = 125_000_000;
  localparam int AVST_DATA_WIDTH     = 39;
  localparam int AVST_CHANNEL_WIDTH  = 4;
  localparam int N_DEBUG_INTERFACE   = 6;
  localparam int CLK_PERIOD_NS       = 8;

  logic i_clk;
  logic i_rst;
  logic i_interval_reset;

  hist_csr_if #(5) csr_if(i_clk);
  hist_bin_if #(AVS_ADDR_WIDTH) bin_if(i_clk);
  hist_fill_if #(AVST_DATA_WIDTH, AVST_CHANNEL_WIDTH) fill_if [N_PORTS](i_clk);
  hist_snoop_if #(AVST_DATA_WIDTH, AVST_CHANNEL_WIDTH) snoop_if(i_clk);
  hist_ctrl_if ctrl_if(i_clk);
  hist_debug_if #(16) dbg_if [N_DEBUG_INTERFACE](i_clk);
  hist_probe_if #(
    .N_PORTS (N_PORTS),
    .PORT_W  ($clog2(N_PORTS)),
    .BIN_W   ($clog2(N_BINS)),
    .KICK_W  (8),
    .QUEUE_W ($clog2(COAL_QUEUE_DEPTH + 1))
  ) probe_if(i_clk);

  hist_env_cfg env_cfg;

  initial begin
    i_clk = 1'b0;
    forever #(CLK_PERIOD_NS/2) i_clk = ~i_clk;
  end

  initial begin
    csr_if.init_master();
    bin_if.init_master();
    ctrl_if.init_source();
    fill_if[0].init_source();
    fill_if[1].init_source();
    fill_if[2].init_source();
    fill_if[3].init_source();
    fill_if[4].init_source();
    fill_if[5].init_source();
    fill_if[6].init_source();
    fill_if[7].init_source();
    dbg_if[0].init_source();
    dbg_if[1].init_source();
    dbg_if[2].init_source();
    dbg_if[3].init_source();
    dbg_if[4].init_source();
    dbg_if[5].init_source();
    snoop_if.ready   = 1'b1;
    i_interval_reset = 1'b0;
    i_rst            = 1'b1;
    repeat (10) @(posedge i_clk);
    i_rst = 1'b0;
  end

  histogram_statistics_v2 #(
    .UPDATE_KEY_BIT_HI         (UPDATE_KEY_BIT_HI),
    .UPDATE_KEY_BIT_LO         (UPDATE_KEY_BIT_LO),
    .UPDATE_KEY_REPRESENTATION ("UNSIGNED"),
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
    .CHANNELS_PER_PORT         (CHANNELS_PER_PORT),
    .COAL_QUEUE_DEPTH          (COAL_QUEUE_DEPTH),
    .ENABLE_PINGPONG           (1'b1),
    .DEF_INTERVAL_CLOCKS       (DEF_INTERVAL_CLOCKS),
    .AVST_DATA_WIDTH           (AVST_DATA_WIDTH),
    .AVST_CHANNEL_WIDTH        (AVST_CHANNEL_WIDTH),
    .N_DEBUG_INTERFACE         (N_DEBUG_INTERFACE),
    .VERSION_MAJOR             (26),
    .VERSION_MINOR             (0),
    .VERSION_PATCH             (0),
    .BUILD                     (0),
    .SNOOP_EN                  (1'b1),
    .ENABLE_PACKET             (1'b1),
    .DEBUG                     (0)
  ) dut (
    .avs_hist_bin_readdata           (bin_if.readdata),
    .avs_hist_bin_read               (bin_if.read),
    .avs_hist_bin_address            (bin_if.address),
    .avs_hist_bin_waitrequest        (bin_if.waitrequest),
    .avs_hist_bin_write              (bin_if.write),
    .avs_hist_bin_writedata          (bin_if.writedata),
    .avs_hist_bin_burstcount         ({1'b0, bin_if.burstcount}),
    .avs_hist_bin_readdatavalid      (bin_if.readdatavalid),
    .avs_hist_bin_writeresponsevalid (bin_if.writeresponsevalid),
    .avs_hist_bin_response           (bin_if.response),

    .avs_csr_readdata                (csr_if.readdata),
    .avs_csr_read                    (csr_if.read),
    .avs_csr_address                 (csr_if.address),
    .avs_csr_waitrequest             (csr_if.waitrequest),
    .avs_csr_write                   (csr_if.write),
    .avs_csr_writedata               (csr_if.writedata),

    .asi_hist_fill_in_ready          (fill_if[0].ready),
    .asi_hist_fill_in_valid          (fill_if[0].valid),
    .asi_hist_fill_in_data           (fill_if[0].data),
    .asi_hist_fill_in_startofpacket  (fill_if[0].sop),
    .asi_hist_fill_in_endofpacket    (fill_if[0].eop),
    .asi_hist_fill_in_channel        (fill_if[0].channel),

    .asi_fill_in_1_ready             (fill_if[1].ready),
    .asi_fill_in_1_valid             (fill_if[1].valid),
    .asi_fill_in_1_data              (fill_if[1].data),
    .asi_fill_in_1_startofpacket     (fill_if[1].sop),
    .asi_fill_in_1_endofpacket       (fill_if[1].eop),
    .asi_fill_in_1_channel           (fill_if[1].channel),

    .asi_fill_in_2_ready             (fill_if[2].ready),
    .asi_fill_in_2_valid             (fill_if[2].valid),
    .asi_fill_in_2_data              (fill_if[2].data),
    .asi_fill_in_2_startofpacket     (fill_if[2].sop),
    .asi_fill_in_2_endofpacket       (fill_if[2].eop),
    .asi_fill_in_2_channel           (fill_if[2].channel),

    .asi_fill_in_3_ready             (fill_if[3].ready),
    .asi_fill_in_3_valid             (fill_if[3].valid),
    .asi_fill_in_3_data              (fill_if[3].data),
    .asi_fill_in_3_startofpacket     (fill_if[3].sop),
    .asi_fill_in_3_endofpacket       (fill_if[3].eop),
    .asi_fill_in_3_channel           (fill_if[3].channel),

    .asi_fill_in_4_ready             (fill_if[4].ready),
    .asi_fill_in_4_valid             (fill_if[4].valid),
    .asi_fill_in_4_data              (fill_if[4].data),
    .asi_fill_in_4_startofpacket     (fill_if[4].sop),
    .asi_fill_in_4_endofpacket       (fill_if[4].eop),
    .asi_fill_in_4_channel           (fill_if[4].channel),

    .asi_fill_in_5_ready             (fill_if[5].ready),
    .asi_fill_in_5_valid             (fill_if[5].valid),
    .asi_fill_in_5_data              (fill_if[5].data),
    .asi_fill_in_5_startofpacket     (fill_if[5].sop),
    .asi_fill_in_5_endofpacket       (fill_if[5].eop),
    .asi_fill_in_5_channel           (fill_if[5].channel),

    .asi_fill_in_6_ready             (fill_if[6].ready),
    .asi_fill_in_6_valid             (fill_if[6].valid),
    .asi_fill_in_6_data              (fill_if[6].data),
    .asi_fill_in_6_startofpacket     (fill_if[6].sop),
    .asi_fill_in_6_endofpacket       (fill_if[6].eop),
    .asi_fill_in_6_channel           (fill_if[6].channel),

    .asi_fill_in_7_ready             (fill_if[7].ready),
    .asi_fill_in_7_valid             (fill_if[7].valid),
    .asi_fill_in_7_data              (fill_if[7].data),
    .asi_fill_in_7_startofpacket     (fill_if[7].sop),
    .asi_fill_in_7_endofpacket       (fill_if[7].eop),
    .asi_fill_in_7_channel           (fill_if[7].channel),

    .aso_hist_fill_out_ready         (snoop_if.ready),
    .aso_hist_fill_out_valid         (snoop_if.valid),
    .aso_hist_fill_out_data          (snoop_if.data),
    .aso_hist_fill_out_startofpacket (snoop_if.sop),
    .aso_hist_fill_out_endofpacket   (snoop_if.eop),
    .aso_hist_fill_out_channel       (snoop_if.channel),

    .asi_ctrl_data                   (ctrl_if.data),
    .asi_ctrl_valid                  (ctrl_if.valid),
    .asi_ctrl_ready                  (ctrl_if.ready),

    .asi_debug_1_valid               (dbg_if[0].valid),
    .asi_debug_1_data                (dbg_if[0].data),
    .asi_debug_2_valid               (dbg_if[1].valid),
    .asi_debug_2_data                (dbg_if[1].data),
    .asi_debug_3_valid               (dbg_if[2].valid),
    .asi_debug_3_data                (dbg_if[2].data),
    .asi_debug_4_valid               (dbg_if[3].valid),
    .asi_debug_4_data                (dbg_if[3].data),
    .asi_debug_5_valid               (dbg_if[4].valid),
    .asi_debug_5_data                (dbg_if[4].data),
    .asi_debug_6_valid               (dbg_if[5].valid),
    .asi_debug_6_data                (dbg_if[5].data),

    .i_interval_reset                (i_interval_reset),
    .i_rst                           (i_rst),
    .i_clk                           (i_clk)
  );

  assign probe_if.rst                 = i_rst;
  assign probe_if.interval_reset      = i_interval_reset;
  assign probe_if.measure_clear_pulse = dut.measure_clear_pulse;
  assign probe_if.interval_pulse      = dut.interval_pulse;
  assign probe_if.active_bank         = dut.active_bank;
  assign probe_if.flushing            = dut.flushing;
  assign probe_if.cfg_apply_pending   = dut.cfg_apply_pending;
  assign probe_if.ingress_accept      = dut.ingress_accept;
  assign probe_if.fifo_write          = dut.fifo_write;
  assign probe_if.fifo_read           = dut.fifo_read;
  assign probe_if.fifo_empty          = dut.fifo_empty;
  assign probe_if.fifo_full           = dut.fifo_full;
  assign probe_if.drop_pulse          = dut.drop_pulse;
  assign probe_if.arb_valid           = dut.arb_valid;
  assign probe_if.arb_port            = dut.arb_port;
  assign probe_if.divider_valid       = dut.divider_valid;
  assign probe_if.divider_underflow   = dut.divider_underflow;
  assign probe_if.divider_overflow    = dut.divider_overflow;
  assign probe_if.divider_bin_index   = dut.divider_bin_index;
  assign probe_if.queue_hit_valid     = dut.queue_hit_valid;
  assign probe_if.queue_hit_bin       = dut.queue_hit_bin;
  assign probe_if.queue_drain_valid   = dut.queue_drain_valid;
  assign probe_if.queue_drain_ready   = dut.queue_drain_ready;
  assign probe_if.queue_drain_bin     = dut.queue_drain_bin;
  assign probe_if.queue_drain_count   = dut.queue_drain_count;
  assign probe_if.queue_occupancy     = dut.queue_occupancy;
  assign probe_if.queue_occupancy_max = dut.queue_occupancy_max;
  assign probe_if.queue_overflow_count = dut.queue_overflow_count;
  assign probe_if.burst_active        = dut.pingpong_inst.burst_active;
  assign probe_if.read_bank_latched   = dut.pingpong_inst.read_bank_latched;

  genvar fill_idx;
  generate
    for (fill_idx = 0; fill_idx < N_PORTS; fill_idx++) begin : g_avst_sva
      hist_avst_sva #(
        .DATA_W(AVST_DATA_WIDTH),
        .CH_W  (AVST_CHANNEL_WIDTH)
      ) u_hist_avst_sva (
        .clk(i_clk),
        .rst(i_rst),
        .stream_if(fill_if[fill_idx])
      );
    end
  endgenerate

  // Snoop port has ready=1 always — no backpressure assertion needed

  hist_avmm_sva u_hist_avmm_sva (
    .clk(i_clk),
    .rst(i_rst),
    .csr_if(csr_if),
    .bin_if(bin_if)
  );

  hist_fifo_sva #(
    .N_PORTS(N_PORTS)
  ) u_hist_fifo_sva (
    .clk(i_clk),
    .rst(i_rst),
    .probe_if(probe_if)
  );

  hist_pipeline_sva u_hist_pipeline_sva (
    .clk(i_clk),
    .rst(i_rst),
    .probe_if(probe_if)
  );

  initial begin
    env_cfg = hist_env_cfg::type_id::create("env_cfg");
    env_cfg.csr_vif   = csr_if;
    env_cfg.bin_vif   = bin_if;
    env_cfg.ctrl_vif  = ctrl_if;
    env_cfg.snoop_vif = snoop_if;
    env_cfg.probe_vif = probe_if;
    env_cfg.fill_vifs[0] = fill_if[0];
    env_cfg.fill_vifs[1] = fill_if[1];
    env_cfg.fill_vifs[2] = fill_if[2];
    env_cfg.fill_vifs[3] = fill_if[3];
    env_cfg.fill_vifs[4] = fill_if[4];
    env_cfg.fill_vifs[5] = fill_if[5];
    env_cfg.fill_vifs[6] = fill_if[6];
    env_cfg.fill_vifs[7] = fill_if[7];
    env_cfg.dbg_vifs[0]  = dbg_if[0];
    env_cfg.dbg_vifs[1]  = dbg_if[1];
    env_cfg.dbg_vifs[2]  = dbg_if[2];
    env_cfg.dbg_vifs[3]  = dbg_if[3];
    env_cfg.dbg_vifs[4]  = dbg_if[4];
    env_cfg.dbg_vifs[5]  = dbg_if[5];
    uvm_config_db#(hist_env_cfg)::set(null, "*", "env_cfg", env_cfg);
    run_test();
  end
endmodule
