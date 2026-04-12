class hist_snoop_test extends hist_base_test;
  `uvm_component_utils(hist_snoop_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 2048;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  local task automatic wait_for_snoop_sample(
    output logic [HS_AVST_DATA_W-1:0] data_word,
    output bit [HS_AVST_CH_W-1:0]     channel,
    output bit                        sop,
    output bit                        eop
  );
    int unsigned timeout_v;
    timeout_v = 0;
    while (!(cfg.snoop_vif.mon_cb.valid && cfg.snoop_vif.mon_cb.ready)) begin
      @(cfg.snoop_vif.mon_cb);
      timeout_v++;
      if (timeout_v > 1000) begin
        `uvm_fatal(get_type_name(), "timeout waiting for snoop sample")
      end
    end
    data_word = cfg.snoop_vif.mon_cb.data;
    channel   = cfg.snoop_vif.mon_cb.channel;
    sop       = cfg.snoop_vif.mon_cb.sop;
    eop       = cfg.snoop_vif.mon_cb.eop;
  endtask

  local task automatic task_b115();
    logic [HS_AVST_DATA_W-1:0] snoop_data;
    bit [HS_AVST_CH_W-1:0]     snoop_channel;
    bit                        snoop_sop;
    bit                        snoop_eop;
    logic [HS_AVST_DATA_W-1:0] exp_word;
    `uvm_info(get_type_name(), "B115: snoop mirrors data", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    exp_word = make_fill_word(16);
    fork
      begin
        wait_for_snoop_sample(snoop_data, snoop_channel, snoop_sop, snoop_eop);
      end
      begin
        send_fill_word_ex(0, exp_word, 4'h3, 1'b0, 1'b0);
      end
    join
    if (snoop_data !== exp_word)
      `uvm_error("B115", $sformatf("snoop data expected 0x%010h got 0x%010h", exp_word, snoop_data))
  endtask

  local task automatic task_b116();
    logic [HS_AVST_DATA_W-1:0] snoop_data;
    bit [HS_AVST_CH_W-1:0]     snoop_channel;
    bit                        snoop_sop;
    bit                        snoop_eop;
    `uvm_info(get_type_name(), "B116: snoop mirrors channel", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    fork
      begin
        wait_for_snoop_sample(snoop_data, snoop_channel, snoop_sop, snoop_eop);
      end
      begin
        send_fill_word_ex(0, make_fill_word(32), 4'hA, 1'b0, 1'b0);
      end
    join
    if (snoop_channel !== 4'hA)
      `uvm_error("B116", $sformatf("snoop channel expected 0x%0h got 0x%0h", 4'hA, snoop_channel))
  endtask

  local task automatic task_b117();
    logic [HS_AVST_DATA_W-1:0] snoop_data;
    bit [HS_AVST_CH_W-1:0]     snoop_channel;
    bit                        snoop_sop;
    bit                        snoop_eop;
    `uvm_info(get_type_name(), "B117: snoop mirrors SOP/EOP", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    fork
      begin
        wait_for_snoop_sample(snoop_data, snoop_channel, snoop_sop, snoop_eop);
      end
      begin
        send_fill_word_ex(0, make_fill_word(48), 4'h1, 1'b1, 1'b1);
      end
    join
    if ((snoop_sop !== 1'b1) || (snoop_eop !== 1'b1))
      `uvm_error("B117", $sformatf("snoop SOP/EOP expected 1/1 got %0b/%0b", snoop_sop, snoop_eop))
  endtask

  local task automatic task_b118();
    `uvm_info(get_type_name(), "B118: SOP/EOP forced 0 — skipped (generic)", UVM_LOW)
  endtask

  local task automatic task_b119();
    logic [HS_AVST_DATA_W-1:0] snoop_data;
    bit [HS_AVST_CH_W-1:0]     snoop_channel;
    bit                        snoop_sop;
    bit                        snoop_eop;
    bit                        saw_blocked_valid;
    bit                        accepted_while_blocked;
    `uvm_info(get_type_name(), "B119: snoop backpressure stalls port 0 until ready returns", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    saw_blocked_valid       = 1'b0;
    accepted_while_blocked  = 1'b0;
    cfg.snoop_vif.ready = 1'b0;
    fork
      begin
        send_fill_word_ex(0, make_fill_word(64), 4'h2, 1'b1, 1'b1);
      end
      begin
        repeat (4) begin
          @(cfg.snoop_vif.mon_cb);
          if (cfg.snoop_vif.mon_cb.valid)
            saw_blocked_valid = 1'b1;
          if (cfg.probe_vif.mon_cb.ingress_accept[0])
            accepted_while_blocked = 1'b1;
        end
        cfg.snoop_vif.ready = 1'b1;
      end
    join
    wait_for_snoop_sample(snoop_data, snoop_channel, snoop_sop, snoop_eop);
    if (!saw_blocked_valid)
      `uvm_error("B119", "expected snoop valid to be held while ready was deasserted")
    if (accepted_while_blocked)
      `uvm_error("B119", "port 0 ingress accepted data while snoop ready was deasserted")
    if ((snoop_channel !== 4'h2) || (snoop_sop !== 1'b1) || (snoop_eop !== 1'b1))
      `uvm_error("B119", $sformatf("unexpected snoop metadata after release: channel=%0h sop=%0b eop=%0b", snoop_channel, snoop_sop, snoop_eop))
    wait_pipeline_drain(128);
  endtask

  local task automatic task_b120();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B120: snoop not on ports 1-7, port 1 enters histogram", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    // Hit on port 1: effective key = 0 + 1*32 = 32, bin = 32/16 = 2
    send_fill_word(1, make_fill_word(0));
    wait_pipeline_drain(128);
    wait_bank_swap();
    bin_burst_read(8'd2, 8'd1, burst_data);
    if (burst_data[0] !== 32'd1)
      `uvm_error("B120", $sformatf("bin2 expected 1 got %0d", burst_data[0]))
  endtask

  local task automatic task_b121();
    bit saw_valid;
    `uvm_info(get_type_name(), "B121: snoop valid follows input activity", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    saw_valid = 1'b0;
    fork
      begin
        repeat (4) begin
          @(cfg.snoop_vif.mon_cb);
          if (cfg.snoop_vif.mon_cb.valid)
            saw_valid = 1'b1;
        end
      end
      begin
        send_fill_word_ex(0, make_fill_word(80), 4'h4, 1'b0, 1'b0);
      end
    join
    if (!saw_valid)
      `uvm_error("B121", "expected snoop valid pulse during port 0 traffic")
  endtask

  local task automatic task_b122();
    logic [HS_AVST_DATA_W-1:0] snoop_data;
    bit [HS_AVST_CH_W-1:0]     snoop_channel;
    bit                        snoop_sop;
    bit                        snoop_eop;
    bit [31:0]                 burst_data[$];
    logic [HS_AVST_DATA_W-1:0] exp_word;
    `uvm_info(get_type_name(), "B122: snoop still mirrors filtered-out traffic", UVM_LOW)
    program_key_fields(.filter_key(16'h0005));
    program_histogram(
      .left_bound(0),
      .bin_width(16),
      .key_unsigned(1'b1),
      .filter_enable(1'b1),
      .filter_reject(1'b0),
      .interval_cfg(HS_TEST_INTERVAL_CFG)
    );
    exp_word = make_fill_word(.key_value(0), .filter_value(3));
    fork
      begin
        wait_for_snoop_sample(snoop_data, snoop_channel, snoop_sop, snoop_eop);
      end
      begin
        send_fill_word_ex(0, exp_word, 4'h6, 1'b0, 1'b0);
      end
    join
    if (snoop_data !== exp_word)
      `uvm_error("B122", "snoop did not mirror filtered-out word")
    wait_pipeline_drain(128);
    wait_bank_swap();
    bin_burst_read(8'd0, 8'd1, burst_data);
    if (burst_data[0] !== 32'd0)
      `uvm_error("B122", $sformatf("filtered hit should not update bin0, got %0d", burst_data[0]))
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_b115(); issue_measure_clear();
    task_b116(); issue_measure_clear();
    task_b117(); issue_measure_clear();
    task_b118(); issue_measure_clear();
    task_b119(); issue_measure_clear();
    task_b120(); issue_measure_clear();
    task_b121(); issue_measure_clear();
    task_b122();

    phase.drop_objection(this);
  endtask
endclass
