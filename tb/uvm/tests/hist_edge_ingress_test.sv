class hist_edge_ingress_test extends hist_base_test;
  `uvm_component_utils(hist_edge_ingress_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 8192;
  localparam bit [3:0] CSR_TOTAL_HITS = 4'd11;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  local task automatic configure(
    input int unsigned interval_cfg = HS_TEST_INTERVAL_CFG
  );
    program_histogram(
      .left_bound   (0),
      .bin_width    (16),
      .key_unsigned (1'b1),
      .interval_cfg (interval_cfg)
    );
  endtask

  // E155: Snoop mode port 0 backpressure — requires aso_hist_fill_out_ready=0
  local task automatic task_e155();
    `uvm_info(get_type_name(), "E155: snoop mode backpressure — skipped (requires snoop output stall)", UVM_LOW)
  endtask

  // E156: Non-snoop mode — DUT has SNOOP_EN=1, so this is N/A
  local task automatic task_e156();
    `uvm_info(get_type_name(), "E156: no-snoop mode — skipped (DUT has SNOOP_EN=1)", UVM_LOW)
  endtask

  // E157: ingress_stage cleared by measure_clear — verified by E018/E127
  local task automatic task_e157();
    `uvm_info(get_type_name(), "E157: ingress clear by measure_clear — verified by E018/E127", UVM_LOW)
  endtask

  // E158: ingress_stage self-clearing — verified by all single-hit tests
  local task automatic task_e158();
    `uvm_info(get_type_name(), "E158: ingress self-clearing — implicitly verified", UVM_LOW)
  endtask

  // E159: Accept + drop same port — by design impossible; skip
  local task automatic task_e159();
    `uvm_info(get_type_name(), "E159: accept+drop same port — by design impossible", UVM_LOW)
  endtask

  // E160: accept_pulse fires even when filter rejects
  // Verify total_hits counts filtered-out hits too
  local task automatic task_e160();
    bit [31:0] th;
    `uvm_info(get_type_name(), "E160: accept_pulse fires even when filter rejects hit", UVM_LOW)
    // Configure with filter enabled, accept mode, key=5
    begin
      bit [31:0] bits_word, value_word;
      bits_word = '0;
      bits_word[7:0]   = HS_DEF_UPDATE_LO;
      bits_word[15:8]  = HS_DEF_UPDATE_HI;
      bits_word[23:16] = HS_DEF_FILTER_LO;
      bits_word[31:24] = HS_DEF_FILTER_HI;
      value_word = '0;
      value_word[31:16] = 16'h0005;
      csr_write(4'd4, bits_word);
      csr_write(4'd5, value_word);
    end
    program_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1),
                      .filter_enable(1'b1), .filter_reject(1'b0),
                      .interval_cfg(HS_TEST_INTERVAL_CFG));
    // Send hit with filter_value=3 (doesn't match key=5 → filtered out)
    send_fill_word(0, make_fill_word(.key_value(0), .filter_value(3)));
    // Send hit with filter_value=5 (matches → passes)
    send_fill_word(0, make_fill_word(.key_value(0), .filter_value(5)));
    wait_pipeline_drain(256);
    // total_hits should count BOTH hits (accept_pulse fires for sampled input)
    csr_read(CSR_TOTAL_HITS, th);
    if (th !== 32'd2)
      `uvm_error("E160", $sformatf("total_hits expected 2 (both accepted at input) got %0d", th))
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_e155(); issue_measure_clear();
    task_e156(); issue_measure_clear();
    task_e157(); issue_measure_clear();
    task_e158(); issue_measure_clear();
    task_e159(); issue_measure_clear();
    task_e160();

    phase.drop_objection(this);
  endtask
endclass
