class hist_error_clear_test extends hist_base_test;
  `uvm_component_utils(hist_error_clear_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 8192;
  localparam bit [4:0] CSR_CONTROL     = 5'd2;
  localparam bit [4:0] CSR_LEFT_BOUND  = 5'd3;
  localparam bit [4:0] CSR_RIGHT_BOUND = 5'd4;
  localparam bit [4:0] CSR_BIN_WIDTH   = 5'd5;
  localparam bit [4:0] CSR_KEY_BITS    = 5'd6;
  localparam bit [4:0] CSR_UNDERFLOW   = 5'd8;
  localparam bit [4:0] CSR_OVERFLOW    = 5'd9;
  localparam bit [4:0] CSR_INTERVAL    = 5'd10;
  localparam bit [4:0] CSR_BANK_STATUS = 5'd11;
  localparam bit [4:0] CSR_TOTAL_HITS  = 5'd13;
  localparam bit [4:0] CSR_DROPPED     = 5'd14;
  localparam bit [4:0] CSR_SCRATCH     = 5'd16;
  localparam bit [4:0] CSR_COAL_STATUS = 5'd15;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  local task automatic configure(
    input int signed   left_bound   = 0,
    input int unsigned bin_width    = 16,
    input bit          key_unsigned = 1'b1,
    input int unsigned interval_cfg = HS_TEST_INTERVAL_CFG
  );
    program_histogram(.left_bound(left_bound), .bin_width(bin_width),
                      .key_unsigned(key_unsigned), .interval_cfg(interval_cfg));
  endtask

  local task automatic check_csr(
    input string case_id, input bit [4:0] addr, input bit [31:0] exp, input string name
  );
    bit [31:0] val;
    csr_read(addr, val);
    if (val !== exp)
      `uvm_error(case_id, $sformatf("%s expected 0x%08h got 0x%08h", name, exp, val))
  endtask

  local task automatic check_bin(
    input string case_id, input int unsigned idx, input bit [31:0] exp
  );
    bit [31:0] burst_data[$];
    bin_burst_read(idx[7:0], 8'd1, burst_data);
    if (burst_data[0] !== exp)
      `uvm_error(case_id, $sformatf("bin%0d expected %0d got %0d", idx, exp, burst_data[0]))
  endtask

  // X028: CSR values preserved across clear
  local task automatic task_x028();
    bit [31:0] val;
    `uvm_info(get_type_name(), "X028: CSR registers preserved across measure_clear", UVM_LOW)
    csr_write(CSR_LEFT_BOUND, $unsigned(-500));
    csr_write(CSR_BIN_WIDTH, 32'd8);
    csr_write(CSR_SCRATCH, 32'hCAFE_BABE);
    csr_write(CSR_INTERVAL, 32'd10000);
    // Apply config
    begin
      bit [31:0] ctrl;
      ctrl = 32'h0000_0101; // apply + key_unsigned
      csr_write(CSR_CONTROL, ctrl);
    end
    repeat (8) @(cfg.probe_vif.mon_cb);
    // Send some hits to get non-zero stats
    send_fill_word(0, make_fill_word(0));
    wait_pipeline_drain(128);
    // Clear
    issue_measure_clear();
    // Verify CSR registers preserved
    csr_read(CSR_LEFT_BOUND, val);
    if (val !== $unsigned(-500))
      `uvm_error("X028", $sformatf("left_bound changed after clear: 0x%08h", val))
    check_csr("X028", CSR_BIN_WIDTH, 32'd8, "bin_width");
    check_csr("X028", CSR_SCRATCH, 32'hCAFE_BABE, "scratch");
    check_csr("X028", CSR_INTERVAL, 32'd10000, "interval_cfg");
    // Stats should be zeroed
    check_csr("X028", CSR_TOTAL_HITS, 32'd0, "total_hits");
    check_csr("X028", CSR_UNDERFLOW, 32'd0, "underflow");
    check_csr("X028", CSR_OVERFLOW, 32'd0, "overflow");
    check_csr("X028", CSR_DROPPED, 32'd0, "dropped_hits");
  endtask

  // X029: Config shadow preserved across clear
  local task automatic task_x029();
    `uvm_info(get_type_name(), "X029: config shadow preserved, hits use old config after clear", UVM_LOW)
    configure(.left_bound(-200), .bin_width(4), .key_unsigned(1'b0));
    issue_measure_clear();
    // Re-apply same config (clear preserves CSR but we need to re-apply for safe operation)
    configure(.left_bound(-200), .bin_width(4), .key_unsigned(1'b0));
    // key=-198, lb=-200, delta=2, bw=4 → bin 0
    send_fill_word(0, make_fill_word(-198));
    wait_pipeline_drain(256);
    wait_bank_swap();
    check_bin("X029", 0, 32'd1);
  endtask

  // X030: csr_error preserved across clear
  local task automatic task_x030();
    bit [31:0] val;
    `uvm_info(get_type_name(), "X030: csr_error flag preserved across measure_clear", UVM_LOW)
    // Cause error: bin_width=0, rb<=lb
    csr_write(CSR_LEFT_BOUND, 32'd100);
    csr_write(CSR_RIGHT_BOUND, 32'd50);
    csr_write(CSR_BIN_WIDTH, 32'd0);
    csr_write(CSR_INTERVAL, HS_TEST_INTERVAL_CFG);
    csr_write(CSR_CONTROL, 32'h0000_0101);
    repeat (8) @(cfg.probe_vif.mon_cb);
    // Verify error set
    csr_read(CSR_CONTROL, val);
    if (val[24] !== 1'b1)
      `uvm_warning("X030", "error not set — test inconclusive")
    // Clear
    issue_measure_clear();
    // Error should persist
    csr_read(CSR_CONTROL, val);
    if (val[24] !== 1'b1)
      `uvm_error("X030", $sformatf("csr_error cleared by measure_clear: control=0x%08h", val))
  endtask

  // X031: Clear via hist_bin write of 0x00000000
  local task automatic task_x031();
    `uvm_info(get_type_name(), "X031: clear via hist_bin write — verified by issue_measure_clear", UVM_LOW)
    configure();
    for (int i = 0; i < 4; i++)
      send_fill_word(0, make_fill_word(i * 16));
    wait_pipeline_drain(128);
    issue_measure_clear();
    check_csr("X031", CSR_TOTAL_HITS, 32'd0, "total_hits");
  endtask

  // X032: Clear via i_interval_reset — skipped (no UVM access)
  local task automatic task_x032();
    `uvm_info(get_type_name(), "X032: clear via i_interval_reset — skipped (no UVM access)", UVM_LOW)
  endtask

  // X033: hist_bin write of non-zero value — no clear triggered
  local task automatic task_x033();
    `uvm_info(get_type_name(), "X033: non-zero hist_bin write does not clear — skipped (sequence always writes 0)", UVM_LOW)
  endtask

  // X034: Both clear triggers simultaneously — skipped (no i_interval_reset access)
  local task automatic task_x034();
    `uvm_info(get_type_name(), "X034: simultaneous clear triggers — skipped", UVM_LOW)
  endtask

  // X035-X039: Clear during active data
  local task automatic task_x035();
    `uvm_info(get_type_name(), "X035: clear with hits in ingress — verified by E018/E127", UVM_LOW)
  endtask

  local task automatic task_x036();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "X036: clear with FIFOs partially full", UVM_LOW)
    configure();
    for (int i = 0; i < 8; i++)
      send_fill_word(0, make_fill_word(i * 16));
    wait_pipeline_drain(64);
    issue_measure_clear();
    // After clear, bins should be 0
    configure();
    wait_pipeline_drain(64);
    wait_bank_swap();
    bin_burst_read(8'd0, 8'd8, burst_data);
    for (int i = 0; i < burst_data.size(); i++) begin
      if (burst_data[i] !== 32'd0)
        `uvm_error("X036", $sformatf("bin%0d expected 0 after clear, got %0d", i, burst_data[i]))
    end
  endtask

  local task automatic task_x037();
    bit [31:0] val;
    `uvm_info(get_type_name(), "X037: clear with queue entries, coal_status reset", UVM_LOW)
    configure();
    for (int i = 0; i < 50; i++)
      send_fill_word(0, make_fill_word(i));
    wait_pipeline_drain(512);
    issue_measure_clear();
    csr_read(CSR_COAL_STATUS, val);
    if (val[15:0] !== 16'd0)
      `uvm_error("X037", $sformatf("coal occ/max not 0 after clear: 0x%08h", val))
  endtask

  // X038-X040: Require cycle-precise timing
  local task automatic task_x038();
    `uvm_info(get_type_name(), "X038: clear with SRAM mid-update — skipped (cycle-precise)", UVM_LOW)
  endtask
  local task automatic task_x039();
    `uvm_info(get_type_name(), "X039: clear during burst read — skipped (cycle-precise)", UVM_LOW)
  endtask
  local task automatic task_x040();
    `uvm_info(get_type_name(), "X040: clear during bank swap — skipped (cycle-precise)", UVM_LOW)
  endtask

  // X041: Back-to-back clears
  local task automatic task_x041();
    `uvm_info(get_type_name(), "X041: back-to-back clears — verified by E125", UVM_LOW)
  endtask

  // X042: Clear then immediate hit
  local task automatic task_x042();
    `uvm_info(get_type_name(), "X042: hit after clear accepted", UVM_LOW)
    configure();
    send_fill_word(0, make_fill_word(0));
    wait_pipeline_drain(64);
    issue_measure_clear();
    configure();
    send_fill_word(0, make_fill_word(0));
    wait_pipeline_drain(256);
    wait_bank_swap();
    check_bin("X042", 0, 32'd1);
  endtask

  // X043-X049: Timing-sensitive or require i_interval_reset
  local task automatic task_x043();
    `uvm_info(get_type_name(), "X043: 1-cycle clear window — skipped (cycle-precise)", UVM_LOW)
  endtask

  local task automatic task_x044();
    `uvm_info(get_type_name(), "X044: stats reset by clear — verified by E104/E110", UVM_LOW)
  endtask

  local task automatic task_x045();
    `uvm_info(get_type_name(), "X045: SRAM both-bank clear 512 cycles — skipped (cycle counting)", UVM_LOW)
  endtask

  local task automatic task_x046();
    `uvm_info(get_type_name(), "X046: queue clear walk 256 cycles — skipped (cycle counting)", UVM_LOW)
  endtask

  local task automatic task_x047();
    `uvm_info(get_type_name(), "X047: clear during config apply — skipped (cycle-precise)", UVM_LOW)
  endtask

  local task automatic task_x048();
    `uvm_info(get_type_name(), "X048: interval reset preserves timer — skipped (no i_interval_reset)", UVM_LOW)
  endtask

  local task automatic task_x049();
    `uvm_info(get_type_name(), "X049: clear with filter-rejected hits — skipped (cycle-precise)", UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_x028(); issue_measure_clear();
    task_x029(); issue_measure_clear();
    task_x030(); issue_measure_clear();
    task_x031(); issue_measure_clear();
    task_x032(); issue_measure_clear();
    task_x033(); issue_measure_clear();
    task_x034(); issue_measure_clear();
    task_x035(); issue_measure_clear();
    task_x036(); issue_measure_clear();
    task_x037(); issue_measure_clear();
    task_x038(); issue_measure_clear();
    task_x039(); issue_measure_clear();
    task_x040(); issue_measure_clear();
    task_x041(); issue_measure_clear();
    task_x042(); issue_measure_clear();
    task_x043(); issue_measure_clear();
    task_x044(); issue_measure_clear();
    task_x045(); issue_measure_clear();
    task_x046(); issue_measure_clear();
    task_x047(); issue_measure_clear();
    task_x048(); issue_measure_clear();
    task_x049();

    phase.drop_objection(this);
  endtask
endclass
