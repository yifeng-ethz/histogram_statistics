class hist_error_config_test extends hist_base_test;
  `uvm_component_utils(hist_error_config_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 8192;
  localparam bit [3:0] CSR_CONTROL     = 4'd0;
  localparam bit [3:0] CSR_LEFT_BOUND  = 4'd1;
  localparam bit [3:0] CSR_RIGHT_BOUND = 4'd2;
  localparam bit [3:0] CSR_BIN_WIDTH   = 4'd3;
  localparam bit [3:0] CSR_KEY_BITS    = 4'd4;
  localparam bit [3:0] CSR_KEY_VAL     = 4'd5;
  localparam bit [3:0] CSR_INTERVAL    = 4'd8;
  localparam bit [3:0] CSR_TOTAL_HITS  = 4'd11;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  local task automatic apply_raw(
    input bit key_unsigned = 1'b1,
    input bit filter_enable = 1'b0,
    input bit filter_reject = 1'b0
  );
    bit [31:0] ctrl;
    ctrl = '0;
    ctrl[0]  = 1'b1;
    ctrl[8]  = key_unsigned;
    ctrl[12] = filter_enable;
    ctrl[13] = filter_reject;
    csr_write(4'd0, ctrl);
    repeat (16) @(cfg.probe_vif.mon_cb);
  endtask

  local task automatic check_error(input string case_id, input bit expected);
    bit [31:0] val;
    csr_read(CSR_CONTROL, val);
    if (val[24] !== expected)
      `uvm_error(case_id, $sformatf("csr_error expected %0b got %0b (control=0x%08h)", expected, val[24], val))
  endtask

  local task automatic check_bin(input string case_id, input int unsigned idx, input bit [31:0] exp);
    bit [31:0] burst_data[$];
    bin_burst_read(idx[7:0], 8'd1, burst_data);
    if (burst_data[0] !== exp)
      `uvm_error(case_id, $sformatf("bin%0d expected %0d got %0d", idx, exp, burst_data[0]))
  endtask

  // X050: bin_width=0, rb < lb → error
  local task automatic task_x050();
    `uvm_info(get_type_name(), "X050: bin_width=0, rb<lb → error", UVM_LOW)
    csr_write(CSR_LEFT_BOUND, 32'd100);
    csr_write(CSR_RIGHT_BOUND, 32'd50);
    csr_write(CSR_BIN_WIDTH, 32'd0);
    csr_write(CSR_INTERVAL, HS_TEST_INTERVAL_CFG);
    apply_raw();
    check_error("X050", 1'b1);
  endtask

  // X051: bin_width=0, rb = lb → error
  local task automatic task_x051();
    `uvm_info(get_type_name(), "X051: bin_width=0, rb=lb → error", UVM_LOW)
    csr_write(CSR_LEFT_BOUND, 32'd100);
    csr_write(CSR_RIGHT_BOUND, 32'd100);
    csr_write(CSR_BIN_WIDTH, 32'd0);
    csr_write(CSR_INTERVAL, HS_TEST_INTERVAL_CFG);
    apply_raw();
    check_error("X051", 1'b1);
  endtask

  // X052: bin_width=0, rb > lb → no error (manual bounds mode)
  local task automatic task_x052();
    `uvm_info(get_type_name(), "X052: bin_width=0, rb>lb → no error (manual mode)", UVM_LOW)
    csr_write(CSR_LEFT_BOUND, 32'd0);
    csr_write(CSR_RIGHT_BOUND, 32'd256);
    csr_write(CSR_BIN_WIDTH, 32'd0);
    csr_write(CSR_INTERVAL, HS_TEST_INTERVAL_CFG);
    apply_raw();
    check_error("X052", 1'b0);
  endtask

  // X053: bin_width=1, auto-compute right_bound
  local task automatic task_x053();
    bit [31:0] val;
    `uvm_info(get_type_name(), "X053: bin_width=1, lb=-128 → rb=128", UVM_LOW)
    csr_write(CSR_LEFT_BOUND, $unsigned(-128));
    csr_write(CSR_BIN_WIDTH, 32'd1);
    csr_write(CSR_INTERVAL, HS_TEST_INTERVAL_CFG);
    apply_raw();
    check_error("X053", 1'b0);
    csr_read(CSR_RIGHT_BOUND, val);
    // -128 + 1*256 = 128
    if ($signed(val) !== 128)
      `uvm_error("X053", $sformatf("right_bound expected 128 got %0d", $signed(val)))
  endtask

  // X054: bin_width=0xFFFF (maximum), auto-compute right_bound
  local task automatic task_x054();
    bit [31:0] val;
    `uvm_info(get_type_name(), "X054: bin_width=0xFFFF, lb=0 → rb=16776960", UVM_LOW)
    csr_write(CSR_LEFT_BOUND, 32'd0);
    csr_write(CSR_BIN_WIDTH, 32'h0000_FFFF);
    csr_write(CSR_INTERVAL, HS_TEST_INTERVAL_CFG);
    apply_raw();
    check_error("X054", 1'b0);
    csr_read(CSR_RIGHT_BOUND, val);
    // 0 + 65535*256 = 16776960
    if (val !== 32'd16776960)
      `uvm_error("X054", $sformatf("right_bound expected 16776960 got %0d", val))
  endtask

  // X055: Negative lb with bin_width=0, rb < lb → error
  local task automatic task_x055();
    `uvm_info(get_type_name(), "X055: lb=-100, rb=-200, bw=0 → error", UVM_LOW)
    csr_write(CSR_LEFT_BOUND, $unsigned(-100));
    csr_write(CSR_RIGHT_BOUND, $unsigned(-200));
    csr_write(CSR_BIN_WIDTH, 32'd0);
    csr_write(CSR_INTERVAL, HS_TEST_INTERVAL_CFG);
    apply_raw();
    check_error("X055", 1'b1);
  endtask

  // X056: Large negative lb, rb just above → no error
  local task automatic task_x056();
    `uvm_info(get_type_name(), "X056: lb=INT_MIN, rb=INT_MIN+1, bw=0 → no error", UVM_LOW)
    csr_write(CSR_LEFT_BOUND, 32'h8000_0000); // -2147483648
    csr_write(CSR_RIGHT_BOUND, 32'h8000_0001); // -2147483647
    csr_write(CSR_BIN_WIDTH, 32'd0);
    csr_write(CSR_INTERVAL, HS_TEST_INTERVAL_CFG);
    apply_raw();
    check_error("X056", 1'b0);
  endtask

  // X057: Error cleared on next valid apply
  local task automatic task_x057();
    `uvm_info(get_type_name(), "X057: error cleared by valid apply", UVM_LOW)
    // Set error
    csr_write(CSR_LEFT_BOUND, 32'd100);
    csr_write(CSR_RIGHT_BOUND, 32'd50);
    csr_write(CSR_BIN_WIDTH, 32'd0);
    csr_write(CSR_INTERVAL, HS_TEST_INTERVAL_CFG);
    apply_raw();
    check_error("X057-pre", 1'b1);
    // Valid apply
    csr_write(CSR_LEFT_BOUND, 32'd0);
    csr_write(CSR_BIN_WIDTH, 32'd4);
    apply_raw();
    check_error("X057", 1'b0);
  endtask

  // X058: Error re-set after clear-then-set in same apply
  local task automatic task_x058();
    `uvm_info(get_type_name(), "X058: bad apply after bad apply — error still set", UVM_LOW)
    csr_write(CSR_LEFT_BOUND, 32'd100);
    csr_write(CSR_RIGHT_BOUND, 32'd50);
    csr_write(CSR_BIN_WIDTH, 32'd0);
    apply_raw();
    check_error("X058-1", 1'b1);
    // Another bad apply
    csr_write(CSR_LEFT_BOUND, 32'd200);
    csr_write(CSR_RIGHT_BOUND, 32'd100);
    csr_write(CSR_BIN_WIDTH, 32'd0);
    apply_raw();
    check_error("X058", 1'b1);
  endtask

  // X059: Error survives non-apply CSR writes
  local task automatic task_x059();
    `uvm_info(get_type_name(), "X059: error survives scratch write", UVM_LOW)
    csr_write(CSR_LEFT_BOUND, 32'd100);
    csr_write(CSR_RIGHT_BOUND, 32'd50);
    csr_write(CSR_BIN_WIDTH, 32'd0);
    apply_raw();
    check_error("X059-pre", 1'b1);
    // Write scratch (non-apply)
    csr_write(4'd15, 32'hABCD_1234);
    csr_write(CSR_LEFT_BOUND, $unsigned(-500));
    check_error("X059", 1'b1);
  endtask

  // X060: Write control without apply bit → error not cleared
  local task automatic task_x060();
    `uvm_info(get_type_name(), "X060: control write without apply → error persists", UVM_LOW)
    csr_write(CSR_LEFT_BOUND, 32'd100);
    csr_write(CSR_RIGHT_BOUND, 32'd50);
    csr_write(CSR_BIN_WIDTH, 32'd0);
    apply_raw();
    check_error("X060-pre", 1'b1);
    // Write control WITHOUT apply bit
    csr_write(CSR_CONTROL, 32'h0000_0000);
    repeat (8) @(cfg.probe_vif.mon_cb);
    check_error("X060", 1'b1);
  endtask

  // X061: Error blocks config shadow update
  local task automatic task_x061();
    bit [31:0] val;
    `uvm_info(get_type_name(), "X061: error blocks shadow update — old config still active", UVM_LOW)
    // Apply valid config: lb=0, bw=16
    program_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    // Attempt bad apply: lb=100, rb=50, bw=0
    csr_write(CSR_LEFT_BOUND, 32'd100);
    csr_write(CSR_RIGHT_BOUND, 32'd50);
    csr_write(CSR_BIN_WIDTH, 32'd0);
    apply_raw();
    check_error("X061-pre", 1'b1);
    // Inject hit with key=0 — should use old config (lb=0, bw=16) → bin 0
    send_fill_word(0, make_fill_word(0));
    wait_pipeline_drain(256);
    wait_bank_swap();
    check_bin("X061", 0, 32'd1);
  endtask

  // X062-X065: Apply interaction with pipeline
  local task automatic task_x062();
    `uvm_info(get_type_name(), "X062: apply blocked while ingress not empty — verified by E084", UVM_LOW)
  endtask

  local task automatic task_x063();
    `uvm_info(get_type_name(), "X063: apply with continuous hits — backpressure resolves", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    // Send some hits
    for (int i = 0; i < 4; i++)
      send_fill_word(0, make_fill_word(i * 16));
    // Apply new config immediately
    program_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    // Verify apply completed
    begin
      bit [31:0] ctrl;
      csr_read(CSR_CONTROL, ctrl);
      if (ctrl[1] !== 1'b0)
        `uvm_error("X063", $sformatf("apply_pending still high: 0x%08h", ctrl))
    end
  endtask

  local task automatic task_x064();
    `uvm_info(get_type_name(), "X064: apply during error — pending not set", UVM_LOW)
    csr_write(CSR_LEFT_BOUND, 32'd100);
    csr_write(CSR_RIGHT_BOUND, 32'd50);
    csr_write(CSR_BIN_WIDTH, 32'd0);
    apply_raw();
    begin
      bit [31:0] ctrl;
      csr_read(CSR_CONTROL, ctrl);
      if (ctrl[1] !== 1'b0)
        `uvm_error("X064", $sformatf("apply_pending set during error: 0x%08h", ctrl))
    end
  endtask

  local task automatic task_x065();
    `uvm_info(get_type_name(), "X065: rapid applies — only one config update", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    // Quick re-apply
    program_histogram(.left_bound(0), .bin_width(32), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    begin
      bit [31:0] rb;
      csr_read(CSR_RIGHT_BOUND, rb);
      // 0 + 32*256 = 8192
      if (rb !== 32'd8192)
        `uvm_error("X065", $sformatf("right_bound expected 8192 got %0d", rb))
    end
  endtask

  // X066: Config change without apply has no effect
  local task automatic task_x066();
    `uvm_info(get_type_name(), "X066: config write without apply — no effect on shadow", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    // Write new lb without apply
    csr_write(CSR_LEFT_BOUND, $unsigned(-999));
    // Don't apply — hits should still use lb=0
    send_fill_word(0, make_fill_word(0));
    wait_pipeline_drain(256);
    wait_bank_swap();
    check_bin("X066", 0, 32'd1);
  endtask

  // X067-X071: Various apply interactions
  local task automatic task_x067();
    `uvm_info(get_type_name(), "X067: apply during clear — skipped (cycle-precise)", UVM_LOW)
  endtask

  local task automatic task_x068();
    bit [31:0] rb;
    `uvm_info(get_type_name(), "X068: bin_width change 0→non-zero auto-computes rb", UVM_LOW)
    // First: manual mode (bw=0, rb=256)
    csr_write(CSR_LEFT_BOUND, 32'd0);
    csr_write(CSR_RIGHT_BOUND, 32'd256);
    csr_write(CSR_BIN_WIDTH, 32'd0);
    apply_raw();
    // Now switch to bw=2
    csr_write(CSR_BIN_WIDTH, 32'd2);
    apply_raw();
    csr_read(CSR_RIGHT_BOUND, rb);
    // 0 + 2*256 = 512
    if (rb !== 32'd512)
      `uvm_error("X068", $sformatf("right_bound expected 512 got %0d", rb))
  endtask

  local task automatic task_x069();
    bit [31:0] rb;
    `uvm_info(get_type_name(), "X069: manual rb overwritten by auto-compute on apply", UVM_LOW)
    csr_write(CSR_LEFT_BOUND, 32'd0);
    csr_write(CSR_RIGHT_BOUND, 32'd1000);
    csr_write(CSR_BIN_WIDTH, 32'd4);
    csr_write(CSR_INTERVAL, HS_TEST_INTERVAL_CFG);
    apply_raw();
    csr_read(CSR_RIGHT_BOUND, rb);
    // 0 + 4*256 = 1024, NOT 1000
    if (rb !== 32'd1024)
      `uvm_error("X069", $sformatf("right_bound expected 1024 got %0d", rb))
  endtask

  local task automatic task_x070();
    `uvm_info(get_type_name(), "X070: key_hi < key_lo degenerate — skipped (undefined behavior)", UVM_LOW)
  endtask

  local task automatic task_x071();
    `uvm_info(get_type_name(), "X071: filter reject mode — verified by E140/E141", UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_x050(); issue_measure_clear();
    task_x051(); issue_measure_clear();
    task_x052(); issue_measure_clear();
    task_x053(); issue_measure_clear();
    task_x054(); issue_measure_clear();
    task_x055(); issue_measure_clear();
    task_x056(); issue_measure_clear();
    task_x057(); issue_measure_clear();
    task_x058(); issue_measure_clear();
    task_x059(); issue_measure_clear();
    task_x060(); issue_measure_clear();
    task_x061(); issue_measure_clear();
    task_x062(); issue_measure_clear();
    task_x063(); issue_measure_clear();
    task_x064(); issue_measure_clear();
    task_x065(); issue_measure_clear();
    task_x066(); issue_measure_clear();
    task_x067(); issue_measure_clear();
    task_x068(); issue_measure_clear();
    task_x069(); issue_measure_clear();
    task_x070(); issue_measure_clear();
    task_x071();

    phase.drop_objection(this);
  endtask
endclass
