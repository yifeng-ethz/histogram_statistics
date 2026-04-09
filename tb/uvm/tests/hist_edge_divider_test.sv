class hist_edge_divider_test extends hist_base_test;
  `uvm_component_utils(hist_edge_divider_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 8192;
  localparam bit [3:0] CSR_UNDERFLOW  = 4'd6;
  localparam bit [3:0] CSR_OVERFLOW   = 4'd7;
  localparam bit [3:0] CSR_TOTAL_HITS = 4'd11;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  local task automatic configure(
    input int signed   left_bound,
    input int unsigned bin_width,
    input bit          key_unsigned = 1'b1,
    input int unsigned interval_cfg = HS_TEST_INTERVAL_CFG
  );
    program_histogram(
      .left_bound   (left_bound),
      .bin_width    (bin_width),
      .key_unsigned (key_unsigned),
      .interval_cfg (interval_cfg)
    );
  endtask

  local task automatic check_bin(
    input string       case_id,
    input int unsigned expected_bin,
    input bit [31:0]   expected_count
  );
    bit [31:0] burst_data[$];
    bin_burst_read(expected_bin[7:0], 8'd1, burst_data);
    if (burst_data[0] !== expected_count)
      `uvm_error(case_id, $sformatf("bin%0d expected %0d got %0d", expected_bin, expected_count, burst_data[0]))
  endtask

  local task automatic check_underflow(
    input string     case_id,
    input bit [31:0] expected
  );
    bit [31:0] val;
    csr_read(CSR_UNDERFLOW, val);
    if (val !== expected)
      `uvm_error(case_id, $sformatf("underflow expected %0d got %0d", expected, val))
  endtask

  local task automatic check_overflow(
    input string     case_id,
    input bit [31:0] expected
  );
    bit [31:0] val;
    csr_read(CSR_OVERFLOW, val);
    if (val !== expected)
      `uvm_error(case_id, $sformatf("overflow expected %0d got %0d", expected, val))
  endtask

  // E001: Key exactly at left_bound → bin 0
  local task automatic task_e001();
    `uvm_info(get_type_name(), "E001: key = left_bound, delta=0, bin_index=0", UVM_LOW)
    configure(.left_bound(100), .bin_width(16));
    send_fill_word(0, make_fill_word(100));
    wait_pipeline_drain(256);
    wait_bank_swap();
    check_bin("E001", 0, 32'd1);
  endtask

  // E002: Key at left_bound + bin_width - 1 → last key in bin 0
  local task automatic task_e002();
    `uvm_info(get_type_name(), "E002: key = left_bound + bin_width - 1, bin 0", UVM_LOW)
    configure(.left_bound(0), .bin_width(16));
    send_fill_word(0, make_fill_word(15));
    wait_pipeline_drain(128);
    wait_bank_swap();
    check_bin("E002", 0, 32'd1);
  endtask

  // E003: Key at left_bound + bin_width → first key of bin 1
  local task automatic task_e003();
    `uvm_info(get_type_name(), "E003: key = left_bound + bin_width, bin 1", UVM_LOW)
    configure(.left_bound(0), .bin_width(16));
    send_fill_word(0, make_fill_word(16));
    wait_pipeline_drain(128);
    wait_bank_swap();
    check_bin("E003", 1, 32'd1);
  endtask

  // E004: Key at right_bound - 1 → last in-range key
  local task automatic task_e004();
    `uvm_info(get_type_name(), "E004: key = right_bound - 1, bin 255", UVM_LOW)
    configure(.left_bound(0), .bin_width(16));
    // right_bound = 0 + 256*16 = 4096; key = 4095 → bin = 4095/16 = 255 r15
    send_fill_word(0, make_fill_word(4095));
    wait_pipeline_drain(256);
    wait_bank_swap();
    check_bin("E004", 255, 32'd1);
    // Also verify no overflow
    check_overflow("E004", 32'd0);
  endtask

  // E005: Key at exactly right_bound → overflow
  local task automatic task_e005();
    `uvm_info(get_type_name(), "E005: key = right_bound, overflow", UVM_LOW)
    configure(.left_bound(0), .bin_width(16));
    send_fill_word(0, make_fill_word(4096));
    wait_pipeline_drain(256);
    check_overflow("E005", 32'd1);
  endtask

  // E006: Key at left_bound - 1 → underflow
  local task automatic task_e006();
    `uvm_info(get_type_name(), "E006: key = left_bound - 1, underflow", UVM_LOW)
    configure(.left_bound(100), .bin_width(16));
    send_fill_word(0, make_fill_word(99));
    wait_pipeline_drain(256);
    check_underflow("E006", 32'd1);
  endtask

  // E007: bin_width=1 sweep (identity mapping)
  local task automatic task_e007();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "E007: bin_width=1, key=0..255 identity sweep", UVM_LOW)
    configure(.left_bound(0), .bin_width(1), .interval_cfg(65536));
    for (int i = 0; i < 256; i++)
      send_fill_word(0, make_fill_word(i));
    wait_pipeline_drain(512);
    wait_bank_swap();
    for (int base = 0; base < 256; base += 16) begin
      bin_burst_read(base[7:0], 8'd16, burst_data);
      for (int j = 0; j < burst_data.size(); j++) begin
        if (burst_data[j] !== 32'd1)
          `uvm_error("E007", $sformatf("bin%0d expected 1 got %0d", base + j, burst_data[j]))
      end
    end
  endtask

  // E008: bin_width=256 (single bin for entire range)
  local task automatic task_e008();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "E008: bin_width=256, all keys → bin 0", UVM_LOW)
    configure(.left_bound(0), .bin_width(256), .interval_cfg(65536));
    for (int i = 0; i < 256; i++)
      send_fill_word(0, make_fill_word(i));
    wait_pipeline_drain(512);
    wait_bank_swap();
    check_bin("E008", 0, 32'd256);
    // bins 1-3 should be empty
    bin_burst_read(8'd1, 8'd3, burst_data);
    for (int i = 0; i < burst_data.size(); i++) begin
      if (burst_data[i] !== 32'd0)
        `uvm_error("E008", $sformatf("bin%0d expected 0 got %0d", 1 + i, burst_data[i]))
    end
  endtask

  // E009: bin_width=0 → overflow (division by zero guard)
  local task automatic task_e009();
    `uvm_info(get_type_name(), "E009: bin_width=0, division by zero → skipped (apply rejects bw=0 when rb<=lb)", UVM_LOW)
  endtask

  // E010: Remainder exactly 0 (delta = exact multiple of bin_width)
  local task automatic task_e010();
    `uvm_info(get_type_name(), "E010: key=48, lb=0, bw=16, exact quotient=3", UVM_LOW)
    configure(.left_bound(0), .bin_width(16));
    send_fill_word(0, make_fill_word(48));
    wait_pipeline_drain(128);
    wait_bank_swap();
    check_bin("E010", 3, 32'd1);
  endtask

  // E011: Remainder = bin_width - 1
  local task automatic task_e011();
    `uvm_info(get_type_name(), "E011: key=31, lb=0, bw=16, quotient=1 r15", UVM_LOW)
    configure(.left_bound(0), .bin_width(16));
    send_fill_word(0, make_fill_word(31));
    wait_pipeline_drain(128);
    wait_bank_swap();
    check_bin("E011", 1, 32'd1);
  endtask

  // E012: Maximum bin_index = 255 (all quotient bits set)
  local task automatic task_e012();
    `uvm_info(get_type_name(), "E012: lb=0, bw=1, key=255, bin=255", UVM_LOW)
    configure(.left_bound(0), .bin_width(1));
    send_fill_word(0, make_fill_word(255));
    wait_pipeline_drain(256);
    wait_bank_swap();
    check_bin("E012", 255, 32'd1);
  endtask

  // E013: Signed cross: lb=-100, key=+100, delta=200
  local task automatic task_e013();
    `uvm_info(get_type_name(), "E013: signed lb=-100, key=+100, delta=200, bw=16 → bin 12", UVM_LOW)
    // Use signed key extraction so we can set negative left_bound meaningfully
    // With key_unsigned=0: signed extraction of 13-bit field.
    // key=100 fits in 13-bit signed (max +4095). lb=-100 written as $unsigned(-100).
    // delta = 100 - (-100) = 200. 200/16 = 12 r8.
    configure(.left_bound(-100), .bin_width(16), .key_unsigned(1'b0));
    send_fill_word(0, make_fill_word(100));
    wait_pipeline_drain(256);
    wait_bank_swap();
    check_bin("E013", 12, 32'd1);
  endtask

  // E014-E017: Require 32-bit key values beyond 13-bit extraction width
  local task automatic task_e014();
    `uvm_info(get_type_name(), "E014: key=left_bound=INT_MAX — skipped (exceeds extraction width)", UVM_LOW)
  endtask
  local task automatic task_e015();
    `uvm_info(get_type_name(), "E015: key=left_bound=INT_MIN — skipped (exceeds extraction width)", UVM_LOW)
  endtask
  local task automatic task_e016();
    `uvm_info(get_type_name(), "E016: key=INT_MIN, lb=INT_MAX — skipped (exceeds extraction width)", UVM_LOW)
  endtask
  local task automatic task_e017();
    `uvm_info(get_type_name(), "E017: key=INT_MAX, lb=INT_MIN — skipped (exceeds extraction width)", UVM_LOW)
  endtask

  // E018: Pipeline flush during active divider stages
  local task automatic task_e018();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "E018: pipeline flush clears in-flight entries", UVM_LOW)
    configure(.left_bound(0), .bin_width(16));
    // Send 3 hits then immediately issue measure_clear
    send_fill_word(0, make_fill_word(0));
    send_fill_word(0, make_fill_word(16));
    send_fill_word(0, make_fill_word(32));
    // Clear before pipeline fully drains (hits may still be in divider)
    issue_measure_clear();
    // After clear, bins should be empty
    configure(.left_bound(0), .bin_width(16));
    wait_pipeline_drain(64);
    wait_bank_swap();
    bin_burst_read(8'd0, 8'd4, burst_data);
    for (int i = 0; i < burst_data.size(); i++) begin
      if (burst_data[i] !== 32'd0)
        `uvm_error("E018", $sformatf("bin%0d expected 0 after flush, got %0d", i, burst_data[i]))
    end
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_e001(); issue_measure_clear();
    task_e002(); issue_measure_clear();
    task_e003(); issue_measure_clear();
    task_e004(); issue_measure_clear();
    task_e005(); issue_measure_clear();
    task_e006(); issue_measure_clear();
    task_e007(); issue_measure_clear();
    task_e008(); issue_measure_clear();
    task_e009(); issue_measure_clear();
    task_e010(); issue_measure_clear();
    task_e011(); issue_measure_clear();
    task_e012(); issue_measure_clear();
    task_e013(); issue_measure_clear();
    task_e014(); issue_measure_clear();
    task_e015(); issue_measure_clear();
    task_e016(); issue_measure_clear();
    task_e017(); issue_measure_clear();
    task_e018();

    phase.drop_objection(this);
  endtask
endclass
