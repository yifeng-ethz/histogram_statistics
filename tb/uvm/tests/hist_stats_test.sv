class hist_stats_test extends hist_base_test;
  `uvm_component_utils(hist_stats_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 8192;
  localparam bit [3:0] CSR_UNDERFLOW    = 4'd6;
  localparam bit [3:0] CSR_OVERFLOW     = 4'd7;
  localparam bit [3:0] CSR_TOTAL_HITS   = 4'd11;
  localparam bit [3:0] CSR_DROPPED_HITS = 4'd12;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  local task automatic configure_default();
    program_histogram(
      .left_bound   (0),
      .bin_width    (16),
      .key_unsigned (1'b1),
      .interval_cfg (HS_TEST_INTERVAL_CFG)
    );
  endtask

  local task automatic check_csr(
    input string    case_id,
    input bit [3:0] address,
    input bit [31:0] expected,
    input string    name
  );
    bit [31:0] val;
    csr_read(address, val);
    if (val !== expected)
      `uvm_error(case_id, $sformatf("%s expected %0d got %0d", name, expected, val))
  endtask

  local task automatic task_b131();
    `uvm_info(get_type_name(), "B131: total_hits increments by 1 per hit", UVM_LOW)
    configure_default();
    for (int i = 0; i < 5; i++)
      send_fill_word(0, make_fill_word(i * 16));
    wait_pipeline_drain(128);
    check_csr("B131", CSR_TOTAL_HITS, 32'd5, "total_hits");
  endtask

  local task automatic task_b132();
    `uvm_info(get_type_name(), "B132: total_hits across all ports", UVM_LOW)
    configure_default();
    for (int p = 0; p < HS_N_PORTS; p++)
      send_fill_word(p, make_fill_word(0));
    wait_pipeline_drain(256);
    check_csr("B132", CSR_TOTAL_HITS, 32'd8, "total_hits");
  endtask

  local task automatic task_b133();
    `uvm_info(get_type_name(), "B133: dropped_hits on FIFO-full — skipped", UVM_LOW)
  endtask

  local task automatic task_b134();
    `uvm_info(get_type_name(), "B134: underflow_cnt for keys below left_bound", UVM_LOW)
    program_histogram(
      .left_bound   (HS_DEF_LEFT_BOUND),
      .bin_width    (HS_DEF_BIN_WIDTH),
      .key_unsigned (1'b1),
      .interval_cfg (HS_TEST_INTERVAL_CFG)
    );
    // Keys below left_bound (-1000): need unsigned keys that map to negative effective values
    // With unsigned extraction, smallest key is 0. Effective = 0 + 0*32 = 0. 0 > -1000, so no underflow.
    // For underflow with default config: need key < -1000. But unsigned extraction can't produce negative.
    // Use signed extraction instead:
    program_histogram(
      .left_bound   (0),
      .bin_width    (16),
      .key_unsigned (1'b1),
      .interval_cfg (HS_TEST_INTERVAL_CFG)
    );
    // With lb=0, keys that are negative would underflow, but unsigned extraction gives non-negative.
    // Simplest approach: use a left_bound > 0 so that key=0 underflows
    program_histogram(
      .left_bound   (100),
      .bin_width    (16),
      .key_unsigned (1'b1),
      .interval_cfg (HS_TEST_INTERVAL_CFG)
    );
    // key=0 < lb=100 -> underflow. key=50 < 100 -> underflow. key=99 < 100 -> underflow.
    send_fill_word(0, make_fill_word(0));
    send_fill_word(0, make_fill_word(50));
    send_fill_word(0, make_fill_word(99));
    wait_pipeline_drain(128);
    check_csr("B134", CSR_UNDERFLOW, 32'd3, "underflow_cnt");
  endtask

  local task automatic task_b135();
    `uvm_info(get_type_name(), "B135: overflow_cnt for keys at/above right_bound", UVM_LOW)
    program_histogram(
      .left_bound   (0),
      .bin_width    (16),
      .key_unsigned (1'b1),
      .interval_cfg (HS_TEST_INTERVAL_CFG)
    );
    // right_bound = 0 + 256*16 = 4096
    send_fill_word(0, make_fill_word(4096));
    send_fill_word(0, make_fill_word(5000));
    wait_pipeline_drain(128);
    check_csr("B135", CSR_OVERFLOW, 32'd2, "overflow_cnt");
  endtask

  local task automatic task_b136();
    bit [31:0] val;
    `uvm_info(get_type_name(), "B136: stats reset on interval_pulse", UVM_LOW)
    configure_default();
    for (int i = 0; i < 4; i++)
      send_fill_word(0, make_fill_word(i * 16));
    wait_pipeline_drain(128);
    wait_bank_swap();
    // Stats should have been reset by interval_pulse
    csr_read(CSR_TOTAL_HITS, val);
    if (val !== 32'd0)
      `uvm_error("B136", $sformatf("total_hits expected 0 after interval, got %0d", val))
  endtask

  local task automatic task_b137();
    bit [31:0] val;
    `uvm_info(get_type_name(), "B137: stats reset on measure_clear", UVM_LOW)
    configure_default();
    for (int i = 0; i < 4; i++)
      send_fill_word(0, make_fill_word(i * 16));
    wait_pipeline_drain(128);
    issue_measure_clear();
    csr_read(CSR_TOTAL_HITS, val);
    if (val !== 32'd0)
      `uvm_error("B137", $sformatf("total_hits expected 0 after measure_clear, got %0d", val))
  endtask

  local task automatic task_b138();
    bit [31:0] uf_val, of_val;
    `uvm_info(get_type_name(), "B138: underflow and overflow mutually exclusive", UVM_LOW)
    program_histogram(
      .left_bound   (100),
      .bin_width    (16),
      .key_unsigned (1'b1),
      .interval_cfg (HS_TEST_INTERVAL_CFG)
    );
    // right_bound = 100 + 256*16 = 4196
    send_fill_word(0, make_fill_word(50));   // underflow (50 < 100)
    send_fill_word(0, make_fill_word(4200)); // overflow (4200 >= 4196)
    wait_pipeline_drain(512);
    csr_read(CSR_UNDERFLOW, uf_val);
    csr_read(CSR_OVERFLOW, of_val);
    if (uf_val !== 32'd1)
      `uvm_error("B138", $sformatf("underflow expected 1 got %0d", uf_val))
    if (of_val !== 32'd1)
      `uvm_error("B138", $sformatf("overflow expected 1 got %0d", of_val))
  endtask

  local task automatic task_b139();
    `uvm_info(get_type_name(), "B139: filtered hits in total_hits — skipped", UVM_LOW)
  endtask

  local task automatic task_b140();
    `uvm_info(get_type_name(), "B140: saturating counter — skipped", UVM_LOW)
  endtask

  local task automatic task_b141();
    `uvm_info(get_type_name(), "B141: stats pipeline latency — skipped", UVM_LOW)
  endtask

  local task automatic task_b142();
    `uvm_info(get_type_name(), "B142: stats vs coalescing overflow — skipped", UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_b131(); issue_measure_clear();
    task_b132(); issue_measure_clear();
    task_b133(); issue_measure_clear();
    task_b134(); issue_measure_clear();
    task_b135(); issue_measure_clear();
    task_b136(); issue_measure_clear();
    task_b137(); issue_measure_clear();
    task_b138(); issue_measure_clear();
    task_b139(); issue_measure_clear();
    task_b140(); issue_measure_clear();
    task_b141(); issue_measure_clear();
    task_b142();

    phase.drop_objection(this);
  endtask
endclass
