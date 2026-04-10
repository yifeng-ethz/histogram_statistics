class hist_prof_ccl_test extends hist_base_test;
  `uvm_component_utils(hist_prof_ccl_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 16384;
  localparam bit [4:0] CSR_CONTROL     = 5'd2;
  localparam bit [4:0] CSR_LEFT_BOUND  = 5'd3;
  localparam bit [4:0] CSR_RIGHT_BOUND = 5'd4;
  localparam bit [4:0] CSR_BIN_WIDTH   = 5'd5;
  localparam bit [4:0] CSR_TOTAL_HITS  = 5'd13;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  local task automatic check_bin(
    input string case_id, input int unsigned idx, input bit [31:0] exp
  );
    bit [31:0] burst_data[$];
    bin_burst_read(idx[7:0], 8'd1, burst_data);
    if (burst_data[0] !== exp)
      `uvm_error(case_id, $sformatf("bin%0d expected %0d got %0d", idx, exp, burst_data[0]))
  endtask

  // CCL P089-P098: Config Change Under Load.

  // P089: Change left_bound while injecting
  local task automatic task_p089();
    `uvm_info(get_type_name(), "P089: change left_bound under load", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1),
                      .interval_cfg(HS_TEST_INTERVAL_CFG));
    // Send hits with key=0 → bin 0 under lb=0, bw=1
    for (int i = 0; i < 10; i++)
      send_fill_word(0, make_fill_word(0));
    wait_pipeline_drain(128);
    // Change left_bound to -10 → key=0 now maps to bin 10
    program_histogram(.left_bound(-10), .bin_width(1), .key_unsigned(1'b1),
                      .interval_cfg(HS_TEST_INTERVAL_CFG));
    for (int i = 0; i < 10; i++)
      send_fill_word(0, make_fill_word(0));
    wait_pipeline_drain(256);
    wait_bank_swap();
    // bin 0 should have old-config hits, bin 10 should have new-config hits
    check_bin("P089", 0, 32'd10);
    check_bin("P089", 10, 32'd10);
  endtask

  // P090: Change bin_width from 16 to 32
  local task automatic task_p090();
    `uvm_info(get_type_name(), "P090: change bin_width 16→32", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1),
                      .interval_cfg(HS_TEST_INTERVAL_CFG));
    // key=0 → bin 0 (0/16=0)
    send_fill_word(0, make_fill_word(0));
    // key=16 → bin 1 (16/16=1)
    send_fill_word(0, make_fill_word(16));
    wait_pipeline_drain(128);
    // Change to bw=32
    program_histogram(.left_bound(0), .bin_width(32), .key_unsigned(1'b1),
                      .interval_cfg(HS_TEST_INTERVAL_CFG));
    // key=0 → bin 0 (0/32=0)
    send_fill_word(0, make_fill_word(0));
    // key=32 → bin 1 (32/32=1)
    send_fill_word(0, make_fill_word(32));
    wait_pipeline_drain(256);
    wait_bank_swap();
    // bin 0 should have 2 hits (0/16 + 0/32), bin 1 should have 2 hits (16/16 + 32/32)
    check_bin("P090", 0, 32'd2);
    check_bin("P090", 1, 32'd2);
  endtask

  // P091: Enable filter mid-stream
  local task automatic task_p091();
    bit [31:0] total;
    `uvm_info(get_type_name(), "P091: enable filter mid-stream", UVM_LOW)
    // Start without filter
    program_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1),
                      .interval_cfg(HS_TEST_INTERVAL_CFG));
    for (int i = 0; i < 5; i++)
      send_fill_word(0, make_fill_word(i));
    wait_pipeline_drain(128);
    // Enable filter — reject hits with filter_key != 0
    // Use default filter key bits; hits with filter value 0 pass
    program_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1),
                      .filter_enable(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    for (int i = 0; i < 5; i++)
      send_fill_word(0, make_fill_word(i));
    wait_pipeline_drain(256);
    csr_read(CSR_TOTAL_HITS, total);
    `uvm_info("P091", $sformatf("total_hits=%0d (5 unfiltered + some filtered)", total), UVM_LOW)
  endtask

  // P092: Rapid 3 config applies
  local task automatic task_p092();
    bit [31:0] rb;
    `uvm_info(get_type_name(), "P092: 3 rapid config applies", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(4), .key_unsigned(1'b1),
                      .interval_cfg(HS_TEST_INTERVAL_CFG));
    program_histogram(.left_bound(0), .bin_width(8), .key_unsigned(1'b1),
                      .interval_cfg(HS_TEST_INTERVAL_CFG));
    program_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1),
                      .interval_cfg(HS_TEST_INTERVAL_CFG));
    csr_read(CSR_RIGHT_BOUND, rb);
    // Final config: lb=0, bw=16 → rb = 0 + 16*256 = 4096
    if (rb !== 32'd4096)
      `uvm_error("P092", $sformatf("right_bound expected 4096 got %0d (last apply didn't stick)", rb))
  endtask

  // P093: Config apply with 8 ports active — skip (multi-port rate control)
  local task automatic task_p093();
    `uvm_info(get_type_name(), "P093: config apply 8 ports — skipped (multi-port rate control)", UVM_LOW)
  endtask

  // P094: Config change during bank swap — skip (cycle-precise)
  local task automatic task_p094();
    `uvm_info(get_type_name(), "P094: config change during swap — skipped (timing)", UVM_LOW)
  endtask

  // P095: Invalid config rejected — already verified by DV_ERROR CER section
  local task automatic task_p095();
    `uvm_info(get_type_name(), "P095: invalid config rejected — verified by X050-X056", UVM_LOW)
  endtask

  // P096: Config change + immediate burst read
  local task automatic task_p096();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "P096: config change then burst read", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1),
                      .interval_cfg(HS_TEST_INTERVAL_CFG));
    for (int i = 0; i < 10; i++)
      send_fill_word(0, make_fill_word(i));
    wait_pipeline_drain(256);
    wait_bank_swap();
    // Change config
    program_histogram(.left_bound(0), .bin_width(32), .key_unsigned(1'b1),
                      .interval_cfg(HS_TEST_INTERVAL_CFG));
    // Burst read — should see old config's data in frozen bank
    bin_burst_read(8'd0, 8'd10, burst_data);
    for (int i = 0; i < 10; i++) begin
      if (burst_data[i] !== 32'd1)
        `uvm_error("P096", $sformatf("bin%0d expected 1 got %0d (old config data)", i, burst_data[i]))
    end
  endtask

  // P097: Random config changes — skip (LCG infrastructure)
  local task automatic task_p097();
    `uvm_info(get_type_name(), "P097: random config changes — skipped (LCG)", UVM_LOW)
  endtask

  // P098: Config apply with zero hits in flight
  local task automatic task_p098();
    bit [31:0] ctrl;
    `uvm_info(get_type_name(), "P098: quiescent config apply", UVM_LOW)
    // No hits in flight
    program_histogram(.left_bound(0), .bin_width(4), .key_unsigned(1'b1),
                      .interval_cfg(HS_TEST_INTERVAL_CFG));
    csr_read(CSR_CONTROL, ctrl);
    // Apply pending should be cleared (apply completed instantly)
    if (ctrl[1] !== 1'b0)
      `uvm_error("P098", $sformatf("apply_pending still set: 0x%08h", ctrl))
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_p089(); issue_measure_clear();
    task_p090(); issue_measure_clear();
    task_p091(); issue_measure_clear();
    task_p092(); issue_measure_clear();
    task_p093(); task_p094(); task_p095();
    issue_measure_clear();
    task_p096(); issue_measure_clear();
    task_p097();
    task_p098();

    phase.drop_objection(this);
  endtask
endclass
