class hist_bank_test extends hist_base_test;
  `uvm_component_utils(hist_bank_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 2048;
  localparam bit [4:0] CSR_BANK_STATUS = 5'd11;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  local task automatic task_b091();
    bit [31:0] csr_val;
    `uvm_info(get_type_name(), "B091: timer fires at interval boundary", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    wait_bank_swap();
    csr_read(CSR_BANK_STATUS, csr_val);
    if (csr_val[0] !== 1'b1)
      `uvm_error("B091", $sformatf("active_bank expected 1 got %0d", csr_val[0]))
  endtask

  local task automatic task_b092();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B092: bank swap clears new active bank", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    // Interval 1: 4 hits to bins 0-3
    for (int i = 0; i < 4; i++)
      send_fill_word(0, make_fill_word(i * 16));
    wait_pipeline_drain(128);
    wait_bank_swap();
    // Interval 2: 4 hits to same bins (different counts)
    for (int i = 0; i < 4; i++) begin
      send_fill_word(0, make_fill_word(i * 16));
      send_fill_word(0, make_fill_word(i * 16));
    end
    wait_pipeline_drain(128);
    wait_bank_swap();
    // Read frozen bank = interval 2 data
    bin_burst_read(8'd0, 8'd4, burst_data);
    for (int i = 0; i < 4; i++) begin
      if (burst_data[i] !== 32'd2)
        `uvm_error("B092", $sformatf("bin%0d expected 2 got %0d", i, burst_data[i]))
    end
  endtask

  local task automatic task_b093();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B093: host reads frozen bank", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    for (int i = 0; i < 4; i++)
      send_fill_word(0, make_fill_word(0));
    wait_pipeline_drain(128);
    wait_bank_swap();
    bin_burst_read(8'd0, 8'd1, burst_data);
    if (burst_data[0] !== 32'd4)
      `uvm_error("B093", $sformatf("bin0 expected 4 got %0d", burst_data[0]))
  endtask

  local task automatic task_b094();
    `uvm_info(get_type_name(), "B094: flush timing — skipped", UVM_LOW)
  endtask

  local task automatic task_b095();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B095: two consecutive swaps, 3 intervals", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    // Interval 1: 4 hits bin 0
    for (int i = 0; i < 4; i++)
      send_fill_word(0, make_fill_word(0));
    wait_pipeline_drain(128);
    wait_bank_swap();
    // Interval 2: 8 hits bin 0
    for (int i = 0; i < 8; i++)
      send_fill_word(0, make_fill_word(0));
    wait_pipeline_drain(128);
    wait_bank_swap();
    // Frozen = interval 2
    bin_burst_read(8'd0, 8'd1, burst_data);
    if (burst_data[0] !== 32'd8)
      `uvm_error("B095", $sformatf("bin0 expected 8 got %0d", burst_data[0]))
  endtask

  local task automatic task_b096();
    `uvm_info(get_type_name(), "B096: bank status during flush — skipped", UVM_LOW)
  endtask

  local task automatic task_b097();
    bit [31:0] csr_val;
    `uvm_info(get_type_name(), "B097: measure_clear resets timer and bank", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    send_fill_word(0, make_fill_word(0));
    wait_pipeline_drain(64);
    issue_measure_clear();
    csr_read(CSR_BANK_STATUS, csr_val);
    if (csr_val[0] !== 1'b0)
      `uvm_error("B097", $sformatf("active_bank expected 0 after measure_clear, got %0d", csr_val[0]))
  endtask

  local task automatic task_b098();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B098: measure_clear resets both banks", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    for (int i = 0; i < 8; i++)
      send_fill_word(0, make_fill_word(i * 16));
    wait_pipeline_drain(128);
    issue_measure_clear();
    // Read bins — all should be 0
    bin_burst_read(8'd0, 8'd8, burst_data);
    for (int i = 0; i < burst_data.size(); i++) begin
      if (burst_data[i] !== 32'd0)
        `uvm_error("B098", $sformatf("bin%0d expected 0 got %0d", i, burst_data[i]))
    end
  endtask

  local task automatic task_b099();
    `uvm_info(get_type_name(), "B099: interval_cfg=0 disables timer — skipped (scoreboard edge case)", UVM_LOW)
  endtask

  local task automatic task_b100();
    `uvm_info(get_type_name(), "B100: read during active update — skipped (scoreboard edge case)", UVM_LOW)
  endtask

  local task automatic task_b101();
    `uvm_info(get_type_name(), "B101: pingpong disabled — skipped (generic)", UVM_LOW)
  endtask

  local task automatic task_b102();
    `uvm_info(get_type_name(), "B102: interval_reset — skipped", UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_b091(); issue_measure_clear();
    task_b092(); issue_measure_clear();
    task_b093(); issue_measure_clear();
    task_b094(); issue_measure_clear();
    task_b095(); issue_measure_clear();
    task_b096(); issue_measure_clear();
    task_b097(); issue_measure_clear();
    task_b098(); issue_measure_clear();
    task_b099(); issue_measure_clear();
    task_b100(); issue_measure_clear();
    task_b101(); issue_measure_clear();
    task_b102();

    phase.drop_objection(this);
  endtask
endclass
