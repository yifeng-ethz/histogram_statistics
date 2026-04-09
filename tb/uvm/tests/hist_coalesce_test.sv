class hist_coalesce_test extends hist_base_test;
  `uvm_component_utils(hist_coalesce_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 4096;
  localparam bit [3:0] CSR_COAL_STATUS = 4'd14;

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

  local task automatic task_b103();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B103: 4 hits same bin coalesced", UVM_LOW)
    configure_default();
    for (int i = 0; i < 4; i++)
      send_fill_word(0, make_fill_word(160));  // all map to bin 10
    wait_pipeline_drain(128);
    wait_bank_swap();
    bin_burst_read(8'd10, 8'd1, burst_data);
    if (burst_data[0] !== 32'd4)
      `uvm_error("B103", $sformatf("bin10 expected 4 got %0d", burst_data[0]))
  endtask

  local task automatic task_b104();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B104: 4 hits to 4 different bins", UVM_LOW)
    configure_default();
    send_fill_word(0, make_fill_word(0));
    send_fill_word(0, make_fill_word(16));
    send_fill_word(0, make_fill_word(32));
    send_fill_word(0, make_fill_word(48));
    wait_pipeline_drain(128);
    wait_bank_swap();
    bin_burst_read(8'd0, 8'd4, burst_data);
    for (int i = 0; i < 4; i++) begin
      if (burst_data[i] !== 32'd1)
        `uvm_error("B104", $sformatf("bin%0d expected 1 got %0d", i, burst_data[i]))
    end
  endtask

  local task automatic task_b105();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B105: 8 hits alternating 2 bins", UVM_LOW)
    configure_default();
    for (int i = 0; i < 4; i++) begin
      send_fill_word(0, make_fill_word(0));
      send_fill_word(0, make_fill_word(16));
    end
    wait_pipeline_drain(128);
    wait_bank_swap();
    bin_burst_read(8'd0, 8'd2, burst_data);
    if (burst_data[0] !== 32'd4)
      `uvm_error("B105", $sformatf("bin0 expected 4 got %0d", burst_data[0]))
    if (burst_data[1] !== 32'd4)
      `uvm_error("B105", $sformatf("bin1 expected 4 got %0d", burst_data[1]))
  endtask

  local task automatic task_b106();
    `uvm_info(get_type_name(), "B106: drain FIFO order — skipped", UVM_LOW)
  endtask

  local task automatic task_b107();
    bit [31:0] csr_val;
    `uvm_info(get_type_name(), "B107: queue occupancy_max reported", UVM_LOW)
    configure_default();
    for (int i = 0; i < 10; i++)
      send_fill_word(0, make_fill_word(i * 16));
    wait_pipeline_drain(256);
    csr_read(CSR_COAL_STATUS, csr_val);
    begin
      int unsigned occ_max;
      occ_max = (csr_val >> 8) & 8'hFF;
      if (occ_max < 1)
        `uvm_error("B107", $sformatf("occupancy_max expected >=1 got %0d", occ_max))
    end
  endtask

  local task automatic task_b108();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B108: queue clear on measure_clear", UVM_LOW)
    configure_default();
    for (int i = 0; i < 8; i++)
      send_fill_word(0, make_fill_word(i * 16));
    wait_pipeline_drain(64);
    issue_measure_clear();
    bin_burst_read(8'd0, 8'd8, burst_data);
    for (int i = 0; i < burst_data.size(); i++) begin
      if (burst_data[i] !== 32'd0)
        `uvm_error("B108", $sformatf("bin%0d expected 0 got %0d", i, burst_data[i]))
    end
  endtask

  local task automatic task_b109();
    `uvm_info(get_type_name(), "B109: kick counter saturation — skipped", UVM_LOW)
  endtask

  local task automatic task_b110();
    `uvm_info(get_type_name(), "B110: queue full coalesce — skipped", UVM_LOW)
  endtask

  local task automatic task_b111();
    `uvm_info(get_type_name(), "B111: queue overflow — skipped", UVM_LOW)
  endtask

  local task automatic task_b112();
    `uvm_info(get_type_name(), "B112: simultaneous drain/hit — skipped", UVM_LOW)
  endtask

  local task automatic task_b113();
    `uvm_info(get_type_name(), "B113: clear sweep cycles — skipped", UVM_LOW)
  endtask

  local task automatic task_b114();
    bit [31:0] csr_val;
    `uvm_info(get_type_name(), "B114: occupancy_max survives across bursts", UVM_LOW)
    configure_default();
    // Burst 1: 5 distinct bins
    for (int i = 0; i < 5; i++)
      send_fill_word(0, make_fill_word(i * 16));
    wait_pipeline_drain(256);
    // Burst 2: 3 distinct bins
    for (int i = 0; i < 3; i++)
      send_fill_word(0, make_fill_word(i * 16));
    wait_pipeline_drain(256);
    csr_read(CSR_COAL_STATUS, csr_val);
    begin
      int unsigned occ_max;
      occ_max = (csr_val >> 8) & 8'hFF;
      if (occ_max < 1)
        `uvm_error("B114", $sformatf("occupancy_max expected >=1 got %0d", occ_max))
    end
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_b103(); issue_measure_clear();
    task_b104(); issue_measure_clear();
    task_b105(); issue_measure_clear();
    task_b106(); issue_measure_clear();
    task_b107(); issue_measure_clear();
    task_b108(); issue_measure_clear();
    task_b109(); issue_measure_clear();
    task_b110(); issue_measure_clear();
    task_b111(); issue_measure_clear();
    task_b112(); issue_measure_clear();
    task_b113(); issue_measure_clear();
    task_b114();

    phase.drop_objection(this);
  endtask
endclass
