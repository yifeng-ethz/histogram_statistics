class hist_snoop_test extends hist_base_test;
  `uvm_component_utils(hist_snoop_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 2048;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  local task automatic task_b115();
    `uvm_info(get_type_name(), "B115: snoop mirrors data — skipped (needs snoop capture)", UVM_LOW)
  endtask

  local task automatic task_b116();
    `uvm_info(get_type_name(), "B116: snoop mirrors channel — skipped", UVM_LOW)
  endtask

  local task automatic task_b117();
    `uvm_info(get_type_name(), "B117: snoop mirrors SOP/EOP — skipped", UVM_LOW)
  endtask

  local task automatic task_b118();
    `uvm_info(get_type_name(), "B118: SOP/EOP forced 0 — skipped (generic)", UVM_LOW)
  endtask

  local task automatic task_b119();
    `uvm_info(get_type_name(), "B119: snoop backpressure — skipped", UVM_LOW)
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
    `uvm_info(get_type_name(), "B121: snoop valid follows input — skipped", UVM_LOW)
  endtask

  local task automatic task_b122();
    `uvm_info(get_type_name(), "B122: snoop with filter — skipped", UVM_LOW)
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
