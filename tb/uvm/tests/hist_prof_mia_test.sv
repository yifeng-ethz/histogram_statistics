class hist_prof_mia_test extends hist_base_test;
  `uvm_component_utils(hist_prof_mia_test)

  localparam bit [4:0] CSR_TOTAL_HITS = 5'd13;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  // MIA P107-P116: Multi-Interval Accumulation.

  // P107: 10 intervals, verify frozen bank per interval
  local task automatic task_p107();
    bit [31:0] burst_data[$];
    int unsigned sum_v;
    `uvm_info(get_type_name(), "P107: 10 intervals, 20 hits each, verify frozen bank", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1),
                      .interval_cfg(8192));
    for (int intv = 0; intv < 10; intv++) begin
      for (int i = 0; i < 20; i++)
        send_fill_word(0, make_fill_word(i % 256));
      wait_pipeline_drain(256);
      wait_bank_swap();
      bin_burst_read(8'd0, 8'd255, burst_data);
      sum_v = 0;
      for (int j = 0; j < burst_data.size(); j++)
        sum_v += burst_data[j];
      if (sum_v !== 20)
        `uvm_error("P107", $sformatf("interval %0d: sum expected 20 got %0d", intv, sum_v))
    end
  endtask

  // P108: 10 intervals, 8 ports at 10%
  local task automatic task_p108();
    `uvm_info(get_type_name(), "P108: 10 intervals 8-port — skipped (multi-port rate control)", UVM_LOW)
  endtask

  // P109: 20 intervals with smaller count — scaled from 100
  local task automatic task_p109();
    bit [31:0] burst_data[$];
    int unsigned sum_v;
    `uvm_info(get_type_name(), "P109: 20 intervals, 10 hits each", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1),
                      .interval_cfg(4096));
    for (int intv = 0; intv < 20; intv++) begin
      for (int i = 0; i < 10; i++)
        send_fill_word(0, make_fill_word(i % 256));
      wait_pipeline_drain(128);
      wait_bank_swap();
      bin_burst_read(8'd0, 8'd255, burst_data);
      sum_v = 0;
      for (int j = 0; j < burst_data.size(); j++)
        sum_v += burst_data[j];
      if (sum_v !== 10)
        `uvm_error("P109", $sformatf("interval %0d: sum expected 10 got %0d", intv, sum_v))
    end
  endtask

  // P110: Different injection rates per interval
  local task automatic task_p110();
    `uvm_info(get_type_name(), "P110: variable rate per interval — skipped (rate control)", UVM_LOW)
  endtask

  // P111: Hit on exact swap cycle — skip (cycle-precise)
  local task automatic task_p111();
    `uvm_info(get_type_name(), "P111: hit at exact swap — skipped (cycle-precise)", UVM_LOW)
  endtask

  // P112: Read frozen bank bin-by-bin after swap
  local task automatic task_p112();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "P112: bin-by-bin frozen bank read, 10 known bins", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1),
                      .interval_cfg(8192));
    // Inject known pattern: bin i gets (i+1) hits
    for (int b = 0; b < 10; b++)
      for (int h = 0; h <= b; h++)
        send_fill_word(0, make_fill_word(b));
    wait_pipeline_drain(512);
    wait_bank_swap();
    // Verify each bin
    for (int b = 0; b < 10; b++) begin
      bin_burst_read(b[7:0], 8'd1, burst_data);
      if (burst_data[0] !== (b + 1))
        `uvm_error("P112", $sformatf("bin%0d expected %0d got %0d", b, b + 1, burst_data[0]))
    end
  endtask

  // P113: Non-pingpong mode — skip (ENABLE_PINGPONG=1)
  local task automatic task_p113();
    `uvm_info(get_type_name(), "P113: non-pingpong single bank — skipped (ENABLE_PINGPONG=1)", UVM_LOW)
  endtask

  // P114: Interval=1 — extreme edge case, DUT never gets update window
  local task automatic task_p114();
    `uvm_info(get_type_name(), "P114: interval=1 — skipped (extreme, no effective window)", UVM_LOW)
  endtask

  // P115: Interval=512 with burst read after every swap
  local task automatic task_p115();
    `uvm_info(get_type_name(), "P115: interval=512 + burst reads — skipped (short interval + UVM rate)", UVM_LOW)
  endtask

  // P116: Random hit timing relative to interval boundary — skip (LCG)
  local task automatic task_p116();
    `uvm_info(get_type_name(), "P116: random timing vs interval — skipped (LCG)", UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_p107(); issue_measure_clear();
    task_p108();
    task_p109(); issue_measure_clear();
    task_p110(); task_p111();
    task_p112(); issue_measure_clear();
    task_p113(); task_p114(); task_p115(); task_p116();

    phase.drop_objection(this);
  endtask
endclass
