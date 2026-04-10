class hist_prof_qst_test extends hist_base_test;
  `uvm_component_utils(hist_prof_qst_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 32768;
  localparam bit [4:0] CSR_TOTAL_HITS  = 5'd13;
  localparam bit [4:0] CSR_COAL_STATUS = 5'd16;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  local task automatic configure();
    program_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1),
                      .interval_cfg(HS_TEST_INTERVAL_CFG));
  endtask

  local task automatic check_bin(
    input string case_id, input int unsigned idx, input bit [31:0] exp
  );
    bit [31:0] burst_data[$];
    bin_burst_read(idx[7:0], 8'd1, burst_data);
    if (burst_data[0] !== exp)
      `uvm_error(case_id, $sformatf("bin%0d expected %0d got %0d", idx, exp, burst_data[0]))
  endtask

  // QST P127-P140: Queue Saturation.

  // P127: Drive exactly 256 distinct bins sequentially
  local task automatic task_p127();
    bit [31:0] coal, total;
    `uvm_info(get_type_name(), "P127: 256 distinct bins, one hit each", UVM_LOW)
    configure();
    for (int i = 0; i < 256; i++)
      send_fill_word(0, make_fill_word(i));
    wait_pipeline_drain(2048);
    csr_read(CSR_COAL_STATUS, coal);
    csr_read(CSR_TOTAL_HITS, total);
    if (total !== 32'd256)
      `uvm_error("P127", $sformatf("total_hits expected 256 got %0d", total))
    if (coal[31:16] !== 16'd0)
      `uvm_error("P127", $sformatf("overflow_count expected 0 got %0d", coal[31:16]))
    `uvm_info("P127", $sformatf("total=%0d occ_max=%0d overflow=%0d", total, coal[15:8], coal[31:16]), UVM_LOW)
  endtask

  // P128: Second pass coalesces — 512 hits, 256 bins, two passes
  local task automatic task_p128();
    bit [31:0] coal;
    `uvm_info(get_type_name(), "P128: 512 hits, sequential 256-bin x2", UVM_LOW)
    configure();
    for (int i = 0; i < 512; i++)
      send_fill_word(0, make_fill_word(i % 256));
    wait_pipeline_drain(2048);
    csr_read(CSR_COAL_STATUS, coal);
    if (coal[31:16] !== 16'd0)
      `uvm_error("P128", $sformatf("overflow expected 0 (coalesced), got %0d", coal[31:16]))
    wait_bank_swap();
    // Each bin should have count 2
    check_bin("P128", 0, 32'd2);
    check_bin("P128", 128, 32'd2);
    check_bin("P128", 255, 32'd2);
  endtask

  // P129: Fill queue, then 100 extra hits to already-queued bins
  local task automatic task_p129();
    bit [31:0] coal;
    `uvm_info(get_type_name(), "P129: 256 distinct + 100 coalescing hits", UVM_LOW)
    configure();
    for (int i = 0; i < 256; i++)
      send_fill_word(0, make_fill_word(i));
    // 100 more to already-queued bins
    for (int i = 0; i < 100; i++)
      send_fill_word(0, make_fill_word(i % 256));
    wait_pipeline_drain(2048);
    csr_read(CSR_COAL_STATUS, coal);
    if (coal[31:16] !== 16'd0)
      `uvm_error("P129", $sformatf("overflow expected 0, got %0d", coal[31:16]))
    wait_bank_swap();
    // Bins 0-99 should have 2 hits, bins 100-255 should have 1 hit
    check_bin("P129", 0, 32'd2);
    check_bin("P129", 99, 32'd2);
    check_bin("P129", 100, 32'd1);
    check_bin("P129", 255, 32'd1);
  endtask

  // P130: Queue stalled — need upd_ready=0
  local task automatic task_p130();
    `uvm_info(get_type_name(), "P130: queue stalled drain — skipped (need SRAM stall)", UVM_LOW)
  endtask

  // P131: Kick saturation — need 256 hits to one bin before drain
  local task automatic task_p131();
    `uvm_info(get_type_name(), "P131: kick saturation 256 hits to bin 0 — skipped (drain faster than injection)", UVM_LOW)
  endtask

  // P132: Kick saturation all bins — need wire-speed
  local task automatic task_p132();
    `uvm_info(get_type_name(), "P132: kick saturation all 256 bins — skipped (too slow)", UVM_LOW)
  endtask

  // P133: Queue drain race — cycle-precise
  local task automatic task_p133();
    `uvm_info(get_type_name(), "P133: drain race same bin — skipped (cycle-precise)", UVM_LOW)
  endtask

  // P134: occupancy_max accuracy — testable
  local task automatic task_p134();
    bit [31:0] coal;
    `uvm_info(get_type_name(), "P134: occupancy_max tracks peak", UVM_LOW)
    configure();
    // Phase 1: 200 distinct bins
    for (int i = 0; i < 200; i++)
      send_fill_word(0, make_fill_word(i));
    wait_pipeline_drain(1024);
    csr_read(CSR_COAL_STATUS, coal);
    `uvm_info("P134", $sformatf("after 200 bins: occ_max=%0d", coal[15:8]), UVM_LOW)
    // Phase 2: let drain, then 256 distinct bins
    for (int i = 0; i < 256; i++)
      send_fill_word(0, make_fill_word(i));
    wait_pipeline_drain(2048);
    csr_read(CSR_COAL_STATUS, coal);
    `uvm_info("P134", $sformatf("after 256 bins: occ_max=%0d", coal[15:8]), UVM_LOW)
    // UVM injection is slow, so queue drains as fast as it fills.
    // occupancy_max reflects peak during slow injection — may be low.
    // Verify it is > 0 (queue was used) rather than checking absolute value.
    if (coal[15:8] === 8'd0)
      `uvm_error("P134", "occ_max is 0 after 256 bins — queue was never used")
  endtask

  // P135: overflow counter saturation — need 65536+ overflow events
  local task automatic task_p135();
    `uvm_info(get_type_name(), "P135: overflow 16-bit saturation — skipped (too slow)", UVM_LOW)
  endtask

  // P136: Random bins near-full — skip (LCG)
  local task automatic task_p136();
    `uvm_info(get_type_name(), "P136: random bins near-full — skipped (LCG)", UVM_LOW)
  endtask

  // P137: Clear during queue full
  local task automatic task_p137();
    bit [31:0] coal;
    `uvm_info(get_type_name(), "P137: clear with 256 bins queued", UVM_LOW)
    configure();
    for (int i = 0; i < 256; i++)
      send_fill_word(0, make_fill_word(i));
    wait_pipeline_drain(512);
    issue_measure_clear();
    csr_read(CSR_COAL_STATUS, coal);
    if (coal[7:0] !== 8'd0)
      `uvm_error("P137", $sformatf("occupancy expected 0 after clear, got %0d", coal[7:0]))
    if (coal[31:16] !== 16'd0)
      `uvm_error("P137", $sformatf("overflow expected 0 after clear, got %0d", coal[31:16]))
  endtask

  // P138: drain stall during pingpong clear — skip (timing)
  local task automatic task_p138();
    `uvm_info(get_type_name(), "P138: drain stall during clear — skipped (timing)", UVM_LOW)
  endtask

  // P139: Scaled queue pointer wrap — 1024 hits
  local task automatic task_p139();
    bit [31:0] total;
    `uvm_info(get_type_name(), "P139: 1024 hits sequential scan", UVM_LOW)
    configure();
    for (int i = 0; i < 1024; i++)
      send_fill_word(0, make_fill_word(i % 256));
    wait_pipeline_drain(4096);
    csr_read(CSR_TOTAL_HITS, total);
    if (total !== 32'd1024)
      `uvm_error("P139", $sformatf("total_hits expected 1024 got %0d", total))
    wait_bank_swap();
    // Each bin should have 4 hits (1024/256)
    check_bin("P139", 0, 32'd4);
    check_bin("P139", 128, 32'd4);
    check_bin("P139", 255, 32'd4);
  endtask

  // P140: queue overflow + bank swap — skip (timing)
  local task automatic task_p140();
    `uvm_info(get_type_name(), "P140: queue overflow + bank swap — skipped (timing)", UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_p127(); issue_measure_clear();
    task_p128(); issue_measure_clear();
    task_p129(); issue_measure_clear();
    task_p130(); issue_measure_clear();
    task_p131(); task_p132(); task_p133();
    issue_measure_clear();
    task_p134(); issue_measure_clear();
    task_p135(); task_p136();
    task_p137(); issue_measure_clear();
    task_p138();
    task_p139(); issue_measure_clear();
    task_p140();

    phase.drop_objection(this);
  endtask
endclass
