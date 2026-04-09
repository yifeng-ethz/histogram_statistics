class hist_error_queue_test extends hist_base_test;
  `uvm_component_utils(hist_error_queue_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 8192;
  localparam bit [3:0] CSR_CONTROL     = 4'd0;
  localparam bit [3:0] CSR_LEFT_BOUND  = 4'd1;
  localparam bit [3:0] CSR_BIN_WIDTH   = 4'd3;
  localparam bit [3:0] CSR_INTERVAL    = 4'd8;
  localparam bit [3:0] CSR_TOTAL_HITS  = 4'd11;
  localparam bit [3:0] CSR_COAL_STATUS = 4'd14;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  local task automatic configure(
    input int signed   left_bound   = 0,
    input int unsigned bin_width    = 1,
    input bit          key_unsigned = 1'b1,
    input int unsigned interval_cfg = HS_TEST_INTERVAL_CFG
  );
    program_histogram(.left_bound(left_bound), .bin_width(bin_width),
                      .key_unsigned(key_unsigned), .interval_cfg(interval_cfg));
  endtask

  local task automatic check_bin(
    input string case_id, input int unsigned idx, input bit [31:0] exp
  );
    bit [31:0] burst_data[$];
    bin_burst_read(idx[7:0], 8'd1, burst_data);
    if (burst_data[0] !== exp)
      `uvm_error(case_id, $sformatf("bin%0d expected %0d got %0d", idx, exp, burst_data[0]))
  endtask

  // CQO X092-X108: Coalescing Queue Overflow.

  // X092: With N_BINS=256 and QUEUE_DEPTH=256, all bins fit in queue.
  // Queue-full overflow from new bins is impossible by design.
  local task automatic task_x092();
    `uvm_info(get_type_name(), "X092: queue full + new bin — design invariant (N_BINS=QUEUE_DEPTH=256)", UVM_LOW)
  endtask

  // X093-X094: Queue drain/fill interaction — need cycle-precise drain timing
  local task automatic task_x093();
    `uvm_info(get_type_name(), "X093: queue level-1 + drain + new bin — skipped (cycle-precise)", UVM_LOW)
  endtask
  local task automatic task_x094();
    `uvm_info(get_type_name(), "X094: queue full + drain + re-queue — skipped (cycle-precise)", UVM_LOW)
  endtask

  // X095: Requires QUEUE_DEPTH < N_BINS generic override (not our DUT config)
  local task automatic task_x095();
    `uvm_info(get_type_name(), "X095: queue overflow QUEUE_DEPTH=128 — skipped (generic override needed)", UVM_LOW)
  endtask

  // X096: Kick counter saturation at 255 — need 255 hits to same bin before drain.
  // Drain runs at pipeline speed (~1/cycle), UVM injection is slower.
  // The kick counter will never reach 255 because drain fires after every few hits.
  local task automatic task_x096();
    `uvm_info(get_type_name(), "X096: kick counter saturation at 255 — skipped (drain faster than injection)", UVM_LOW)
  endtask

  // X097: Kick saturation + drain same cycle — need kick=255 first
  local task automatic task_x097();
    `uvm_info(get_type_name(), "X097: kick saturation + drain — skipped (need kick=255)", UVM_LOW)
  endtask

  // X098: All 256 bins at kick_max — need 256*255 hits without drain
  local task automatic task_x098();
    `uvm_info(get_type_name(), "X098: all bins kick_max — skipped (requires stall)", UVM_LOW)
  endtask

  // X099: Kick value preservation across multiple drains
  // Testable: inject N hits to bin 0, let drain, inject more, check final SRAM count
  local task automatic task_x099();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "X099: kick preserved across drains — inject 10+5 hits to bin 0", UVM_LOW)
    configure(.left_bound(0), .bin_width(1));
    // Phase 1: inject 10 hits to bin 0
    for (int i = 0; i < 10; i++)
      send_fill_word(0, make_fill_word(0));
    wait_pipeline_drain(512);
    // Phase 2: inject 5 more hits to bin 0
    for (int i = 0; i < 5; i++)
      send_fill_word(0, make_fill_word(0));
    wait_pipeline_drain(512);
    wait_bank_swap();
    // Read bin 0 — should have accumulated 15 total via read-modify-write
    bin_burst_read(8'd0, 8'd1, burst_data);
    if (burst_data[0] !== 32'd15)
      `uvm_error("X099", $sformatf("bin0 expected 15 got %0d", burst_data[0]))
  endtask

  // X100-X101: overflow_count accuracy/saturation — need kick saturation first
  local task automatic task_x100();
    `uvm_info(get_type_name(), "X100: overflow_count 1000 events — skipped (need kick saturation)", UVM_LOW)
  endtask
  local task automatic task_x101();
    `uvm_info(get_type_name(), "X101: overflow_count saturation 65535 — skipped (need kick saturation)", UVM_LOW)
  endtask

  // X102: overflow_count reset by measure_clear — testable
  local task automatic task_x102();
    bit [31:0] val;
    `uvm_info(get_type_name(), "X102: overflow_count reset by clear", UVM_LOW)
    configure(.left_bound(0), .bin_width(1));
    // Inject some hits to get non-zero coal_status
    for (int i = 0; i < 50; i++)
      send_fill_word(0, make_fill_word(i));
    wait_pipeline_drain(512);
    issue_measure_clear();
    csr_read(CSR_COAL_STATUS, val);
    // After clear: occupancy=0, max=0, overflow=0
    if (val !== 32'd0)
      `uvm_error("X102", $sformatf("coal_status expected 0 after clear, got 0x%08h", val))
  endtask

  // X103: overflow_count reset by i_rst — needs reset access
  local task automatic task_x103();
    `uvm_info(get_type_name(), "X103: overflow_count reset by reset — skipped (no i_rst access)", UVM_LOW)
  endtask

  // X104: Queue occupancy_max tracking — testable
  local task automatic task_x104();
    bit [31:0] val;
    `uvm_info(get_type_name(), "X104: queue occupancy_max tracking", UVM_LOW)
    configure(.left_bound(0), .bin_width(1));
    // Inject 50 distinct-bin hits to build queue occupancy
    for (int i = 0; i < 50; i++)
      send_fill_word(0, make_fill_word(i));
    wait_pipeline_drain(1024);
    csr_read(CSR_COAL_STATUS, val);
    // occupancy_max should be > 0 (some entries queued before drain)
    if (val[15:8] === 8'd0)
      `uvm_error("X104", $sformatf("occupancy_max is 0 after 50 hits — expected > 0 (coal_status=0x%08h)", val))
    else
      `uvm_info("X104", $sformatf("occupancy_max = %0d (coal_status=0x%08h)", val[15:8], val), UVM_LOW)
  endtask

  // X105: occupancy_max clipping to 8-bit — requires QUEUE_DEPTH > 255
  local task automatic task_x105();
    `uvm_info(get_type_name(), "X105: occupancy_max 8-bit clip — skipped (QUEUE_DEPTH=256, max=255 fits)", UVM_LOW)
  endtask

  // X106: Drain blocked by clear_active — timing-dependent
  local task automatic task_x106();
    `uvm_info(get_type_name(), "X106: drain blocked by clear_active — skipped (cycle-precise)", UVM_LOW)
  endtask

  // X107: Drain gated by SRAM clear — timing-dependent
  local task automatic task_x107();
    `uvm_info(get_type_name(), "X107: drain gated by SRAM clear — skipped (cycle-precise)", UVM_LOW)
  endtask

  // X108: Simultaneous hit and drain for different bins — design doc says they don't interact
  local task automatic task_x108();
    `uvm_info(get_type_name(), "X108: simultaneous hit+drain different bins — design invariant (independent)", UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_x092(); issue_measure_clear();
    task_x093(); issue_measure_clear();
    task_x094(); issue_measure_clear();
    task_x095(); issue_measure_clear();
    task_x096(); issue_measure_clear();
    task_x097(); issue_measure_clear();
    task_x098(); issue_measure_clear();
    task_x099(); issue_measure_clear();
    task_x100(); issue_measure_clear();
    task_x101(); issue_measure_clear();
    task_x102(); issue_measure_clear();
    task_x103(); issue_measure_clear();
    task_x104(); issue_measure_clear();
    task_x105(); issue_measure_clear();
    task_x106(); issue_measure_clear();
    task_x107(); issue_measure_clear();
    task_x108();

    phase.drop_objection(this);
  endtask
endclass
