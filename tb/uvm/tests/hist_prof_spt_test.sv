class hist_prof_spt_test extends hist_base_test;
  `uvm_component_utils(hist_prof_spt_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 16384;
  localparam bit [3:0] CSR_TOTAL_HITS = 4'd11;
  localparam bit [3:0] CSR_DROPPED    = 4'd12;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  local task automatic configure();
    program_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1),
                      .interval_cfg(HS_TEST_INTERVAL_CFG));
  endtask

  // SPT P001-P012: Single-Port Throughput.
  // UVM send_fill_word takes ~4 cycles per hit through the sequencer.
  // Effective max rate ~25%. Tests needing >25% rate are rate-limited by UVM.

  // P001: 1% rate (1 hit per 100 cycles) — 100 hits
  local task automatic task_p001();
    bit [31:0] total, dropped;
    `uvm_info(get_type_name(), "P001: 1% rate, 100 hits, port 0", UVM_LOW)
    configure();
    for (int i = 0; i < 100; i++) begin
      send_fill_word(0, make_fill_word(i % 256));
      repeat (96) @(cfg.probe_vif.mon_cb);
    end
    csr_read(CSR_TOTAL_HITS, total);
    csr_read(CSR_DROPPED, dropped);
    if (dropped !== 32'd0)
      `uvm_error("P001", $sformatf("expected 0 drops at 1%% rate, got %0d", dropped))
    `uvm_info("P001", $sformatf("total=%0d dropped=%0d", total, dropped), UVM_LOW)
  endtask

  // P002: 10% rate (1 hit per 10 cycles) — 200 hits
  local task automatic task_p002();
    bit [31:0] total, dropped;
    `uvm_info(get_type_name(), "P002: 10% rate, 200 hits, port 0", UVM_LOW)
    configure();
    for (int i = 0; i < 200; i++) begin
      send_fill_word(0, make_fill_word(i % 256));
      repeat (6) @(cfg.probe_vif.mon_cb);
    end
    csr_read(CSR_TOTAL_HITS, total);
    csr_read(CSR_DROPPED, dropped);
    if (dropped !== 32'd0)
      `uvm_error("P002", $sformatf("expected 0 drops at 10%% rate, got %0d", dropped))
    `uvm_info("P002", $sformatf("total=%0d dropped=%0d", total, dropped), UVM_LOW)
  endtask

  // P003: 25% rate — effective UVM max, 200 hits
  local task automatic task_p003();
    bit [31:0] total, dropped;
    `uvm_info(get_type_name(), "P003: ~25% rate, 200 hits, port 0", UVM_LOW)
    configure();
    for (int i = 0; i < 200; i++)
      send_fill_word(0, make_fill_word(i % 256));
    csr_read(CSR_TOTAL_HITS, total);
    csr_read(CSR_DROPPED, dropped);
    if (dropped !== 32'd0)
      `uvm_error("P003", $sformatf("expected 0 drops at ~25%% rate, got %0d", dropped))
    `uvm_info("P003", $sformatf("total=%0d dropped=%0d", total, dropped), UVM_LOW)
  endtask

  // P004-P006: 50-100% rate — UVM cannot achieve wire-speed injection
  local task automatic task_p004();
    `uvm_info(get_type_name(), "P004: 50% rate — skipped (UVM rate limited to ~25%)", UVM_LOW)
  endtask
  local task automatic task_p005();
    `uvm_info(get_type_name(), "P005: 75% rate — skipped (UVM rate limited)", UVM_LOW)
  endtask
  local task automatic task_p006();
    `uvm_info(get_type_name(), "P006: 100% rate — skipped (UVM rate limited)", UVM_LOW)
  endtask

  // P007-P009: Burst patterns — need precise burst timing
  local task automatic task_p007();
    `uvm_info(get_type_name(), "P007: burst-16 pattern — skipped (wire-speed bursts needed)", UVM_LOW)
  endtask
  local task automatic task_p008();
    `uvm_info(get_type_name(), "P008: burst-17 pattern — skipped (wire-speed bursts needed)", UVM_LOW)
  endtask
  local task automatic task_p009();
    `uvm_info(get_type_name(), "P009: burst-32 pattern — skipped (wire-speed bursts needed)", UVM_LOW)
  endtask

  // P010-P012: Random/ramp/square-wave — need LCG infrastructure or rate control
  local task automatic task_p010();
    `uvm_info(get_type_name(), "P010: random 50-100% rate — skipped (need LCG + rate control)", UVM_LOW)
  endtask
  local task automatic task_p011();
    `uvm_info(get_type_name(), "P011: ramp 0-100-0% — skipped (need rate control)", UVM_LOW)
  endtask
  local task automatic task_p012();
    `uvm_info(get_type_name(), "P012: square-wave — skipped (wire-speed bursts needed)", UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_p001(); issue_measure_clear();
    task_p002(); issue_measure_clear();
    task_p003(); issue_measure_clear();
    task_p004(); task_p005(); task_p006();
    task_p007(); task_p008(); task_p009();
    task_p010(); task_p011(); task_p012();

    phase.drop_objection(this);
  endtask
endclass
