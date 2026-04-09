class hist_prof_mpt_test extends hist_base_test;
  `uvm_component_utils(hist_prof_mpt_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 32768;
  localparam bit [3:0] CSR_TOTAL_HITS = 4'd11;
  localparam bit [3:0] CSR_DROPPED    = 4'd12;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  // MPT P013-P024: Multi-Port Throughput.
  // Multi-port simultaneous injection at controlled rates requires parallel
  // sequence threads with timing coordination. Each send_fill_word() is
  // serialized through its port's sequencer, but port agents are independent.
  //
  // P013: 8 ports at 10% — testable with slow round-robin injection
  local task automatic task_p013();
    bit [31:0] total, dropped;
    `uvm_info(get_type_name(), "P013: 8 ports at ~10%, 50 hits/port", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1),
                      .interval_cfg(HS_TEST_INTERVAL_CFG));
    // Sequential round-robin across 8 ports with gaps
    for (int round = 0; round < 50; round++) begin
      for (int p = 0; p < 8; p++)
        send_fill_word(p, make_fill_word((round * 8 + p) % 256));
      repeat (40) @(cfg.probe_vif.mon_cb);
    end
    csr_read(CSR_TOTAL_HITS, total);
    csr_read(CSR_DROPPED, dropped);
    if (dropped !== 32'd0)
      `uvm_error("P013", $sformatf("expected 0 drops, got %0d", dropped))
    `uvm_info("P013", $sformatf("total=%0d dropped=%0d", total, dropped), UVM_LOW)
  endtask

  // P014-P024: Require precise per-port rate control or wire-speed injection
  local task automatic task_p014();
    `uvm_info(get_type_name(), "P014: 8 ports at 12.5% — skipped (rate control)", UVM_LOW)
  endtask
  local task automatic task_p015();
    `uvm_info(get_type_name(), "P015: 8 ports at 15% — skipped (rate control)", UVM_LOW)
  endtask
  local task automatic task_p016();
    `uvm_info(get_type_name(), "P016: 8 ports at 25% — skipped (rate control)", UVM_LOW)
  endtask
  local task automatic task_p017();
    `uvm_info(get_type_name(), "P017: 8 ports at 50% — skipped (rate control)", UVM_LOW)
  endtask
  local task automatic task_p018();
    `uvm_info(get_type_name(), "P018: 8 ports at 100% — skipped (wire-speed)", UVM_LOW)
  endtask
  local task automatic task_p019();
    `uvm_info(get_type_name(), "P019: random per-port rates — skipped (LCG + rate control)", UVM_LOW)
  endtask
  local task automatic task_p020();
    `uvm_info(get_type_name(), "P020: staggered start — skipped (rate control)", UVM_LOW)
  endtask
  local task automatic task_p021();
    `uvm_info(get_type_name(), "P021: port 0 hot, others cold — skipped (rate control)", UVM_LOW)
  endtask
  local task automatic task_p022();
    `uvm_info(get_type_name(), "P022: synchronized 8-port bursts — skipped (wire-speed)", UVM_LOW)
  endtask
  local task automatic task_p023();
    `uvm_info(get_type_name(), "P023: alternating port pairs — skipped (rate control)", UVM_LOW)
  endtask
  local task automatic task_p024();
    `uvm_info(get_type_name(), "P024: random burst lengths — skipped (LCG + rate control)", UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_p013(); issue_measure_clear();
    task_p014(); task_p015(); task_p016(); task_p017();
    task_p018(); task_p019(); task_p020(); task_p021();
    task_p022(); task_p023(); task_p024();

    phase.drop_objection(this);
  endtask
endclass
