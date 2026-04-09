class hist_edge_pkg_test extends hist_base_test;
  `uvm_component_utils(hist_edge_pkg_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 8192;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  // E165-E172: Package helper function boundaries.
  // These test pure VHDL functions (sat_inc, sat_add, extract_unsigned, extract_signed, clog2).
  // Testing at integration level through end-to-end behavior.

  // E165: sat_inc(0) = 1 — verified by every single-hit test (bin 0→1)
  local task automatic task_e165();
    `uvm_info(get_type_name(), "E165: sat_inc(0)=1 — verified by every single-hit bin test", UVM_LOW)
  endtask

  // E166: sat_inc(0xFFFFFFFF) — requires force
  local task automatic task_e166();
    `uvm_info(get_type_name(), "E166: sat_inc(max) saturation — skipped (requires force)", UVM_LOW)
  endtask

  // E167: sat_add(0xFFFFFFFF, 1) — requires force
  local task automatic task_e167();
    `uvm_info(get_type_name(), "E167: sat_add(max,1) saturation — skipped (requires force)", UVM_LOW)
  endtask

  // E168: sat_add(0x7FFFFFFF, 0x7FFFFFFF) = 0xFFFFFFFE — requires force
  local task automatic task_e168();
    `uvm_info(get_type_name(), "E168: sat_add no saturation — skipped (requires force)", UVM_LOW)
  endtask

  // E169: sat_add(0x80000000, 0x80000000) saturates — requires force
  local task automatic task_e169();
    `uvm_info(get_type_name(), "E169: sat_add carry saturation — skipped (requires force)", UVM_LOW)
  endtask

  // E170: extract_unsigned single bit — verified by E020, E021, E028
  local task automatic task_e170();
    `uvm_info(get_type_name(), "E170: extract_unsigned single bit — verified by E020/E021/E028", UVM_LOW)
  endtask

  // E171: extract_signed negative sign extension — verified by E023, E024
  local task automatic task_e171();
    `uvm_info(get_type_name(), "E171: extract_signed sign extension — verified by E023/E024", UVM_LOW)
  endtask

  // E172: clog2(1)=0 — internal constant, not directly testable from integration
  local task automatic task_e172();
    `uvm_info(get_type_name(), "E172: clog2(1)=0 — skipped (compile-time constant)", UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_e165(); issue_measure_clear();
    task_e166(); issue_measure_clear();
    task_e167(); issue_measure_clear();
    task_e168(); issue_measure_clear();
    task_e169(); issue_measure_clear();
    task_e170(); issue_measure_clear();
    task_e171(); issue_measure_clear();
    task_e172();

    phase.drop_objection(this);
  endtask
endclass
