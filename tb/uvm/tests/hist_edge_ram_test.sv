class hist_edge_ram_test extends hist_base_test;
  `uvm_component_utils(hist_edge_ram_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 8192;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  // E161-E164: Dual-port RAM bypass logic — requires direct RAM port access.
  // These test the true_dual_port_ram_single_clock bypass mux, which is internal
  // to pingpong_sram. No CSR/AVMM exposure. All skipped.

  local task automatic task_e161();
    `uvm_info(get_type_name(), "E161: port A read / port B write bypass — skipped (internal RAM)", UVM_LOW)
  endtask

  local task automatic task_e162();
    `uvm_info(get_type_name(), "E162: port B read / port A write bypass — skipped (internal RAM)", UVM_LOW)
  endtask

  local task automatic task_e163();
    `uvm_info(get_type_name(), "E163: both ports write same address — skipped (internal RAM)", UVM_LOW)
  endtask

  local task automatic task_e164();
    `uvm_info(get_type_name(), "E164: both ports read same address — skipped (internal RAM)", UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_e161(); issue_measure_clear();
    task_e162(); issue_measure_clear();
    task_e163(); issue_measure_clear();
    task_e164();

    phase.drop_objection(this);
  endtask
endclass
