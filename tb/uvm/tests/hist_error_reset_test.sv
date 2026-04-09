class hist_error_reset_test extends hist_base_test;
  `uvm_component_utils(hist_error_reset_test)

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  // RST X001-X027: Hard reset behavior.
  // All cases require driving i_rst from UVM, but i_rst is controlled by
  // tb_top's initial block and is not accessible from UVM sequences.
  // All cases skipped.

  local task automatic task_x001();
    `uvm_info(get_type_name(), "X001: all CSR defaults after reset — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x002();
    `uvm_info(get_type_name(), "X002: config shadow defaults after reset — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x003();
    `uvm_info(get_type_name(), "X003: all 8 FIFOs empty after reset — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x004();
    `uvm_info(get_type_name(), "X004: queue empty after reset — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x005();
    `uvm_info(get_type_name(), "X005: both SRAM banks zeroed after reset — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x006();
    `uvm_info(get_type_name(), "X006: stats counters zeroed after reset — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x007();
    `uvm_info(get_type_name(), "X007: active_bank=0 after reset — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x008();
    `uvm_info(get_type_name(), "X008: interval timer zeroed after reset — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x009();
    `uvm_info(get_type_name(), "X009: reset with hits in ingress — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x010();
    `uvm_info(get_type_name(), "X010: reset with data in FIFOs — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x011();
    `uvm_info(get_type_name(), "X011: reset with arbiter mid-grant — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x012();
    `uvm_info(get_type_name(), "X012: reset with divider mid-pipeline — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x013();
    `uvm_info(get_type_name(), "X013: reset with 50 queue entries — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x014();
    `uvm_info(get_type_name(), "X014: reset with SRAM update mid-flight — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x015();
    `uvm_info(get_type_name(), "X015: reset during SRAM clear — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x016();
    `uvm_info(get_type_name(), "X016: reset during burst read — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x017();
    `uvm_info(get_type_name(), "X017: reset during CSR read — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x018();
    `uvm_info(get_type_name(), "X018: reset during CSR write — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x019();
    `uvm_info(get_type_name(), "X019: reset during hist_bin write — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x020();
    `uvm_info(get_type_name(), "X020: 1-cycle reset pulse — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x021();
    `uvm_info(get_type_name(), "X021: 100-cycle sustained reset — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x022();
    `uvm_info(get_type_name(), "X022: reset deassert + immediate hit — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x023();
    `uvm_info(get_type_name(), "X023: reset deassert + immediate CSR write — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x024();
    `uvm_info(get_type_name(), "X024: double reset — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x025();
    `uvm_info(get_type_name(), "X025: reset glitch 1-cycle gap — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x026();
    `uvm_info(get_type_name(), "X026: reset on power-on — skipped (no i_rst access)", UVM_LOW)
  endtask
  local task automatic task_x027();
    `uvm_info(get_type_name(), "X027: reset during error+pending — skipped (no i_rst access)", UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_x001(); task_x002(); task_x003(); task_x004();
    task_x005(); task_x006(); task_x007(); task_x008();
    task_x009(); task_x010(); task_x011(); task_x012();
    task_x013(); task_x014(); task_x015(); task_x016();
    task_x017(); task_x018(); task_x019(); task_x020();
    task_x021(); task_x022(); task_x023(); task_x024();
    task_x025(); task_x026(); task_x027();

    phase.drop_objection(this);
  endtask
endclass
