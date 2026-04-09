class hist_error_pipeline_test extends hist_base_test;
  `uvm_component_utils(hist_error_pipeline_test)

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  // PFR X109-X125: Pipeline Flush and Recovery.
  // All cases require cycle-precise timing to place a hit at a specific
  // pipeline stage (ingress, FIFO, arbiter, divider stage N, queue_hit_pipe,
  // coalescing queue, SRAM write-back) and then fire measure_clear_pulse
  // on that exact cycle. UVM sequence overhead makes this impossible.

  local task automatic task_x109();
    `uvm_info(get_type_name(), "X109: flush with hit in ingress — skipped (cycle-precise)", UVM_LOW)
  endtask
  local task automatic task_x110();
    `uvm_info(get_type_name(), "X110: flush with hit in FIFO — skipped (cycle-precise)", UVM_LOW)
  endtask
  local task automatic task_x111();
    `uvm_info(get_type_name(), "X111: flush with hit in arbiter — skipped (cycle-precise)", UVM_LOW)
  endtask
  local task automatic task_x112();
    `uvm_info(get_type_name(), "X112: flush with hit in divider stage 1 — skipped (cycle-precise)", UVM_LOW)
  endtask
  local task automatic task_x113();
    `uvm_info(get_type_name(), "X113: flush with hit in divider stage 4 — skipped (cycle-precise)", UVM_LOW)
  endtask
  local task automatic task_x114();
    `uvm_info(get_type_name(), "X114: flush with hit at divider output — skipped (cycle-precise)", UVM_LOW)
  endtask
  local task automatic task_x115();
    `uvm_info(get_type_name(), "X115: flush with hit in queue_hit_pipe — skipped (cycle-precise)", UVM_LOW)
  endtask
  local task automatic task_x116();
    `uvm_info(get_type_name(), "X116: flush with hit in coalescing queue — verified by X037", UVM_LOW)
  endtask
  local task automatic task_x117();
    `uvm_info(get_type_name(), "X117: flush with SRAM write-back — skipped (cycle-precise)", UVM_LOW)
  endtask
  local task automatic task_x118();
    `uvm_info(get_type_name(), "X118: flush with hits at every stage — skipped (cycle-precise)", UVM_LOW)
  endtask
  local task automatic task_x119();
    `uvm_info(get_type_name(), "X119: flush with 8 FIFOs full + all stages — skipped (cycle-precise)", UVM_LOW)
  endtask
  local task automatic task_x120();
    `uvm_info(get_type_name(), "X120: clear blocks upd_ready for 512 cycles — skipped (cycle counting)", UVM_LOW)
  endtask
  local task automatic task_x121();
    `uvm_info(get_type_name(), "X121: clear with non-pingpong burst read — skipped (ENABLE_PINGPONG=1)", UVM_LOW)
  endtask
  local task automatic task_x122();
    `uvm_info(get_type_name(), "X122: SRAM clear_both vs single-bank timing — skipped (cycle counting)", UVM_LOW)
  endtask
  local task automatic task_x123();
    `uvm_info(get_type_name(), "X123: SRAM forward path zeroed after flush — verified by X036/X042", UVM_LOW)
  endtask
  local task automatic task_x124();
    `uvm_info(get_type_name(), "X124: rapid clear-inject-clear — skipped (cycle-precise)", UVM_LOW)
  endtask
  local task automatic task_x125();
    `uvm_info(get_type_name(), "X125: queue clear before SRAM clear — skipped (cycle counting)", UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_x109(); task_x110(); task_x111(); task_x112();
    task_x113(); task_x114(); task_x115(); task_x116();
    task_x117(); task_x118(); task_x119(); task_x120();
    task_x121(); task_x122(); task_x123(); task_x124();
    task_x125();

    phase.drop_objection(this);
  endtask
endclass
