`ifndef HIST_CSR_AGENT_SV
`define HIST_CSR_AGENT_SV

import uvm_pkg::*;
`include "uvm_macros.svh"

class hist_csr_agent extends uvm_agent;
  `uvm_component_utils(hist_csr_agent)

  hist_csr_sequencer sequencer_h;
  hist_csr_driver    driver_h;
  hist_csr_monitor   monitor_h;

  function new(string name = "hist_csr_agent", uvm_component parent = null);
    super.new(name, parent);
  endfunction

  function void build_phase(uvm_phase phase);
    super.build_phase(phase);

    if (get_is_active() == UVM_ACTIVE) begin
      sequencer_h = hist_csr_sequencer::type_id::create("sequencer_h", this);
      driver_h    = hist_csr_driver::type_id::create("driver_h", this);
    end

    monitor_h = hist_csr_monitor::type_id::create("monitor_h", this);
  endfunction

  function void connect_phase(uvm_phase phase);
    super.connect_phase(phase);

    if (get_is_active() == UVM_ACTIVE) begin
      driver_h.seq_item_port.connect(sequencer_h.seq_item_export);
    end
  endfunction
endclass

`endif
