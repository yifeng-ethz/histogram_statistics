`ifndef HIST_DBG_AGENT_SV
`define HIST_DBG_AGENT_SV

import uvm_pkg::*;
`include "uvm_macros.svh"

class hist_dbg_agent extends uvm_agent;
  `uvm_component_utils(hist_dbg_agent)

  hist_dbg_sequencer sequencer_h;
  hist_dbg_driver    driver_h;

  function new(string name = "hist_dbg_agent", uvm_component parent = null);
    super.new(name, parent);
  endfunction

  function void build_phase(uvm_phase phase);
    super.build_phase(phase);

    if (get_is_active() == UVM_ACTIVE) begin
      sequencer_h = hist_dbg_sequencer::type_id::create("sequencer_h", this);
      driver_h    = hist_dbg_driver::type_id::create("driver_h", this);
    end
  endfunction

  function void connect_phase(uvm_phase phase);
    super.connect_phase(phase);

    if (get_is_active() == UVM_ACTIVE) begin
      driver_h.seq_item_port.connect(sequencer_h.seq_item_export);
    end
  endfunction
endclass

`endif
