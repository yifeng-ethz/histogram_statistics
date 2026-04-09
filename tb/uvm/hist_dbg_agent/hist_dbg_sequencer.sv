`ifndef HIST_DBG_SEQUENCER_SV
`define HIST_DBG_SEQUENCER_SV

import uvm_pkg::*;
`include "uvm_macros.svh"

class hist_dbg_sequencer extends uvm_sequencer #(hist_dbg_seq_item);
  `uvm_component_utils(hist_dbg_sequencer)

  function new(string name = "hist_dbg_sequencer", uvm_component parent = null);
    super.new(name, parent);
  endfunction
endclass

`endif
