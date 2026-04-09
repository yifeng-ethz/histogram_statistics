`ifndef HIST_FILL_SEQUENCER_SV
`define HIST_FILL_SEQUENCER_SV

import uvm_pkg::*;
`include "uvm_macros.svh"

class hist_fill_sequencer extends uvm_sequencer #(hist_fill_seq_item);
  `uvm_component_utils(hist_fill_sequencer)

  function new(string name = "hist_fill_sequencer", uvm_component parent = null);
    super.new(name, parent);
  endfunction
endclass

`endif
