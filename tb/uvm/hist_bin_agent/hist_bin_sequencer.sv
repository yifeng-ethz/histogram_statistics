`ifndef HIST_BIN_SEQUENCER_SV
`define HIST_BIN_SEQUENCER_SV

import uvm_pkg::*;
`include "uvm_macros.svh"

class hist_bin_sequencer extends uvm_sequencer #(hist_bin_seq_item);
  `uvm_component_utils(hist_bin_sequencer)

  function new(string name = "hist_bin_sequencer", uvm_component parent = null);
    super.new(name, parent);
  endfunction
endclass

`endif
