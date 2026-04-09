`ifndef HIST_CTRL_SEQUENCER_SV
`define HIST_CTRL_SEQUENCER_SV

import uvm_pkg::*;
`include "uvm_macros.svh"

class hist_ctrl_sequencer extends uvm_sequencer #(hist_ctrl_seq_item);
  `uvm_component_utils(hist_ctrl_sequencer)

  function new(string name = "hist_ctrl_sequencer", uvm_component parent = null);
    super.new(name, parent);
  endfunction
endclass

`endif
