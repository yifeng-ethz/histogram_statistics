`ifndef HIST_CSR_SEQUENCER_SV
`define HIST_CSR_SEQUENCER_SV

import uvm_pkg::*;
`include "uvm_macros.svh"

class hist_csr_sequencer extends uvm_sequencer #(hist_csr_seq_item);
  `uvm_component_utils(hist_csr_sequencer)

  function new(string name = "hist_csr_sequencer", uvm_component parent = null);
    super.new(name, parent);
  endfunction
endclass

`endif
