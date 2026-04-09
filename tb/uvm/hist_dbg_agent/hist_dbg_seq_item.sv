`ifndef HIST_DBG_SEQ_ITEM_SV
`define HIST_DBG_SEQ_ITEM_SV

import uvm_pkg::*;
`include "uvm_macros.svh"

class hist_dbg_seq_item extends uvm_sequence_item;
  `uvm_object_utils(hist_dbg_seq_item)

  rand bit [2:0]  debug_index;
  rand bit [15:0] data;

  constraint c_debug_index {
    debug_index inside {[3'd0:3'd5]};
  }

  function new(string name = "hist_dbg_seq_item");
    super.new(name);
  endfunction

  function string convert2string();
    return $sformatf("debug_index=%0d data=0x%04h", debug_index, data);
  endfunction
endclass

`endif
