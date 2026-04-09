`ifndef HIST_CTRL_SEQ_ITEM_SV
`define HIST_CTRL_SEQ_ITEM_SV

import uvm_pkg::*;
`include "uvm_macros.svh"

class hist_ctrl_seq_item extends uvm_sequence_item;
  `uvm_object_utils(hist_ctrl_seq_item)

  rand bit [8:0] data;

  function new(string name = "hist_ctrl_seq_item");
    super.new(name);
  endfunction

  function string convert2string();
    return $sformatf("data=0x%0h", data);
  endfunction
endclass

`endif
