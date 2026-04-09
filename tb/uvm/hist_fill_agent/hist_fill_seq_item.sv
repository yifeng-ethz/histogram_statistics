`ifndef HIST_FILL_SEQ_ITEM_SV
`define HIST_FILL_SEQ_ITEM_SV

import uvm_pkg::*;
`include "uvm_macros.svh"

class hist_fill_seq_item extends uvm_sequence_item;
  `uvm_object_utils(hist_fill_seq_item)

  rand bit [2:0]  port_index;
  rand bit [38:0] data;
  rand bit [3:0]  channel;
  rand bit        sop;
  rand bit        eop;

  constraint c_port_index {
    port_index inside {[3'd0:3'd7]};
  }

  function new(string name = "hist_fill_seq_item");
    super.new(name);
  endfunction

  function string convert2string();
    return $sformatf(
      "port_index=%0d data=0x%010h channel=0x%0h sop=%0b eop=%0b",
      port_index,
      data,
      channel,
      sop,
      eop
    );
  endfunction
endclass

`endif
