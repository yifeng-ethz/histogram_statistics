`ifndef HIST_BIN_SEQ_ITEM_SV
`define HIST_BIN_SEQ_ITEM_SV

import uvm_pkg::*;
`include "uvm_macros.svh"

class hist_bin_seq_item extends uvm_sequence_item;
  `uvm_object_utils(hist_bin_seq_item)

  rand bit        is_write;
  rand bit [7:0]  address;
  rand bit [7:0]  burstcount;
  rand bit [31:0] writedata;
  bit [31:0]      readdata[];

  constraint c_write_burstcount {
    if (is_write) burstcount == 8'h01;
  }

  function new(string name = "hist_bin_seq_item");
    super.new(name);
  endfunction

  function int unsigned effective_burstcount();
    return (burstcount == 8'h00) ? 1 : int'(burstcount);
  endfunction

  function string convert2string();
    return $sformatf(
      "is_write=%0b address=0x%0h burstcount=0x%0h effective_burstcount=%0d writedata=0x%08h readdata_words=%0d",
      is_write,
      address,
      burstcount,
      effective_burstcount(),
      writedata,
      readdata.size()
    );
  endfunction
endclass

`endif
