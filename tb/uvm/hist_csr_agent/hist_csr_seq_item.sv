`ifndef HIST_CSR_SEQ_ITEM_SV
`define HIST_CSR_SEQ_ITEM_SV

import uvm_pkg::*;
`include "uvm_macros.svh"

class hist_csr_seq_item extends uvm_sequence_item;
  `uvm_object_utils(hist_csr_seq_item)

  rand bit        is_write;
  rand bit [4:0]  address;
  rand bit [31:0] writedata;
  bit [31:0]      readdata;

  constraint c_address {
    address inside {[5'h00:5'h10]};
  }

  function new(string name = "hist_csr_seq_item");
    super.new(name);
  endfunction

  function string convert2string();
    return $sformatf(
      "is_write=%0b address=0x%0h writedata=0x%08h readdata=0x%08h",
      is_write,
      address,
      writedata,
      readdata
    );
  endfunction
endclass

`endif
