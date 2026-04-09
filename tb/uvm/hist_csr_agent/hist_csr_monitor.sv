`ifndef HIST_CSR_MONITOR_SV
`define HIST_CSR_MONITOR_SV

import uvm_pkg::*;
`include "uvm_macros.svh"

class hist_csr_monitor extends uvm_monitor;
  `uvm_component_utils(hist_csr_monitor)

  virtual hist_csr_if vif;
  uvm_analysis_port #(hist_csr_seq_item) ap;

  function new(string name = "hist_csr_monitor", uvm_component parent = null);
    super.new(name, parent);
    ap = new("ap", this);
  endfunction

  function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    if (!uvm_config_db#(virtual hist_csr_if)::get(this, "", "vif", vif)) begin
      `uvm_fatal(get_type_name(), "Missing hist_csr_if in uvm_config_db under key 'vif'")
    end
  endfunction

  task run_phase(uvm_phase phase);
    hist_csr_seq_item pending_read;
    bit               pending_read_valid;
    hist_csr_seq_item txn;

    pending_read_valid = 1'b0;

    forever begin
      @(posedge vif.i_clk);
      #1step;

      if (vif.i_rst === 1'b1) begin
        pending_read_valid = 1'b0;
        pending_read = null;
        continue;
      end

      if (pending_read_valid) begin
        pending_read.readdata = vif.avs_csr_readdata;
        ap.write(pending_read);
        pending_read_valid = 1'b0;
        pending_read = null;
      end

      if ((vif.avs_csr_write === 1'b1) && (vif.avs_csr_waitrequest !== 1'b1)) begin
        txn = hist_csr_seq_item::type_id::create("csr_write_txn", this);
        txn.is_write  = 1'b1;
        txn.address   = vif.avs_csr_address;
        txn.writedata = vif.avs_csr_writedata;
        txn.readdata  = '0;
        ap.write(txn);
      end

      if ((vif.avs_csr_read === 1'b1) && (vif.avs_csr_waitrequest !== 1'b1)) begin
        pending_read = hist_csr_seq_item::type_id::create("csr_read_txn", this);
        pending_read.is_write  = 1'b0;
        pending_read.address   = vif.avs_csr_address;
        pending_read.writedata = '0;
        pending_read.readdata  = '0;
        pending_read_valid = 1'b1;
      end
    end
  endtask
endclass

`endif
