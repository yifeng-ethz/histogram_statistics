`ifndef HIST_BIN_MONITOR_SV
`define HIST_BIN_MONITOR_SV

import uvm_pkg::*;
`include "uvm_macros.svh"

class hist_bin_monitor extends uvm_monitor;
  `uvm_component_utils(hist_bin_monitor)

  virtual hist_bin_if vif;
  uvm_analysis_port #(hist_bin_seq_item) ap;

  function new(string name = "hist_bin_monitor", uvm_component parent = null);
    super.new(name, parent);
    ap = new("ap", this);
  endfunction

  function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    if (!uvm_config_db#(virtual hist_bin_if)::get(this, "", "vif", vif)) begin
      `uvm_fatal(get_type_name(), "Missing hist_bin_if in uvm_config_db under key 'vif'")
    end
  endfunction

  task run_phase(uvm_phase phase);
    hist_bin_seq_item pending_reads[$];
    hist_bin_seq_item active_read;
    int unsigned      active_index;
    int unsigned      active_words;
    hist_bin_seq_item txn;

    active_read  = null;
    active_index = 0;
    active_words = 0;

    forever begin
      @(posedge vif.i_clk);
      #1step;

      if (vif.i_rst === 1'b1) begin
        pending_reads.delete();
        active_read  = null;
        active_index = 0;
        active_words = 0;
        continue;
      end

      if ((vif.avs_hist_bin_write === 1'b1) && (vif.avs_hist_bin_waitrequest !== 1'b1)) begin
        txn = hist_bin_seq_item::type_id::create("hist_bin_write_txn", this);
        txn.is_write  = 1'b1;
        txn.address   = vif.avs_hist_bin_address;
        txn.burstcount = vif.avs_hist_bin_burstcount;
        txn.writedata = vif.avs_hist_bin_writedata;
        txn.readdata  = new[0];
        ap.write(txn);
      end

      if ((vif.avs_hist_bin_read === 1'b1) && (vif.avs_hist_bin_waitrequest !== 1'b1)) begin
        txn = hist_bin_seq_item::type_id::create("hist_bin_read_cmd", this);
        txn.is_write   = 1'b0;
        txn.address    = vif.avs_hist_bin_address;
        txn.burstcount = vif.avs_hist_bin_burstcount;
        txn.writedata  = '0;
        txn.readdata   = new[0];
        pending_reads.push_back(txn);
      end

      if (vif.avs_hist_bin_readdatavalid === 1'b1) begin
        if (active_read == null) begin
          if (pending_reads.size() == 0) begin
            `uvm_error(get_type_name(), "Observed avs_hist_bin_readdatavalid with no pending read command")
            continue;
          end

          active_read  = pending_reads.pop_front();
          active_words = active_read.effective_burstcount();
          active_index = 0;
          active_read.readdata = new[active_words];
        end

        if (active_index >= active_words) begin
          `uvm_error(get_type_name(), "Received more hist_bin read data than requested")
          active_read  = null;
          active_index = 0;
          active_words = 0;
          continue;
        end

        active_read.readdata[active_index] = vif.avs_hist_bin_readdata;
        active_index++;

        if (active_index == active_words) begin
          ap.write(active_read);
          active_read  = null;
          active_index = 0;
          active_words = 0;
        end
      end
    end
  endtask
endclass

`endif
