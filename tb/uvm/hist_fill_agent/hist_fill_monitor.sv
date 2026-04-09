`ifndef HIST_FILL_MONITOR_SV
`define HIST_FILL_MONITOR_SV

import uvm_pkg::*;
`include "uvm_macros.svh"

class hist_fill_monitor extends uvm_monitor;
  `uvm_component_utils(hist_fill_monitor)

  localparam int unsigned N_PORTS = 8;

  virtual hist_fill_if vif;
  uvm_analysis_port #(hist_fill_seq_item) ap;

  function new(string name = "hist_fill_monitor", uvm_component parent = null);
    super.new(name, parent);
    ap = new("ap", this);
  endfunction

  function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    if (!uvm_config_db#(virtual hist_fill_if)::get(this, "", "vif", vif)) begin
      `uvm_fatal(get_type_name(), "Missing hist_fill_if in uvm_config_db under key 'vif'")
    end
  endfunction

  function automatic bit get_port_valid(int unsigned port_index);
    case (port_index)
      0: return vif.asi_hist_fill_in_valid;
      1: return vif.asi_fill_in_1_valid;
      2: return vif.asi_fill_in_2_valid;
      3: return vif.asi_fill_in_3_valid;
      4: return vif.asi_fill_in_4_valid;
      5: return vif.asi_fill_in_5_valid;
      6: return vif.asi_fill_in_6_valid;
      7: return vif.asi_fill_in_7_valid;
      default: return 1'b0;
    endcase
  endfunction

  function automatic bit get_port_ready(int unsigned port_index);
    case (port_index)
      0: return vif.asi_hist_fill_in_ready;
      1: return vif.asi_fill_in_1_ready;
      2: return vif.asi_fill_in_2_ready;
      3: return vif.asi_fill_in_3_ready;
      4: return vif.asi_fill_in_4_ready;
      5: return vif.asi_fill_in_5_ready;
      6: return vif.asi_fill_in_6_ready;
      7: return vif.asi_fill_in_7_ready;
      default: return 1'b0;
    endcase
  endfunction

  function automatic bit [38:0] get_port_data(int unsigned port_index);
    case (port_index)
      0: return vif.asi_hist_fill_in_data;
      1: return vif.asi_fill_in_1_data;
      2: return vif.asi_fill_in_2_data;
      3: return vif.asi_fill_in_3_data;
      4: return vif.asi_fill_in_4_data;
      5: return vif.asi_fill_in_5_data;
      6: return vif.asi_fill_in_6_data;
      7: return vif.asi_fill_in_7_data;
      default: return '0;
    endcase
  endfunction

  function automatic bit get_port_sop(int unsigned port_index);
    case (port_index)
      0: return vif.asi_hist_fill_in_startofpacket;
      1: return vif.asi_fill_in_1_startofpacket;
      2: return vif.asi_fill_in_2_startofpacket;
      3: return vif.asi_fill_in_3_startofpacket;
      4: return vif.asi_fill_in_4_startofpacket;
      5: return vif.asi_fill_in_5_startofpacket;
      6: return vif.asi_fill_in_6_startofpacket;
      7: return vif.asi_fill_in_7_startofpacket;
      default: return 1'b0;
    endcase
  endfunction

  function automatic bit get_port_eop(int unsigned port_index);
    case (port_index)
      0: return vif.asi_hist_fill_in_endofpacket;
      1: return vif.asi_fill_in_1_endofpacket;
      2: return vif.asi_fill_in_2_endofpacket;
      3: return vif.asi_fill_in_3_endofpacket;
      4: return vif.asi_fill_in_4_endofpacket;
      5: return vif.asi_fill_in_5_endofpacket;
      6: return vif.asi_fill_in_6_endofpacket;
      7: return vif.asi_fill_in_7_endofpacket;
      default: return 1'b0;
    endcase
  endfunction

  function automatic bit [3:0] get_port_channel(int unsigned port_index);
    case (port_index)
      0: return vif.asi_hist_fill_in_channel;
      1: return vif.asi_fill_in_1_channel;
      2: return vif.asi_fill_in_2_channel;
      3: return vif.asi_fill_in_3_channel;
      4: return vif.asi_fill_in_4_channel;
      5: return vif.asi_fill_in_5_channel;
      6: return vif.asi_fill_in_6_channel;
      7: return vif.asi_fill_in_7_channel;
      default: return '0;
    endcase
  endfunction

  task run_phase(uvm_phase phase);
    hist_fill_seq_item txn;

    forever begin
      @(posedge vif.i_clk);
      #1step;

      if (vif.i_rst === 1'b1) begin
        continue;
      end

      for (int unsigned port_idx = 0; port_idx < N_PORTS; port_idx++) begin
        if ((get_port_valid(port_idx) === 1'b1) && (get_port_ready(port_idx) === 1'b1)) begin
          txn = hist_fill_seq_item::type_id::create($sformatf("fill_txn_%0d", port_idx), this);
          txn.port_index = port_idx[2:0];
          txn.data       = get_port_data(port_idx);
          txn.channel    = get_port_channel(port_idx);
          txn.sop        = get_port_sop(port_idx);
          txn.eop        = get_port_eop(port_idx);
          ap.write(txn);
        end
      end
    end
  endtask
endclass

`endif
