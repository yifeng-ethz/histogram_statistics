`ifndef HIST_FILL_DRIVER_SV
`define HIST_FILL_DRIVER_SV

import uvm_pkg::*;
`include "uvm_macros.svh"

class hist_fill_driver extends uvm_driver #(hist_fill_seq_item);
  `uvm_component_utils(hist_fill_driver)

  localparam int unsigned N_PORTS = 8;

  virtual hist_fill_if vif;

  function new(string name = "hist_fill_driver", uvm_component parent = null);
    super.new(name, parent);
  endfunction

  function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    if (!uvm_config_db#(virtual hist_fill_if)::get(this, "", "vif", vif)) begin
      `uvm_fatal(get_type_name(), "Missing hist_fill_if in uvm_config_db under key 'vif'")
    end
  endfunction

  task run_phase(uvm_phase phase);
    hist_fill_seq_item req;

    reset_outputs();
    forever begin
      wait_for_reset_release();
      seq_item_port.get_next_item(req);
      if (req == null) begin
        `uvm_error(get_type_name(), "Received null hist_fill_seq_item")
        seq_item_port.item_done();
        continue;
      end

      drive_item(req);
      seq_item_port.item_done();
    end
  endtask

  task automatic wait_for_reset_release();
    while (vif.i_rst === 1'b1) begin
      reset_outputs();
      @(posedge vif.i_clk);
    end
  endtask

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

  task automatic drive_port_idle(int unsigned port_index);
    case (port_index)
      0: begin
        vif.asi_hist_fill_in_valid         <= 1'b0;
        vif.asi_hist_fill_in_data          <= '0;
        vif.asi_hist_fill_in_startofpacket <= 1'b0;
        vif.asi_hist_fill_in_endofpacket   <= 1'b0;
        vif.asi_hist_fill_in_channel       <= '0;
      end
      1: begin
        vif.asi_fill_in_1_valid         <= 1'b0;
        vif.asi_fill_in_1_data          <= '0;
        vif.asi_fill_in_1_startofpacket <= 1'b0;
        vif.asi_fill_in_1_endofpacket   <= 1'b0;
        vif.asi_fill_in_1_channel       <= '0;
      end
      2: begin
        vif.asi_fill_in_2_valid         <= 1'b0;
        vif.asi_fill_in_2_data          <= '0;
        vif.asi_fill_in_2_startofpacket <= 1'b0;
        vif.asi_fill_in_2_endofpacket   <= 1'b0;
        vif.asi_fill_in_2_channel       <= '0;
      end
      3: begin
        vif.asi_fill_in_3_valid         <= 1'b0;
        vif.asi_fill_in_3_data          <= '0;
        vif.asi_fill_in_3_startofpacket <= 1'b0;
        vif.asi_fill_in_3_endofpacket   <= 1'b0;
        vif.asi_fill_in_3_channel       <= '0;
      end
      4: begin
        vif.asi_fill_in_4_valid         <= 1'b0;
        vif.asi_fill_in_4_data          <= '0;
        vif.asi_fill_in_4_startofpacket <= 1'b0;
        vif.asi_fill_in_4_endofpacket   <= 1'b0;
        vif.asi_fill_in_4_channel       <= '0;
      end
      5: begin
        vif.asi_fill_in_5_valid         <= 1'b0;
        vif.asi_fill_in_5_data          <= '0;
        vif.asi_fill_in_5_startofpacket <= 1'b0;
        vif.asi_fill_in_5_endofpacket   <= 1'b0;
        vif.asi_fill_in_5_channel       <= '0;
      end
      6: begin
        vif.asi_fill_in_6_valid         <= 1'b0;
        vif.asi_fill_in_6_data          <= '0;
        vif.asi_fill_in_6_startofpacket <= 1'b0;
        vif.asi_fill_in_6_endofpacket   <= 1'b0;
        vif.asi_fill_in_6_channel       <= '0;
      end
      7: begin
        vif.asi_fill_in_7_valid         <= 1'b0;
        vif.asi_fill_in_7_data          <= '0;
        vif.asi_fill_in_7_startofpacket <= 1'b0;
        vif.asi_fill_in_7_endofpacket   <= 1'b0;
        vif.asi_fill_in_7_channel       <= '0;
      end
      default: begin
        `uvm_fatal(get_type_name(), $sformatf("Invalid fill port index %0d", port_index))
      end
    endcase
  endtask

  task automatic drive_port_item(hist_fill_seq_item item);
    case (item.port_index)
      0: begin
        vif.asi_hist_fill_in_valid         <= 1'b1;
        vif.asi_hist_fill_in_data          <= item.data;
        vif.asi_hist_fill_in_startofpacket <= item.sop;
        vif.asi_hist_fill_in_endofpacket   <= item.eop;
        vif.asi_hist_fill_in_channel       <= item.channel;
      end
      1: begin
        vif.asi_fill_in_1_valid         <= 1'b1;
        vif.asi_fill_in_1_data          <= item.data;
        vif.asi_fill_in_1_startofpacket <= item.sop;
        vif.asi_fill_in_1_endofpacket   <= item.eop;
        vif.asi_fill_in_1_channel       <= item.channel;
      end
      2: begin
        vif.asi_fill_in_2_valid         <= 1'b1;
        vif.asi_fill_in_2_data          <= item.data;
        vif.asi_fill_in_2_startofpacket <= item.sop;
        vif.asi_fill_in_2_endofpacket   <= item.eop;
        vif.asi_fill_in_2_channel       <= item.channel;
      end
      3: begin
        vif.asi_fill_in_3_valid         <= 1'b1;
        vif.asi_fill_in_3_data          <= item.data;
        vif.asi_fill_in_3_startofpacket <= item.sop;
        vif.asi_fill_in_3_endofpacket   <= item.eop;
        vif.asi_fill_in_3_channel       <= item.channel;
      end
      4: begin
        vif.asi_fill_in_4_valid         <= 1'b1;
        vif.asi_fill_in_4_data          <= item.data;
        vif.asi_fill_in_4_startofpacket <= item.sop;
        vif.asi_fill_in_4_endofpacket   <= item.eop;
        vif.asi_fill_in_4_channel       <= item.channel;
      end
      5: begin
        vif.asi_fill_in_5_valid         <= 1'b1;
        vif.asi_fill_in_5_data          <= item.data;
        vif.asi_fill_in_5_startofpacket <= item.sop;
        vif.asi_fill_in_5_endofpacket   <= item.eop;
        vif.asi_fill_in_5_channel       <= item.channel;
      end
      6: begin
        vif.asi_fill_in_6_valid         <= 1'b1;
        vif.asi_fill_in_6_data          <= item.data;
        vif.asi_fill_in_6_startofpacket <= item.sop;
        vif.asi_fill_in_6_endofpacket   <= item.eop;
        vif.asi_fill_in_6_channel       <= item.channel;
      end
      7: begin
        vif.asi_fill_in_7_valid         <= 1'b1;
        vif.asi_fill_in_7_data          <= item.data;
        vif.asi_fill_in_7_startofpacket <= item.sop;
        vif.asi_fill_in_7_endofpacket   <= item.eop;
        vif.asi_fill_in_7_channel       <= item.channel;
      end
      default: begin
        `uvm_fatal(get_type_name(), $sformatf("Invalid fill port index %0d", item.port_index))
      end
    endcase
  endtask

  task automatic reset_outputs();
    for (int unsigned idx = 0; idx < N_PORTS; idx++) begin
      drive_port_idle(idx);
    end
  endtask

  task automatic drive_item(hist_fill_seq_item item);
    @(posedge vif.i_clk);
    drive_port_item(item);

    forever begin
      @(posedge vif.i_clk);
      if (get_port_ready(item.port_index) === 1'b1) begin
        break;
      end
    end

    drive_port_idle(item.port_index);
  endtask
endclass

`endif
