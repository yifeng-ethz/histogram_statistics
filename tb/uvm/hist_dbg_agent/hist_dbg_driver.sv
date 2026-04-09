`ifndef HIST_DBG_DRIVER_SV
`define HIST_DBG_DRIVER_SV

import uvm_pkg::*;
`include "uvm_macros.svh"

class hist_dbg_driver extends uvm_driver #(hist_dbg_seq_item);
  `uvm_component_utils(hist_dbg_driver)

  localparam int unsigned N_DEBUG_IFS = 6;

  virtual hist_dbg_if vif;

  function new(string name = "hist_dbg_driver", uvm_component parent = null);
    super.new(name, parent);
  endfunction

  function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    if (!uvm_config_db#(virtual hist_dbg_if)::get(this, "", "vif", vif)) begin
      `uvm_fatal(get_type_name(), "Missing hist_dbg_if in uvm_config_db under key 'vif'")
    end
  endfunction

  task run_phase(uvm_phase phase);
    hist_dbg_seq_item req;

    reset_outputs();
    forever begin
      wait_for_reset_release();
      seq_item_port.get_next_item(req);
      if (req == null) begin
        `uvm_error(get_type_name(), "Received null hist_dbg_seq_item")
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

  task automatic drive_debug_idle(int unsigned debug_index);
    case (debug_index)
      0: begin
        vif.asi_debug_1_valid <= 1'b0;
        vif.asi_debug_1_data  <= '0;
      end
      1: begin
        vif.asi_debug_2_valid <= 1'b0;
        vif.asi_debug_2_data  <= '0;
      end
      2: begin
        vif.asi_debug_3_valid <= 1'b0;
        vif.asi_debug_3_data  <= '0;
      end
      3: begin
        vif.asi_debug_4_valid <= 1'b0;
        vif.asi_debug_4_data  <= '0;
      end
      4: begin
        vif.asi_debug_5_valid <= 1'b0;
        vif.asi_debug_5_data  <= '0;
      end
      5: begin
        vif.asi_debug_6_valid <= 1'b0;
        vif.asi_debug_6_data  <= '0;
      end
      default: begin
        `uvm_fatal(get_type_name(), $sformatf("Invalid debug index %0d", debug_index))
      end
    endcase
  endtask

  task automatic drive_debug_item(hist_dbg_seq_item item);
    case (item.debug_index)
      0: begin
        vif.asi_debug_1_valid <= 1'b1;
        vif.asi_debug_1_data  <= item.data;
      end
      1: begin
        vif.asi_debug_2_valid <= 1'b1;
        vif.asi_debug_2_data  <= item.data;
      end
      2: begin
        vif.asi_debug_3_valid <= 1'b1;
        vif.asi_debug_3_data  <= item.data;
      end
      3: begin
        vif.asi_debug_4_valid <= 1'b1;
        vif.asi_debug_4_data  <= item.data;
      end
      4: begin
        vif.asi_debug_5_valid <= 1'b1;
        vif.asi_debug_5_data  <= item.data;
      end
      5: begin
        vif.asi_debug_6_valid <= 1'b1;
        vif.asi_debug_6_data  <= item.data;
      end
      default: begin
        `uvm_fatal(get_type_name(), $sformatf("Invalid debug index %0d", item.debug_index))
      end
    endcase
  endtask

  task automatic reset_outputs();
    for (int unsigned idx = 0; idx < N_DEBUG_IFS; idx++) begin
      drive_debug_idle(idx);
    end
  endtask

  task automatic drive_item(hist_dbg_seq_item item);
    @(posedge vif.i_clk);
    drive_debug_item(item);

    @(posedge vif.i_clk);
    drive_debug_idle(item.debug_index);
  endtask
endclass

`endif
