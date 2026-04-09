`ifndef HIST_CTRL_DRIVER_SV
`define HIST_CTRL_DRIVER_SV

import uvm_pkg::*;
`include "uvm_macros.svh"

class hist_ctrl_driver extends uvm_driver #(hist_ctrl_seq_item);
  `uvm_component_utils(hist_ctrl_driver)

  virtual hist_ctrl_if vif;

  function new(string name = "hist_ctrl_driver", uvm_component parent = null);
    super.new(name, parent);
  endfunction

  function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    if (!uvm_config_db#(virtual hist_ctrl_if)::get(this, "", "vif", vif)) begin
      `uvm_fatal(get_type_name(), "Missing hist_ctrl_if in uvm_config_db under key 'vif'")
    end
  endfunction

  task run_phase(uvm_phase phase);
    hist_ctrl_seq_item req;

    reset_outputs();
    forever begin
      wait_for_reset_release();
      seq_item_port.get_next_item(req);
      if (req == null) begin
        `uvm_error(get_type_name(), "Received null hist_ctrl_seq_item")
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

  task automatic reset_outputs();
    vif.asi_ctrl_valid <= 1'b0;
    vif.asi_ctrl_data  <= '0;
  endtask

  task automatic drive_item(hist_ctrl_seq_item item);
    @(posedge vif.i_clk);
    vif.asi_ctrl_valid <= 1'b1;
    vif.asi_ctrl_data  <= item.data;

    forever begin
      @(posedge vif.i_clk);
      if (vif.asi_ctrl_ready === 1'b1) begin
        break;
      end
    end

    vif.asi_ctrl_valid <= 1'b0;
    vif.asi_ctrl_data  <= '0;
  endtask
endclass

`endif
