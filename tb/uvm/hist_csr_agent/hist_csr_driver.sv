`ifndef HIST_CSR_DRIVER_SV
`define HIST_CSR_DRIVER_SV

import uvm_pkg::*;
`include "uvm_macros.svh"

class hist_csr_driver extends uvm_driver #(hist_csr_seq_item);
  `uvm_component_utils(hist_csr_driver)

  localparam int unsigned MAX_WAIT_CYCLES = 32;

  virtual hist_csr_if vif;

  function new(string name = "hist_csr_driver", uvm_component parent = null);
    super.new(name, parent);
  endfunction

  function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    if (!uvm_config_db#(virtual hist_csr_if)::get(this, "", "vif", vif)) begin
      `uvm_fatal(get_type_name(), "Missing hist_csr_if in uvm_config_db under key 'vif'")
    end
  endfunction

  task run_phase(uvm_phase phase);
    hist_csr_seq_item req;

    reset_outputs();
    forever begin
      wait_for_reset_release();
      seq_item_port.get_next_item(req);
      if (req == null) begin
        `uvm_error(get_type_name(), "Received null hist_csr_seq_item")
        seq_item_port.item_done();
        continue;
      end

      if (req.is_write) begin
        drive_write(req);
      end else begin
        drive_read(req);
      end
      seq_item_port.item_done();
    end
  endtask

  task automatic wait_for_reset_release();
    while (vif.i_rst === 1'b1) begin
      reset_outputs();
      @(posedge vif.i_clk);
    end
  endtask

  task automatic wait_for_accept();
    int unsigned cycles;

    cycles = 0;
    while (vif.avs_csr_waitrequest === 1'b1) begin
      @(posedge vif.i_clk);
      cycles++;
      if (cycles > MAX_WAIT_CYCLES) begin
        `uvm_fatal(
          get_type_name(),
          $sformatf("CSR waitrequest stuck high for more than %0d cycles", MAX_WAIT_CYCLES)
        )
      end
    end
  endtask

  task automatic reset_outputs();
    vif.avs_csr_address   <= '0;
    vif.avs_csr_read      <= 1'b0;
    vif.avs_csr_write     <= 1'b0;
    vif.avs_csr_writedata <= '0;
  endtask

  task automatic drive_write(hist_csr_seq_item item);
    @(posedge vif.i_clk);
    vif.avs_csr_address   <= item.address;
    vif.avs_csr_writedata <= item.writedata;
    vif.avs_csr_write     <= 1'b1;
    vif.avs_csr_read      <= 1'b0;

    @(posedge vif.i_clk);
    wait_for_accept();
    reset_outputs();
  endtask

  task automatic drive_read(hist_csr_seq_item item);
    @(posedge vif.i_clk);
    vif.avs_csr_address   <= item.address;
    vif.avs_csr_writedata <= '0;
    vif.avs_csr_write     <= 1'b0;
    vif.avs_csr_read      <= 1'b1;

    @(posedge vif.i_clk);
    wait_for_accept();
    reset_outputs();

    @(posedge vif.i_clk);
    #1step;
    item.readdata = vif.avs_csr_readdata;
  endtask
endclass

`endif
