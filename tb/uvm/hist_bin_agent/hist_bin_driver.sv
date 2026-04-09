`ifndef HIST_BIN_DRIVER_SV
`define HIST_BIN_DRIVER_SV

import uvm_pkg::*;
`include "uvm_macros.svh"

class hist_bin_driver extends uvm_driver #(hist_bin_seq_item);
  `uvm_component_utils(hist_bin_driver)

  localparam int unsigned MAX_WAIT_CYCLES = 65536;

  virtual hist_bin_if vif;

  function new(string name = "hist_bin_driver", uvm_component parent = null);
    super.new(name, parent);
  endfunction

  function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    if (!uvm_config_db#(virtual hist_bin_if)::get(this, "", "vif", vif)) begin
      `uvm_fatal(get_type_name(), "Missing hist_bin_if in uvm_config_db under key 'vif'")
    end
  endfunction

  task run_phase(uvm_phase phase);
    hist_bin_seq_item req;

    reset_outputs();
    forever begin
      wait_for_reset_release();
      seq_item_port.get_next_item(req);
      if (req == null) begin
        `uvm_error(get_type_name(), "Received null hist_bin_seq_item")
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

  task automatic reset_outputs();
    vif.avs_hist_bin_address    <= '0;
    vif.avs_hist_bin_read       <= 1'b0;
    vif.avs_hist_bin_write      <= 1'b0;
    vif.avs_hist_bin_writedata  <= '0;
    vif.avs_hist_bin_burstcount <= '0;
  endtask

  task automatic wait_for_accept();
    int unsigned cycles;

    cycles = 0;
    while (vif.avs_hist_bin_waitrequest === 1'b1) begin
      @(posedge vif.i_clk);
      cycles++;
      if (cycles > MAX_WAIT_CYCLES) begin
        `uvm_fatal(
          get_type_name(),
          $sformatf("hist_bin waitrequest stuck high for more than %0d cycles", MAX_WAIT_CYCLES)
        )
      end
    end
  endtask

  task automatic wait_for_writeresponse();
    int unsigned cycles;

    cycles = 0;
    forever begin
      #1step;
      if (vif.avs_hist_bin_writeresponsevalid === 1'b1) begin
        break;
      end
      @(posedge vif.i_clk);
      cycles++;
      if (cycles > MAX_WAIT_CYCLES) begin
        `uvm_fatal(
          get_type_name(),
          $sformatf("Timed out waiting for avs_hist_bin_writeresponsevalid after %0d cycles", MAX_WAIT_CYCLES)
        )
      end
    end
  endtask

  task automatic drive_write(hist_bin_seq_item item);
    bit [7:0] burstcount_value;

    burstcount_value = (item.burstcount == 8'h00) ? 8'h01 : item.burstcount;

    @(posedge vif.i_clk);
    vif.avs_hist_bin_address    <= item.address;
    vif.avs_hist_bin_read       <= 1'b0;
    vif.avs_hist_bin_write      <= 1'b1;
    vif.avs_hist_bin_writedata  <= item.writedata;
    vif.avs_hist_bin_burstcount <= burstcount_value;

    @(posedge vif.i_clk);
    wait_for_accept();
    reset_outputs();

    wait_for_writeresponse();
  endtask

  task automatic drive_read(hist_bin_seq_item item);
    int unsigned n_words;
    int unsigned cycles;

    n_words = item.effective_burstcount();
    item.readdata = new[n_words];

    @(posedge vif.i_clk);
    vif.avs_hist_bin_address    <= item.address;
    vif.avs_hist_bin_read       <= 1'b1;
    vif.avs_hist_bin_write      <= 1'b0;
    vif.avs_hist_bin_writedata  <= '0;
    vif.avs_hist_bin_burstcount <= item.burstcount;

    @(posedge vif.i_clk);
    wait_for_accept();
    reset_outputs();

    for (int unsigned idx = 0; idx < n_words; idx++) begin
      cycles = 0;
      forever begin
        @(posedge vif.i_clk);
        #1step;
        if (vif.avs_hist_bin_readdatavalid === 1'b1) begin
          item.readdata[idx] = vif.avs_hist_bin_readdata;
          break;
        end

        cycles++;
        if (cycles > MAX_WAIT_CYCLES) begin
          `uvm_fatal(
            get_type_name(),
            $sformatf(
              "Timed out waiting for hist_bin read data word %0d/%0d after %0d cycles",
              idx + 1,
              n_words,
              MAX_WAIT_CYCLES
            )
          )
        end
      end
    end
  endtask
endclass

`endif
