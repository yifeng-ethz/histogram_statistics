class hist_smoke_test extends hist_base_test;
  `uvm_component_utils(hist_smoke_test)

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  task run_phase(uvm_phase phase);
    bit [31:0] burst_data[$];
    logic [HS_AVST_DATA_W-1:0] hit_word;

    phase.raise_objection(this);

    wait_reset_release();
    wait_initial_clear();

    // B001: one fill hit, then burst-read the frozen bank after the first interval.
    program_histogram(
      .left_bound   (0),
      .bin_width    (16),
      .key_unsigned (1'b1),
      .interval_cfg (1024)
    );

    // Reset the timer by issuing measure_clear — timer_count already past
    // new interval (64) because it counted through the initial clear phase.
    issue_measure_clear();

    hit_word = make_fill_word(0);
    send_fill_word(0, hit_word);

    wait_pipeline_drain(128);
    wait_bank_swap();
    bin_burst_read(8'd0, 8'd4, burst_data);

    if (burst_data.size() != 4) begin
      `uvm_error(get_type_name(), $sformatf("expected 4 burst words, got %0d", burst_data.size()))
    end else begin
      if (burst_data[0] !== 32'd1) begin
        `uvm_error(get_type_name(), $sformatf("bin0 mismatch dut=%0d ref=1", burst_data[0]))
      end
      for (int idx = 1; idx < burst_data.size(); idx++) begin
        if (burst_data[idx] !== 32'd0) begin
          `uvm_error(get_type_name(), $sformatf("bin%0d expected 0 got %0d", idx, burst_data[idx]))
        end
      end
    end

    phase.drop_objection(this);
  endtask
endclass
