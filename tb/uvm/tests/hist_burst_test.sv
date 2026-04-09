class hist_burst_test extends hist_base_test;
  `uvm_component_utils(hist_burst_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 4096;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  local task automatic configure_default();
    program_histogram(
      .left_bound   (0),
      .bin_width    (16),
      .key_unsigned (1'b1),
      .interval_cfg (HS_TEST_INTERVAL_CFG)
    );
  endtask

  local task automatic task_b143();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B143: single-word read burstcount=1 of bin 0", UVM_LOW)
    configure_default();
    send_fill_word(0, make_fill_word(0));
    send_fill_word(0, make_fill_word(1));
    send_fill_word(0, make_fill_word(2));
    wait_pipeline_drain(128);
    wait_bank_swap();
    bin_burst_read(8'd0, 8'd1, burst_data);
    if (burst_data.size() != 1)
      `uvm_error("B143", $sformatf("expected 1 word, got %0d", burst_data.size()))
    else if (burst_data[0] !== 32'd3)
      `uvm_error("B143", $sformatf("bin0 expected 3 got %0d", burst_data[0]))
  endtask

  local task automatic task_b144();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B144: burst read 16 consecutive bins", UVM_LOW)
    configure_default();
    for (int i = 0; i < 16; i++)
      send_fill_word(0, make_fill_word(i * 16));
    wait_pipeline_drain(256);
    wait_bank_swap();
    bin_burst_read(8'd0, 8'd16, burst_data);
    if (burst_data.size() != 16) begin
      `uvm_error("B144", $sformatf("expected 16 words, got %0d", burst_data.size()))
    end else begin
      for (int i = 0; i < 16; i++) begin
        if (burst_data[i] !== 32'd1)
          `uvm_error("B144", $sformatf("bin%0d expected 1 got %0d", i, burst_data[i]))
      end
    end
  endtask

  local task automatic task_b145();
    `uvm_info(get_type_name(), "B145: full 256-bin burst — skipped (long runtime)", UVM_LOW)
  endtask

  local task automatic task_b146();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B146: burst read with start address 100", UVM_LOW)
    configure_default();
    // Bins 100-103: keys = {1600, 1616, 1632, 1648}
    send_fill_word(0, make_fill_word(1600));
    send_fill_word(0, make_fill_word(1616));
    send_fill_word(0, make_fill_word(1632));
    send_fill_word(0, make_fill_word(1648));
    wait_pipeline_drain(128);
    wait_bank_swap();
    bin_burst_read(8'd100, 8'd4, burst_data);
    if (burst_data.size() != 4) begin
      `uvm_error("B146", $sformatf("expected 4 words, got %0d", burst_data.size()))
    end else begin
      for (int i = 0; i < 4; i++) begin
        if (burst_data[i] !== 32'd1)
          `uvm_error("B146", $sformatf("bin%0d expected 1 got %0d", 100 + i, burst_data[i]))
      end
    end
  endtask

  local task automatic task_b147();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B147: measure_clear via hist_bin write clears all", UVM_LOW)
    configure_default();
    for (int i = 0; i < 4; i++)
      send_fill_word(0, make_fill_word(i * 16));
    wait_pipeline_drain(128);
    issue_measure_clear();
    bin_burst_read(8'd0, 8'd4, burst_data);
    for (int i = 0; i < burst_data.size(); i++) begin
      if (burst_data[i] !== 32'd0)
        `uvm_error("B147", $sformatf("bin%0d expected 0 got %0d", i, burst_data[i]))
    end
  endtask

  local task automatic task_b148();
    `uvm_info(get_type_name(), "B148: concurrent read + update — skipped", UVM_LOW)
  endtask

  local task automatic task_b149();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B149: burstcount=0 treated as 1", UVM_LOW)
    configure_default();
    wait_pipeline_drain(64);
    wait_bank_swap();
    // bin_burst_read with burstcount=0; driver maps to 1
    bin_burst_read(8'd0, 8'd0, burst_data);
    if (burst_data.size() < 1)
      `uvm_error("B149", $sformatf("expected >=1 word with burstcount=0, got %0d", burst_data.size()))
  endtask

  local task automatic task_b150();
    bit [31:0] burst_data1[$];
    bit [31:0] burst_data2[$];
    `uvm_info(get_type_name(), "B150: back-to-back burst reads", UVM_LOW)
    configure_default();
    for (int i = 0; i < 8; i++)
      send_fill_word(0, make_fill_word(i * 16));
    wait_pipeline_drain(128);
    wait_bank_swap();
    bin_burst_read(8'd0, 8'd4, burst_data1);
    bin_burst_read(8'd4, 8'd4, burst_data2);
    if (burst_data1.size() != 4)
      `uvm_error("B150", $sformatf("first burst expected 4 got %0d", burst_data1.size()))
    if (burst_data2.size() != 4)
      `uvm_error("B150", $sformatf("second burst expected 4 got %0d", burst_data2.size()))
    for (int i = 0; i < 4; i++) begin
      if (burst_data1[i] !== 32'd1)
        `uvm_error("B150", $sformatf("bin%0d expected 1 got %0d", i, burst_data1[i]))
      if (burst_data2[i] !== 32'd1)
        `uvm_error("B150", $sformatf("bin%0d expected 1 got %0d", i + 4, burst_data2[i]))
    end
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_b143(); issue_measure_clear();
    task_b144(); issue_measure_clear();
    task_b145(); issue_measure_clear();
    task_b146(); issue_measure_clear();
    task_b147(); issue_measure_clear();
    task_b148(); issue_measure_clear();
    task_b149(); issue_measure_clear();
    task_b150();

    phase.drop_objection(this);
  endtask
endclass
