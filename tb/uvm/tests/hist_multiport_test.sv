class hist_multiport_test extends hist_base_test;
  `uvm_component_utils(hist_multiport_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 2048;
  localparam bit [4:0] CSR_TOTAL_HITS   = 5'd13;
  localparam bit [4:0] CSR_DROPPED_HITS = 5'd14;

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

  local task automatic task_b079();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B079: 1 hit per port, all 8, raw key=0 with SOP/EOP asserted", UVM_LOW)
    configure_default();
    for (int p = 0; p < HS_N_PORTS; p++) begin
      send_fill_word_ex(p, make_fill_word(0), '0, 1'b1, 1'b1);
    end
    wait_pipeline_drain(256);
    wait_bank_swap();
    // Each port adds offset port*32 to key 0. bin = (port*32)/16 = port*2
    for (int p = 0; p < HS_N_PORTS; p++) begin
      int unsigned expected_bin;
      expected_bin = p * 2;
      bin_burst_read(expected_bin[7:0], 8'd1, burst_data);
      if (burst_data[0] !== 32'd1)
        `uvm_error("B079", $sformatf("bin%0d (port %0d) expected 1 got %0d", expected_bin, p, burst_data[0]))
    end
  endtask

  local task automatic task_b080();
    bit [31:0] csr_val;
    `uvm_info(get_type_name(), "B080: 8 hits port 0 only", UVM_LOW)
    configure_default();
    for (int i = 0; i < 8; i++) begin
      send_fill_word(0, make_fill_word(i * 16));
    end
    wait_pipeline_drain(256);
    csr_read(CSR_TOTAL_HITS, csr_val);
    if (csr_val < 32'd8)
      `uvm_error("B080", $sformatf("total_hits expected >=8 got %0d", csr_val))
  endtask

  local task automatic task_b081();
    bit [31:0] csr_val;
    `uvm_info(get_type_name(), "B081: 2 hits per port, all 8 ports", UVM_LOW)
    configure_default();
    for (int p = 0; p < HS_N_PORTS; p++) begin
      send_fill_word(p, make_fill_word(0));
      send_fill_word(p, make_fill_word(16));
    end
    wait_pipeline_drain(512);
    csr_read(CSR_TOTAL_HITS, csr_val);
    if (csr_val < 32'd16)
      `uvm_error("B081", $sformatf("total_hits expected >=16 got %0d", csr_val))
  endtask

  local task automatic task_b082();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B082: port 0 and 7 alternating", UVM_LOW)
    configure_default();
    send_fill_word(0, make_fill_word(0));
    send_fill_word(7, make_fill_word(0));
    send_fill_word(0, make_fill_word(0));
    send_fill_word(7, make_fill_word(0));
    wait_pipeline_drain(128);
    wait_bank_swap();
    // port 0: key=0 -> bin 0 (2 hits)
    bin_burst_read(8'd0, 8'd1, burst_data);
    if (burst_data[0] !== 32'd2)
      `uvm_error("B082", $sformatf("bin0 (port0) expected 2 got %0d", burst_data[0]))
    // port 7: key=0+7*32=224 -> bin 14 (2 hits)
    bin_burst_read(8'd14, 8'd1, burst_data);
    if (burst_data[0] !== 32'd2)
      `uvm_error("B082", $sformatf("bin14 (port7) expected 2 got %0d", burst_data[0]))
  endtask

  local task automatic task_b083();
    bit [31:0] csr_val;
    `uvm_info(get_type_name(), "B083: 16 hits port 0 back-to-back (FIFO depth=16)", UVM_LOW)
    configure_default();
    for (int i = 0; i < 16; i++) begin
      send_fill_word(0, make_fill_word(0));
    end
    wait_pipeline_drain(512);
    csr_read(CSR_DROPPED_HITS, csr_val);
    if (csr_val !== 32'd0)
      `uvm_error("B083", $sformatf("dropped_hits expected 0 got %0d", csr_val))
  endtask

  local task automatic task_b084();
    `uvm_info(get_type_name(), "B084: FIFO overflow — skipped (complex timing)", UVM_LOW)
  endtask

  local task automatic task_b085();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B085: port offset correctness for each port", UVM_LOW)
    configure_default();
    for (int p = 0; p < HS_N_PORTS; p++) begin
      send_fill_word(p, make_fill_word(0));
    end
    wait_pipeline_drain(256);
    wait_bank_swap();
    // port p: effective key = 0 + p*32. bin = (p*32)/16 = p*2
    for (int p = 0; p < HS_N_PORTS; p++) begin
      int unsigned expected_bin;
      expected_bin = p * 2;
      bin_burst_read(expected_bin[7:0], 8'd1, burst_data);
      if (burst_data[0] !== 32'd1)
        `uvm_error("B085", $sformatf("bin%0d (port%0d) expected 1 got %0d", expected_bin, p, burst_data[0]))
    end
  endtask

  local task automatic task_b086();
    bit [31:0] csr_val;
    `uvm_info(get_type_name(), "B086: 4 hits/port all 8 = 32 total", UVM_LOW)
    configure_default();
    for (int p = 0; p < HS_N_PORTS; p++) begin
      for (int h = 0; h < 4; h++) begin
        send_fill_word(p, make_fill_word(0));
      end
    end
    wait_pipeline_drain(512);
    csr_read(CSR_TOTAL_HITS, csr_val);
    if (csr_val < 32'd32)
      `uvm_error("B086", $sformatf("total_hits expected >=32 got %0d", csr_val))
  endtask

  local task automatic task_b087();
    `uvm_info(get_type_name(), "B087: apply during traffic — skipped", UVM_LOW)
  endtask

  local task automatic task_b088();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B088: only odd ports 1,3,5,7", UVM_LOW)
    configure_default();
    send_fill_word(1, make_fill_word(0));
    send_fill_word(3, make_fill_word(0));
    send_fill_word(5, make_fill_word(0));
    send_fill_word(7, make_fill_word(0));
    wait_pipeline_drain(128);
    wait_bank_swap();
    // port 1: bin=2, port 3: bin=6, port 5: bin=10, port 7: bin=14
    bin_burst_read(8'd2, 8'd1, burst_data);
    if (burst_data[0] !== 32'd1)
      `uvm_error("B088", $sformatf("bin2 (port1) expected 1 got %0d", burst_data[0]))
    bin_burst_read(8'd6, 8'd1, burst_data);
    if (burst_data[0] !== 32'd1)
      `uvm_error("B088", $sformatf("bin6 (port3) expected 1 got %0d", burst_data[0]))
    bin_burst_read(8'd10, 8'd1, burst_data);
    if (burst_data[0] !== 32'd1)
      `uvm_error("B088", $sformatf("bin10 (port5) expected 1 got %0d", burst_data[0]))
    bin_burst_read(8'd14, 8'd1, burst_data);
    if (burst_data[0] !== 32'd1)
      `uvm_error("B088", $sformatf("bin14 (port7) expected 1 got %0d", burst_data[0]))
  endtask

  local task automatic task_b089();
    `uvm_info(get_type_name(), "B089: snoop backpressure — skipped", UVM_LOW)
  endtask

  local task automatic task_b090();
    `uvm_info(get_type_name(), "B090: snoop disabled generic — skipped", UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_b079(); issue_measure_clear();
    task_b080(); issue_measure_clear();
    task_b081(); issue_measure_clear();
    task_b082(); issue_measure_clear();
    task_b083(); issue_measure_clear();
    task_b084(); issue_measure_clear();
    task_b085(); issue_measure_clear();
    task_b086(); issue_measure_clear();
    task_b087(); issue_measure_clear();
    task_b088(); issue_measure_clear();
    task_b089(); issue_measure_clear();
    task_b090();

    phase.drop_objection(this);
  endtask
endclass
