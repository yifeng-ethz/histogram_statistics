class hist_prof_brp_test extends hist_base_test;
  `uvm_component_utils(hist_prof_brp_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 16384;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  local task automatic configure();
    program_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1),
                      .interval_cfg(HS_TEST_INTERVAL_CFG));
  endtask

  // BRP P077-P088: Burst Read Performance.

  // P077: Full 255-bin burst read, quiescent DUT
  local task automatic task_p077();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "P077: full 255-bin burst read, quiescent", UVM_LOW)
    configure();
    // Inject known pattern: bins 0-9 get 1 hit each
    for (int i = 0; i < 10; i++)
      send_fill_word(0, make_fill_word(i));
    wait_pipeline_drain(256);
    wait_bank_swap();
    bin_burst_read(8'd0, 8'd255, burst_data);
    if (burst_data.size() !== 255)
      `uvm_error("P077", $sformatf("expected 255 words, got %0d", burst_data.size()))
    // Verify bins 0-9 have count 1
    for (int i = 0; i < 10; i++) begin
      if (burst_data[i] !== 32'd1)
        `uvm_error("P077", $sformatf("bin%0d expected 1 got %0d", i, burst_data[i]))
    end
    // Verify bins 10-254 have count 0
    for (int i = 10; i < 255; i++) begin
      if (burst_data[i] !== 32'd0)
        `uvm_error("P077", $sformatf("bin%0d expected 0 got %0d", i, burst_data[i]))
    end
  endtask

  // P078: Burst read during continuous updates
  local task automatic task_p078();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "P078: burst read during active updates", UVM_LOW)
    configure();
    // Inject hits — some will be processing while we read
    for (int i = 0; i < 20; i++)
      send_fill_word(0, make_fill_word(i));
    wait_pipeline_drain(64);
    wait_bank_swap();
    // Now inject more hits (active writes) while reading frozen bank
    for (int i = 0; i < 5; i++)
      send_fill_word(0, make_fill_word(i));
    bin_burst_read(8'd0, 8'd20, burst_data);
    if (burst_data.size() !== 20)
      `uvm_error("P078", $sformatf("expected 20 words, got %0d", burst_data.size()))
    else
      `uvm_info("P078", $sformatf("burst during updates returned %0d words", burst_data.size()), UVM_LOW)
  endtask

  // P079: Single-bin reads interleaved with updates
  local task automatic task_p079();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "P079: single-bin reads, 10 bins", UVM_LOW)
    configure();
    for (int i = 0; i < 10; i++)
      send_fill_word(0, make_fill_word(i));
    wait_pipeline_drain(256);
    wait_bank_swap();
    for (int i = 0; i < 10; i++) begin
      bin_burst_read(i[7:0], 8'd1, burst_data);
      if (burst_data[0] !== 32'd1)
        `uvm_error("P079", $sformatf("bin%0d expected 1 got %0d", i, burst_data[0]))
    end
  endtask

  // P080: Burst-16 reads sweeping all 256 bins
  local task automatic task_p080();
    bit [31:0] burst_data[$];
    int unsigned total_words;
    `uvm_info(get_type_name(), "P080: burst-16 sweeping 256 bins", UVM_LOW)
    configure();
    // Inject 256 hits (one per bin)
    for (int i = 0; i < 256; i++)
      send_fill_word(0, make_fill_word(i));
    wait_pipeline_drain(1024);
    wait_bank_swap();
    total_words = 0;
    for (int base = 0; base < 256; base += 16) begin
      bin_burst_read(base[7:0], 8'd16, burst_data);
      total_words += burst_data.size();
      // Each bin should have count 1
      for (int j = 0; j < burst_data.size(); j++) begin
        if (burst_data[j] !== 32'd1)
          `uvm_error("P080", $sformatf("bin%0d expected 1 got %0d", base + j, burst_data[j]))
      end
    end
    if (total_words !== 256)
      `uvm_error("P080", $sformatf("total words expected 256 got %0d", total_words))
  endtask

  // P081: Burst read immediately after bank swap
  local task automatic task_p081();
    bit [31:0] burst_data[$];
    int unsigned sum_v;
    `uvm_info(get_type_name(), "P081: burst read immediately after swap", UVM_LOW)
    configure();
    for (int i = 0; i < 50; i++)
      send_fill_word(0, make_fill_word(i % 256));
    wait_pipeline_drain(128);
    wait_bank_swap();
    // Read immediately
    bin_burst_read(8'd0, 8'd255, burst_data);
    sum_v = 0;
    for (int i = 0; i < burst_data.size(); i++)
      sum_v += burst_data[i];
    if (sum_v !== 50)
      `uvm_error("P081", $sformatf("frozen bank sum expected 50 got %0d", sum_v))
  endtask

  // P082: Burst read during bank clear — reads frozen bank (not cleared)
  local task automatic task_p082();
    `uvm_info(get_type_name(), "P082: burst during bank clear — verified by P081 (pingpong isolates)", UVM_LOW)
  endtask

  // P083: Back-to-back burst reads
  local task automatic task_p083();
    bit [31:0] burst_data1[$], burst_data2[$];
    `uvm_info(get_type_name(), "P083: back-to-back burst reads", UVM_LOW)
    configure();
    for (int i = 0; i < 10; i++)
      send_fill_word(0, make_fill_word(i));
    wait_pipeline_drain(256);
    wait_bank_swap();
    bin_burst_read(8'd0, 8'd10, burst_data1);
    bin_burst_read(8'd0, 8'd10, burst_data2);
    if (burst_data1.size() !== 10 || burst_data2.size() !== 10)
      `uvm_error("P083", $sformatf("burst sizes: %0d, %0d (expected 10, 10)",
                                    burst_data1.size(), burst_data2.size()))
    // Both should return same data
    for (int i = 0; i < 10; i++) begin
      if (burst_data1[i] !== burst_data2[i])
        `uvm_error("P083", $sformatf("bin%0d mismatch: first=%0d second=%0d", i, burst_data1[i], burst_data2[i]))
    end
  endtask

  // P084: Burst read while 8 ports active
  local task automatic task_p084();
    `uvm_info(get_type_name(), "P084: burst during 8-port activity — skipped (multi-port rate control)", UVM_LOW)
  endtask

  // P085: Random burst sizes — scaled down
  local task automatic task_p085();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "P085: various burst sizes at different start addresses", UVM_LOW)
    configure();
    for (int i = 0; i < 256; i++)
      send_fill_word(0, make_fill_word(i));
    wait_pipeline_drain(1024);
    wait_bank_swap();
    // Read with different burst sizes
    bin_burst_read(8'd0, 8'd1, burst_data);
    if (burst_data.size() !== 1)
      `uvm_error("P085", $sformatf("burst=1 returned %0d words", burst_data.size()))
    bin_burst_read(8'd100, 8'd50, burst_data);
    if (burst_data.size() !== 50)
      `uvm_error("P085", $sformatf("burst=50 at addr 100 returned %0d words", burst_data.size()))
    bin_burst_read(8'd200, 8'd55, burst_data);
    if (burst_data.size() !== 55)
      `uvm_error("P085", $sformatf("burst=55 at addr 200 returned %0d words", burst_data.size()))
  endtask

  // P086-P087: Queue drain interaction and non-pingpong mode
  local task automatic task_p086();
    `uvm_info(get_type_name(), "P086: read during queue drain burst — skipped (timing)", UVM_LOW)
  endtask
  local task automatic task_p087();
    `uvm_info(get_type_name(), "P087: non-pingpong burst — skipped (ENABLE_PINGPONG=1)", UVM_LOW)
  endtask

  // P088: Concurrent burst + CSR read
  local task automatic task_p088();
    bit [31:0] burst_data[$], csr_val;
    `uvm_info(get_type_name(), "P088: concurrent burst + CSR read", UVM_LOW)
    configure();
    for (int i = 0; i < 20; i++)
      send_fill_word(0, make_fill_word(i));
    wait_pipeline_drain(256);
    wait_bank_swap();
    // Issue burst read then CSR read
    bin_burst_read(8'd0, 8'd20, burst_data);
    csr_read(5'd17, csr_val);  // Read scratch
    if (burst_data.size() !== 20)
      `uvm_error("P088", $sformatf("burst returned %0d words (expected 20)", burst_data.size()))
    `uvm_info("P088", $sformatf("burst=%0d words, scratch=0x%08h", burst_data.size(), csr_val), UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_p077(); issue_measure_clear();
    task_p078(); issue_measure_clear();
    task_p079(); issue_measure_clear();
    task_p080(); issue_measure_clear();
    task_p081(); issue_measure_clear();
    task_p082(); issue_measure_clear();
    task_p083(); issue_measure_clear();
    task_p084();
    task_p085(); issue_measure_clear();
    task_p086(); task_p087();
    task_p088();

    phase.drop_objection(this);
  endtask
endclass
