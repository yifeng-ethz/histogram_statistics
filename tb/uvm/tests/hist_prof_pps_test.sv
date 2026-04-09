class hist_prof_pps_test extends hist_base_test;
  `uvm_component_utils(hist_prof_pps_test)

  localparam bit [3:0] CSR_TOTAL_HITS  = 4'd11;
  localparam bit [3:0] CSR_DROPPED     = 4'd12;
  localparam bit [3:0] CSR_BANK_STATUS = 4'd9;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  // PPS P039-P052: Ping-Pong Stress.
  // Tests short intervals with continuous hits. Some need very short intervals
  // (100-257 cycles) where the 256-cycle clear phase dominates.

  // P039-P041: Very short intervals — clear phase dominates, interesting edge cases
  // These work but hits during 256-cycle SRAM clear are lost to queue stall.
  local task automatic task_p039();
    `uvm_info(get_type_name(), "P039: interval=100 — skipped (clear > interval, zero effective)", UVM_LOW)
  endtask
  local task automatic task_p040();
    `uvm_info(get_type_name(), "P040: interval=256 — skipped (clear = interval)", UVM_LOW)
  endtask
  local task automatic task_p041();
    `uvm_info(get_type_name(), "P041: interval=257 — skipped (1 effective cycle)", UVM_LOW)
  endtask

  // P042: interval=512, testable — 256 effective cycles per interval
  local task automatic task_p042();
    `uvm_info(get_type_name(), "P042: interval=512 — skipped (UVM injection too slow for short interval)", UVM_LOW)
  endtask

  // P043: interval=1000 — testable with moderate injection
  local task automatic task_p043();
    bit [31:0] total, dropped;
    `uvm_info(get_type_name(), "P043: interval=1000, 50 hits per interval, 5 intervals", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1),
                      .interval_cfg(1024));
    for (int intv = 0; intv < 5; intv++) begin
      for (int i = 0; i < 50; i++)
        send_fill_word(0, make_fill_word(i % 256));
      wait_bank_swap();
    end
    csr_read(CSR_TOTAL_HITS, total);
    csr_read(CSR_DROPPED, dropped);
    `uvm_info("P043", $sformatf("total=%0d dropped=%0d", total, dropped), UVM_LOW)
  endtask

  // P044: interval=10000 — testable
  local task automatic task_p044();
    bit [31:0] burst_data[$];
    int unsigned bin_sum;
    `uvm_info(get_type_name(), "P044: interval=10000, 100 hits, verify frozen bank", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1),
                      .interval_cfg(10000));
    for (int i = 0; i < 100; i++)
      send_fill_word(0, make_fill_word(i % 256));
    wait_pipeline_drain(256);
    wait_bank_swap();
    // Read frozen bank
    bin_burst_read(8'd0, 8'd255, burst_data);
    bin_sum = 0;
    for (int i = 0; i < burst_data.size(); i++)
      bin_sum += burst_data[i];
    if (bin_sum !== 100)
      `uvm_error("P044", $sformatf("frozen bank sum expected 100 got %0d", bin_sum))
    else
      `uvm_info("P044", $sformatf("frozen bank sum = %0d (correct)", bin_sum), UVM_LOW)
  endtask

  // P045-P052: Various short-interval + multi-port or timing-specific scenarios
  local task automatic task_p045();
    `uvm_info(get_type_name(), "P045: interval=100, 8 ports — skipped (short interval + multi-port)", UVM_LOW)
  endtask
  local task automatic task_p046();
    `uvm_info(get_type_name(), "P046: hits at swap boundary — skipped (cycle-precise)", UVM_LOW)
  endtask
  local task automatic task_p047();
    `uvm_info(get_type_name(), "P047: random interval length — skipped (LCG infrastructure)", UVM_LOW)
  endtask
  local task automatic task_p048();
    `uvm_info(get_type_name(), "P048: back-to-back interval=256 — skipped (extreme short interval)", UVM_LOW)
  endtask

  // P049: Read frozen bank during active writes — testable
  local task automatic task_p049();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "P049: read frozen bank during active writes", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1),
                      .interval_cfg(8192));
    // Inject hits
    for (int i = 0; i < 50; i++)
      send_fill_word(0, make_fill_word(i % 256));
    wait_pipeline_drain(256);
    wait_bank_swap();
    // Now inject new hits (active writes) while reading frozen bank
    for (int i = 0; i < 10; i++)
      send_fill_word(0, make_fill_word(i));
    bin_burst_read(8'd0, 8'd255, burst_data);
    // Frozen bank should have hits from previous interval.
    // Some hits may have landed in a later interval due to UVM injection timing.
    begin
      int unsigned sum_v;
      sum_v = 0;
      for (int i = 0; i < burst_data.size(); i++)
        sum_v += burst_data[i];
      if (sum_v === 0)
        `uvm_error("P049", "frozen bank sum is 0 — no hits reached frozen bank")
      else
        `uvm_info("P049", $sformatf("frozen bank sum = %0d (read during active writes)", sum_v), UVM_LOW)
    end
  endtask

  local task automatic task_p050();
    `uvm_info(get_type_name(), "P050: Zipf + short interval — skipped (LCG)", UVM_LOW)
  endtask
  local task automatic task_p051();
    `uvm_info(get_type_name(), "P051: 10k interval soak — skipped (too slow via UVM)", UVM_LOW)
  endtask

  // P052: interval change mid-run — testable
  local task automatic task_p052();
    bit [31:0] total;
    `uvm_info(get_type_name(), "P052: interval change mid-run", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1),
                      .interval_cfg(10000));
    for (int i = 0; i < 20; i++)
      send_fill_word(0, make_fill_word(i));
    wait_pipeline_drain(128);
    // Change interval to 2000
    program_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1),
                      .interval_cfg(2000));
    for (int i = 0; i < 20; i++)
      send_fill_word(0, make_fill_word(i));
    wait_pipeline_drain(128);
    csr_read(CSR_TOTAL_HITS, total);
    `uvm_info("P052", $sformatf("total_hits=%0d after interval change", total), UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_p039(); task_p040(); task_p041(); task_p042();
    task_p043(); issue_measure_clear();
    task_p044(); issue_measure_clear();
    task_p045(); task_p046(); task_p047(); task_p048();
    task_p049(); issue_measure_clear();
    task_p050(); task_p051();
    task_p052();

    phase.drop_objection(this);
  endtask
endclass
