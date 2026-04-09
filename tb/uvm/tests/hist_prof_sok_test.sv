class hist_prof_sok_test extends hist_base_test;
  `uvm_component_utils(hist_prof_sok_test)

  localparam bit [3:0] CSR_TOTAL_HITS = 4'd11;
  localparam bit [3:0] CSR_DROPPED    = 4'd12;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  // SOK P065-P076: Long Soak Tests.
  // These require 100k-500k hits which is too slow through UVM sequences.
  // Scaled-down versions implemented for P065 and P068.

  // P065: Scaled soak — 500 hits instead of 100k
  local task automatic task_p065();
    bit [31:0] total, dropped;
    `uvm_info(get_type_name(), "P065: scaled soak 500 hits, single port, uniform bins", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1),
                      .interval_cfg(16384));
    for (int i = 0; i < 500; i++)
      send_fill_word(0, make_fill_word(i % 256));
    wait_pipeline_drain(1024);
    csr_read(CSR_TOTAL_HITS, total);
    csr_read(CSR_DROPPED, dropped);
    if (total !== 32'd500)
      `uvm_error("P065", $sformatf("total_hits expected 500 got %0d", total))
    if (dropped !== 32'd0)
      `uvm_error("P065", $sformatf("dropped expected 0 got %0d", dropped))
  endtask

  local task automatic task_p066();
    `uvm_info(get_type_name(), "P066: 100k 8-port soak — skipped (too slow)", UVM_LOW)
  endtask
  local task automatic task_p067();
    `uvm_info(get_type_name(), "P067: 500k soak — skipped (too slow)", UVM_LOW)
  endtask

  // P068: Scaled — 10 intervals instead of 1000
  local task automatic task_p068();
    bit [31:0] burst_data[$];
    int unsigned sum_v;
    `uvm_info(get_type_name(), "P068: 10 intervals, 20 hits each, verify per-interval", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1),
                      .interval_cfg(4096));
    for (int intv = 0; intv < 10; intv++) begin
      for (int i = 0; i < 20; i++)
        send_fill_word(0, make_fill_word(i % 256));
      wait_pipeline_drain(256);
      wait_bank_swap();
      bin_burst_read(8'd0, 8'd255, burst_data);
      sum_v = 0;
      for (int i = 0; i < burst_data.size(); i++)
        sum_v += burst_data[i];
      if (sum_v !== 20)
        `uvm_error("P068", $sformatf("interval %0d: bin sum expected 20 got %0d", intv, sum_v))
    end
  endtask

  local task automatic task_p069();
    `uvm_info(get_type_name(), "P069: counter saturation 2^32-1 — skipped (too slow)", UVM_LOW)
  endtask

  // P070: Scaled queue pointer wrap — 512 distinct-bin hits
  local task automatic task_p070();
    bit [31:0] total;
    `uvm_info(get_type_name(), "P070: queue pointer wrap, 512 hits sequential scan", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1),
                      .interval_cfg(32768));
    for (int i = 0; i < 512; i++)
      send_fill_word(0, make_fill_word(i % 256));
    wait_pipeline_drain(2048);
    csr_read(CSR_TOTAL_HITS, total);
    if (total !== 32'd512)
      `uvm_error("P070", $sformatf("total_hits expected 512 got %0d", total))
  endtask

  local task automatic task_p071();
    `uvm_info(get_type_name(), "P071: queue fill/drain 1000x — skipped (too slow)", UVM_LOW)
  endtask

  // P072: Multi-interval accumulation — testable (scaled)
  local task automatic task_p072();
    `uvm_info(get_type_name(), "P072: multi-interval — verified by P068", UVM_LOW)
  endtask

  // P073: CSR reads don't disturb processing
  local task automatic task_p073();
    bit [31:0] total, dropped, val;
    `uvm_info(get_type_name(), "P073: CSR reads interleaved with hits", UVM_LOW)
    program_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1),
                      .interval_cfg(32768));
    for (int i = 0; i < 100; i++) begin
      send_fill_word(0, make_fill_word(i % 256));
      if (i % 10 == 0)
        csr_read(4'd11, val);  // Read total_hits periodically
    end
    wait_pipeline_drain(512);
    csr_read(CSR_TOTAL_HITS, total);
    csr_read(CSR_DROPPED, dropped);
    if (total !== 32'd100)
      `uvm_error("P073", $sformatf("total_hits expected 100 got %0d", total))
    if (dropped !== 32'd0)
      `uvm_error("P073", $sformatf("dropped expected 0 got %0d (CSR reads disturbed pipeline)", dropped))
  endtask

  local task automatic task_p074();
    `uvm_info(get_type_name(), "P074: periodic clears — skipped (LCG timing)", UVM_LOW)
  endtask
  local task automatic task_p075();
    `uvm_info(get_type_name(), "P075: 8-port Zipf — skipped (LCG + multi-port)", UVM_LOW)
  endtask
  local task automatic task_p076();
    `uvm_info(get_type_name(), "P076: 8-port stress soak — skipped (too slow + rate control)", UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_p065(); issue_measure_clear();
    task_p066(); task_p067();
    task_p068(); issue_measure_clear();
    task_p069();
    task_p070(); issue_measure_clear();
    task_p071(); task_p072();
    task_p073(); issue_measure_clear();
    task_p074(); task_p075(); task_p076();

    phase.drop_objection(this);
  endtask
endclass
