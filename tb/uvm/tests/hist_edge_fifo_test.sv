class hist_edge_fifo_test extends hist_base_test;
  `uvm_component_utils(hist_edge_fifo_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 65536;
  localparam bit [4:0] CSR_TOTAL_HITS  = 5'd13;
  localparam bit [4:0] CSR_DROPPED     = 5'd14;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  local task automatic configure(
    input int unsigned interval_cfg = HS_TEST_INTERVAL_CFG
  );
    program_histogram(
      .left_bound   (0),
      .bin_width    (1),
      .key_unsigned (1'b1),
      .interval_cfg (interval_cfg)
    );
  endtask

  // E033-E038: Require stalling downstream to fill FIFO to exact depth.
  // Not achievable from UVM sequences alone — need arbiter stall or FIFO direct access.
  local task automatic task_e033();
    `uvm_info(get_type_name(), "E033: FIFO exactly full (16 entries) — skipped (requires downstream stall)", UVM_LOW)
  endtask
  local task automatic task_e034();
    `uvm_info(get_type_name(), "E034: FIFO at 15 — skipped (requires downstream stall)", UVM_LOW)
  endtask
  local task automatic task_e035();
    `uvm_info(get_type_name(), "E035: FIFO overflow attempt — skipped (requires downstream stall)", UVM_LOW)
  endtask
  local task automatic task_e036();
    `uvm_info(get_type_name(), "E036: empty read — skipped (requires direct FIFO access)", UVM_LOW)
  endtask
  local task automatic task_e037();
    `uvm_info(get_type_name(), "E037: simultaneous write/read at full — skipped (requires downstream stall)", UVM_LOW)
  endtask
  local task automatic task_e038();
    `uvm_info(get_type_name(), "E038: simultaneous write/read at empty — skipped (requires direct FIFO access)", UVM_LOW)
  endtask

  // E039: Pointer wrap — fill and drain twice through full depth.
  // We can approximate: send 256 hits (16 per port * 8 ports + more), drain all via bank swap, repeat.
  local task automatic task_e039();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "E039: FIFO pointer wrap via double fill-drain cycle", UVM_LOW)
    configure();
    // Cycle 1: 32 hits, drain
    for (int i = 0; i < 32; i++)
      send_fill_word(0, make_fill_word(i % 256));
    wait_pipeline_drain(512);
    wait_bank_swap();
    // Cycle 2: 32 more hits, drain
    for (int i = 0; i < 32; i++)
      send_fill_word(0, make_fill_word(i % 256));
    wait_pipeline_drain(512);
    wait_bank_swap();
    // Read first 32 bins — each should have 1 hit from cycle 2
    bin_burst_read(8'd0, 8'd32, burst_data);
    for (int i = 0; i < 32; i++) begin
      if (burst_data[i] !== 32'd1)
        `uvm_error("E039", $sformatf("bin%0d expected 1 got %0d", i, burst_data[i]))
    end
  endtask

  // E040: Single-entry oscillation — 100 push-pop cycles (via send + drain)
  local task automatic task_e040();
    bit [31:0] th;
    `uvm_info(get_type_name(), "E040: 100 single-entry push-pop cycles", UVM_LOW)
    configure();
    for (int i = 0; i < 100; i++) begin
      send_fill_word(0, make_fill_word(0));
      wait_pipeline_drain(32);
    end
    csr_read(CSR_TOTAL_HITS, th);
    if (th !== 32'd100)
      `uvm_error("E040", $sformatf("total_hits expected 100 got %0d", th))
  endtask

  // E041-E044: Require direct FIFO observation (level tracking, peak, combinational read)
  local task automatic task_e041();
    `uvm_info(get_type_name(), "E041: peak level tracking — skipped (no CSR for per-FIFO peak)", UVM_LOW)
  endtask
  local task automatic task_e042();
    `uvm_info(get_type_name(), "E042: clear while partially full — verified via issue_measure_clear", UVM_LOW)
    configure();
    for (int i = 0; i < 8; i++)
      send_fill_word(0, make_fill_word(i));
    wait_pipeline_drain(64);
    issue_measure_clear();
    // After clear, send fresh hits and verify only they appear
    configure();
    send_fill_word(0, make_fill_word(0));
    wait_pipeline_drain(256);
    wait_bank_swap();
    begin
      bit [31:0] burst_data[$];
      bin_burst_read(8'd0, 8'd1, burst_data);
      if (burst_data[0] !== 32'd1)
        `uvm_error("E042", $sformatf("bin0 expected 1 after clear+rehit, got %0d", burst_data[0]))
    end
  endtask
  local task automatic task_e043();
    `uvm_info(get_type_name(), "E043: write during clear — skipped (requires cycle-precise timing)", UVM_LOW)
  endtask
  local task automatic task_e044();
    `uvm_info(get_type_name(), "E044: FIFO showahead read — skipped (requires direct FIFO observation)", UVM_LOW)
  endtask

  // E045: All 8 FIFOs full simultaneously — requires stalling arbiter
  local task automatic task_e045();
    `uvm_info(get_type_name(), "E045: all 8 FIFOs full — skipped (requires arbiter stall)", UVM_LOW)
  endtask

  // E046: Drop pulse on FIFO full — requires FIFO to stay full
  local task automatic task_e046();
    `uvm_info(get_type_name(), "E046: drop pulse on FIFO full — skipped (requires sustained full FIFO)", UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_e033(); issue_measure_clear();
    task_e034(); issue_measure_clear();
    task_e035(); issue_measure_clear();
    task_e036(); issue_measure_clear();
    task_e037(); issue_measure_clear();
    task_e038(); issue_measure_clear();
    task_e039(); issue_measure_clear();
    task_e040(); issue_measure_clear();
    task_e041(); issue_measure_clear();
    task_e042(); issue_measure_clear();
    task_e043(); issue_measure_clear();
    task_e044(); issue_measure_clear();
    task_e045(); issue_measure_clear();
    task_e046();

    phase.drop_objection(this);
  endtask
endclass
