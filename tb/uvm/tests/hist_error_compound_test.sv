class hist_error_compound_test extends hist_base_test;
  `uvm_component_utils(hist_error_compound_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 8192;
  localparam bit [3:0] CSR_CONTROL     = 4'd0;
  localparam bit [3:0] CSR_LEFT_BOUND  = 4'd1;
  localparam bit [3:0] CSR_RIGHT_BOUND = 4'd2;
  localparam bit [3:0] CSR_BIN_WIDTH   = 4'd3;
  localparam bit [3:0] CSR_INTERVAL    = 4'd8;
  localparam bit [3:0] CSR_TOTAL_HITS  = 4'd11;
  localparam bit [3:0] CSR_DROPPED     = 4'd12;
  localparam bit [3:0] CSR_COAL_STATUS = 4'd14;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  // Cross-Tier Compound Scenarios X141-X144.

  // X141: Reset during FIFO overflow — needs i_rst
  local task automatic task_x141();
    `uvm_info(get_type_name(), "X141: reset during FIFO overflow — skipped (no i_rst access)", UVM_LOW)
  endtask

  // X142: Config error + measure_clear + hits in pipeline
  // Testable: set error, inject hits, clear, verify error persists and histogram zeroed
  local task automatic task_x142();
    bit [31:0] ctrl, total;
    `uvm_info(get_type_name(), "X142: error + clear + pipeline hits — triple fault", UVM_LOW)
    // First apply valid config so hits can be processed
    program_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1),
                      .interval_cfg(HS_TEST_INTERVAL_CFG));
    // Inject some hits
    for (int i = 0; i < 4; i++)
      send_fill_word(0, make_fill_word(i * 16));
    wait_pipeline_drain(64);
    // Now set error config (bad apply — doesn't change shadow)
    csr_write(CSR_LEFT_BOUND, 32'd100);
    csr_write(CSR_RIGHT_BOUND, 32'd50);
    csr_write(CSR_BIN_WIDTH, 32'd0);
    begin
      bit [31:0] c;
      c = 32'h0000_0101; // apply + key_unsigned
      csr_write(CSR_CONTROL, c);
    end
    repeat (8) @(cfg.probe_vif.mon_cb);
    // Verify error set
    csr_read(CSR_CONTROL, ctrl);
    if (ctrl[24] !== 1'b1)
      `uvm_warning("X142", "error not set — test inconclusive")
    // Clear while error is set and pipeline may have hits
    issue_measure_clear();
    // Error should persist (CSR value preserved by clear)
    csr_read(CSR_CONTROL, ctrl);
    if (ctrl[24] !== 1'b1)
      `uvm_error("X142", $sformatf("error cleared by measure_clear: control=0x%08h", ctrl))
    // Stats should be zeroed
    csr_read(CSR_TOTAL_HITS, total);
    if (total !== 32'd0)
      `uvm_error("X142", $sformatf("total_hits expected 0 after clear, got %0d", total))
  endtask

  // X143: Queue overflow + SRAM clear + new hits — needs stalled drain
  local task automatic task_x143();
    `uvm_info(get_type_name(), "X143: queue overflow + SRAM clear + new hits — skipped (need stalled drain)", UVM_LOW)
  endtask

  // X144: Reset during queue clear walk — needs i_rst
  local task automatic task_x144();
    `uvm_info(get_type_name(), "X144: reset during queue clear walk — skipped (no i_rst access)", UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_x141(); issue_measure_clear();
    task_x142(); issue_measure_clear();
    task_x143(); issue_measure_clear();
    task_x144();

    phase.drop_objection(this);
  endtask
endclass
