class hist_debug_test extends hist_base_test;
  `uvm_component_utils(hist_debug_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 2048;
  localparam bit [3:0] CSR_CONTROL   = 4'd0;
  localparam bit [3:0] CSR_UNDERFLOW = 4'd6;
  localparam bit [3:0] CSR_OVERFLOW  = 4'd7;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  local task automatic configure_debug_mode(
    input bit [3:0]    mode,
    input int signed   left_bound = 0,
    input int unsigned bin_width  = 16,
    input bit          key_unsigned = 1'b1
  );
    bit [31:0] control_word;
    csr_write(4'd1, $unsigned(left_bound));
    csr_write(4'd3, bin_width[31:0]);
    csr_write(4'd8, HS_TEST_INTERVAL_CFG);
    control_word       = 32'h0000_0001;  // apply
    control_word[7:4]  = mode;
    control_word[8]    = key_unsigned;
    csr_write(CSR_CONTROL, control_word);
    repeat (8) @(cfg.probe_vif.mon_cb);
  endtask

  local task automatic send_debug_hit(
    input int unsigned dbg_idx,
    input bit [15:0]   data_val
  );
    @(cfg.dbg_vifs[dbg_idx].drv_cb);
    cfg.dbg_vifs[dbg_idx].drv_cb.valid <= 1'b1;
    cfg.dbg_vifs[dbg_idx].drv_cb.data  <= data_val;
    @(cfg.dbg_vifs[dbg_idx].drv_cb);
    cfg.dbg_vifs[dbg_idx].drv_cb.valid <= 1'b0;
    cfg.dbg_vifs[dbg_idx].drv_cb.data  <= '0;
  endtask

  local task automatic task_b123();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B123: debug mode -1, debug input 1, signed key=100", UVM_LOW)
    configure_debug_mode(.mode(4'hF));  // mode -1
    send_debug_hit(0, 16'h0064);  // 100
    wait_pipeline_drain(128);
    wait_bank_swap();
    // key=100 signed, lb=0, bw=16 -> bin=6
    bin_burst_read(8'd6, 8'd1, burst_data);
    if (burst_data[0] !== 32'd1)
      `uvm_error("B123", $sformatf("bin6 expected 1 got %0d", burst_data[0]))
  endtask

  local task automatic task_b124();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B124: debug mode -2, debug input 2, unsigned key=200", UVM_LOW)
    configure_debug_mode(.mode(4'hE));  // mode -2
    send_debug_hit(1, 16'h00C8);  // 200
    wait_pipeline_drain(128);
    wait_bank_swap();
    // key=200 unsigned, lb=0, bw=16 -> bin=12
    bin_burst_read(8'd12, 8'd1, burst_data);
    if (burst_data[0] !== 32'd1)
      `uvm_error("B124", $sformatf("bin12 expected 1 got %0d", burst_data[0]))
  endtask

  local task automatic task_b125();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B125: debug mode -6, debug input 6, key=1", UVM_LOW)
    configure_debug_mode(.mode(4'hA));  // mode -6
    send_debug_hit(5, 16'h0001);  // 1
    wait_pipeline_drain(128);
    wait_bank_swap();
    // key=1 unsigned, lb=0, bw=16 -> bin=0
    bin_burst_read(8'd0, 8'd1, burst_data);
    if (burst_data[0] !== 32'd1)
      `uvm_error("B125", $sformatf("bin0 expected 1 got %0d", burst_data[0]))
  endtask

  local task automatic task_b126();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B126: debug mode -1, AVST ports 1-7 ignored", UVM_LOW)
    configure_debug_mode(.mode(4'hF));
    // Send AVST hits on ports 1-7 (should be ignored in debug mode)
    for (int p = 1; p < HS_N_PORTS; p++)
      send_fill_word(p, make_fill_word(0));
    wait_pipeline_drain(256);
    wait_bank_swap();
    // No bins should be updated (no debug hits sent)
    bin_burst_read(8'd0, 8'd4, burst_data);
    for (int i = 0; i < burst_data.size(); i++) begin
      if (burst_data[i] !== 32'd0)
        `uvm_error("B126", $sformatf("bin%0d expected 0 got %0d", i, burst_data[i]))
    end
  endtask

  local task automatic task_b127();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B127: debug mode -1, AVST port 0 ignored", UVM_LOW)
    configure_debug_mode(.mode(4'hF));
    // Send AVST hit on port 0 (should be overridden by debug)
    send_fill_word(0, make_fill_word(0));
    // Do NOT send debug hit
    wait_pipeline_drain(128);
    wait_bank_swap();
    // Bins should be empty
    bin_burst_read(8'd0, 8'd4, burst_data);
    for (int i = 0; i < burst_data.size(); i++) begin
      if (burst_data[i] !== 32'd0)
        `uvm_error("B127", $sformatf("bin%0d expected 0 got %0d", i, burst_data[i]))
    end
  endtask

  local task automatic task_b128();
    bit [31:0] csr_val;
    `uvm_info(get_type_name(), "B128: debug mode -1, negative 16-bit value", UVM_LOW)
    configure_debug_mode(.mode(4'hF));
    send_debug_hit(0, 16'hFFFF);  // -1 signed
    wait_pipeline_drain(256);
    // key=-1, lb=0 -> underflow (read before bank swap to avoid stats reset)
    csr_read(CSR_UNDERFLOW, csr_val);
    if (csr_val < 32'd1)
      `uvm_error("B128", $sformatf("underflow expected >=1 got %0d", csr_val))
  endtask

  local task automatic task_b129();
    bit [31:0] csr_val;
    `uvm_info(get_type_name(), "B129: debug mode -3, unsigned 0xFFFF = 65535", UVM_LOW)
    configure_debug_mode(.mode(4'hD));  // mode -3
    send_debug_hit(2, 16'hFFFF);  // 65535 unsigned
    wait_pipeline_drain(256);
    // key=65535, lb=0, bw=16: 65535/16=4095>255 -> overflow (read before bank swap)
    csr_read(CSR_OVERFLOW, csr_val);
    if (csr_val < 32'd1)
      `uvm_error("B129", $sformatf("overflow expected >=1 got %0d", csr_val))
  endtask

  local task automatic task_b130();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B130: switch from debug back to normal mode", UVM_LOW)
    // Start in debug mode -1
    configure_debug_mode(.mode(4'hF));
    wait_pipeline_drain(32);
    // Switch to normal mode
    program_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    // Now AVST hits should work
    send_fill_word(0, make_fill_word(0));
    wait_pipeline_drain(128);
    wait_bank_swap();
    bin_burst_read(8'd0, 8'd1, burst_data);
    if (burst_data[0] !== 32'd1)
      `uvm_error("B130", $sformatf("bin0 expected 1 got %0d", burst_data[0]))
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_b123(); issue_measure_clear();
    task_b124(); issue_measure_clear();
    task_b125(); issue_measure_clear();
    task_b126(); issue_measure_clear();
    task_b127(); issue_measure_clear();
    task_b128(); issue_measure_clear();
    task_b129(); issue_measure_clear();
    task_b130();

    phase.drop_objection(this);
  endtask
endclass
