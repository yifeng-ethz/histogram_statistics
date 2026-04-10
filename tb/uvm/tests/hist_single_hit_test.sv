class hist_single_hit_test extends hist_base_test;
  `uvm_component_utils(hist_single_hit_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 1024;
  localparam bit [4:0]    HS_CSR_UNDERFLOW_ADDR = 5'd8;
  localparam bit [4:0]    HS_CSR_OVERFLOW_ADDR  = 5'd9;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  local task automatic configure_histogram(
    input int signed   left_bound,
    input int unsigned bin_width,
    input bit          key_unsigned,
    input bit          filter_enable = 1'b0,
    input bit          filter_reject = 1'b0,
    input int unsigned interval_cfg  = HS_TEST_INTERVAL_CFG
  );
    bit [31:0] control_word;

    if (interval_cfg < HS_TEST_INTERVAL_CFG) begin
      `uvm_fatal(get_type_name(), $sformatf("interval_cfg must be >= %0d, got %0d", HS_TEST_INTERVAL_CFG, interval_cfg))
    end

    csr_write(5'd3, $unsigned(left_bound));
    csr_write(5'd5, bin_width[31:0]);
    csr_write(5'd10, interval_cfg[31:0]);

    control_word       = 32'h0000_0001;
    control_word[8]    = key_unsigned;
    control_word[12]   = filter_enable;
    control_word[13]   = filter_reject;
    csr_write(5'd2, control_word);

    repeat (8) @(cfg.probe_vif.mon_cb);
  endtask

  local task automatic send_single_hit_and_wait(
    input int unsigned port_index,
    input int signed   key_value
  );
    logic [HS_AVST_DATA_W-1:0] hit_word;

    hit_word = make_fill_word(key_value);
    send_fill_word(port_index, hit_word);
    wait_pipeline_drain(128);
    wait_bank_swap();
  endtask

  local task automatic check_single_bin(
    input string       case_id,
    input int unsigned expected_bin
  );
    bit [31:0] burst_data[$];
    int unsigned start_bin;
    int unsigned burstcount;

    if (expected_bin == 0) begin
      start_bin  = 0;
      burstcount = 3;
    end else if (expected_bin == (HS_N_BINS - 1)) begin
      start_bin  = HS_N_BINS - 3;
      burstcount = 3;
    end else begin
      start_bin  = expected_bin - 1;
      burstcount = 3;
    end

    bin_burst_read(start_bin[7:0], burstcount[7:0], burst_data);

    if (burst_data.size() != burstcount) begin
      `uvm_error(case_id, $sformatf("expected %0d burst words, got %0d", burstcount, burst_data.size()))
      return;
    end

    for (int idx = 0; idx < burst_data.size(); idx++) begin
      int unsigned bin_idx;
      bit [31:0]   expected_count;

      bin_idx        = start_bin + idx;
      expected_count = (bin_idx == expected_bin) ? 32'd1 : 32'd0;

      if (burst_data[idx] !== expected_count) begin
        `uvm_error(case_id, $sformatf("bin%0d expected %0d got %0d", bin_idx, expected_count, burst_data[idx]))
      end
    end
  endtask

  local task automatic check_csr(
    input string    case_id,
    input bit [4:0] address,
    input bit [31:0] expected_value,
    input string    csr_name
  );
    bit [31:0] csr_data;

    csr_read(address, csr_data);
    if (csr_data !== expected_value) begin
      `uvm_error(case_id, $sformatf("%s expected %0d got %0d", csr_name, expected_value, csr_data))
    end
  endtask

  local task automatic task_b001();
    `uvm_info(get_type_name(), "B001: single hit on port 0 maps to bin 0", UVM_LOW)
    configure_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1));
    send_single_hit_and_wait(.port_index(0), .key_value(0));
    check_single_bin("B001", 0);
  endtask

  local task automatic task_b002();
    `uvm_info(get_type_name(), "B002: single hit on port 0 maps to bin 255", UVM_LOW)
    configure_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1));
    send_single_hit_and_wait(.port_index(0), .key_value(255 * 16));
    check_single_bin("B002", 255);
  endtask

  local task automatic task_b003();
    `uvm_info(get_type_name(), "B003: single hit on port 0 maps to bin 128", UVM_LOW)
    configure_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1));
    send_single_hit_and_wait(.port_index(0), .key_value(128 * 16));
    check_single_bin("B003", 128);
  endtask

  local task automatic task_b004();
    `uvm_info(get_type_name(), "B004: single hit on port 7 with raw key 0 maps to bin 14", UVM_LOW)
    configure_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1));
    send_single_hit_and_wait(.port_index(7), .key_value(0));
    check_single_bin("B004", 14);
  endtask

  local task automatic task_b005();
    `uvm_info(get_type_name(), "B005: key equals left_bound on port 0", UVM_LOW)
    configure_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1));
    send_single_hit_and_wait(.port_index(0), .key_value(0));
    check_single_bin("B005", 0);
    check_csr("B005", HS_CSR_UNDERFLOW_ADDR, 32'd0, "underflow_cnt");
  endtask

  local task automatic task_b006();
    `uvm_info(get_type_name(), "B006: key equals right_bound - 1 on port 0", UVM_LOW)
    configure_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1));
    send_single_hit_and_wait(.port_index(0), .key_value(4095));
    check_single_bin("B006", 255);
    check_csr("B006", HS_CSR_OVERFLOW_ADDR, 32'd0, "overflow_cnt");
  endtask

  local task automatic task_b007();
    `uvm_info(get_type_name(), "B007: default config with key 0 maps to bin 62", UVM_LOW)
    configure_histogram(
      .left_bound   (HS_DEF_LEFT_BOUND),
      .bin_width    (HS_DEF_BIN_WIDTH),
      .key_unsigned (1'b1)
    );
    send_single_hit_and_wait(.port_index(0), .key_value(0));
    check_single_bin("B007", 62);
  endtask

  local task automatic task_b008();
    `uvm_info(get_type_name(), "B008: snoop passthrough check skipped", UVM_LOW)
    // TODO: Check aso_hist_fill_out snoop transaction content against the injected fill word.
  endtask

  local task automatic task_b009();
    `uvm_info(get_type_name(), "B009: flush-timing hit check skipped", UVM_LOW)
    // TODO: Inject a fill word while measure_clear is active and verify it is counted after flush completes.
  endtask

  local task automatic task_b010();
    `uvm_info(get_type_name(), "B010: single hit with bin_width 1 maps to bin 100", UVM_LOW)
    configure_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1));
    send_single_hit_and_wait(.port_index(0), .key_value(100));
    check_single_bin("B010", 100);
  endtask

  local task automatic task_b011();
    `uvm_info(get_type_name(), "B011: signed key extraction maps negative key to bin 31", UVM_LOW)
    configure_histogram(.left_bound(HS_DEF_LEFT_BOUND), .bin_width(16), .key_unsigned(1'b0));
    send_single_hit_and_wait(.port_index(0), .key_value(-500));
    check_single_bin("B011", 31);
  endtask

  local task automatic task_b012();
    `uvm_info(get_type_name(), "B012: port 1 offset adds 32 before binning", UVM_LOW)
    configure_histogram(
      .left_bound   (HS_DEF_LEFT_BOUND),
      .bin_width    (HS_DEF_BIN_WIDTH),
      .key_unsigned (1'b1)
    );
    send_single_hit_and_wait(.port_index(1), .key_value(10));
    check_single_bin("B012", 65);
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);

    wait_reset_release();
    wait_initial_clear();

    task_b001();
    issue_measure_clear();
    task_b002();
    issue_measure_clear();
    task_b003();
    issue_measure_clear();
    task_b004();
    issue_measure_clear();
    task_b005();
    issue_measure_clear();
    task_b006();
    issue_measure_clear();
    task_b007();
    issue_measure_clear();
    task_b008();
    issue_measure_clear();
    task_b009();
    issue_measure_clear();
    task_b010();
    issue_measure_clear();
    task_b011();
    issue_measure_clear();
    task_b012();

    phase.drop_objection(this);
  endtask
endclass
