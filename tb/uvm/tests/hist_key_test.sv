class hist_key_test extends hist_base_test;
  `uvm_component_utils(hist_key_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG      = 1024;
  localparam bit [3:0]    HS_CSR_CONTROL_ADDR       = 4'd0;
  localparam bit [3:0]    HS_CSR_LEFT_BOUND_ADDR    = 4'd1;
  localparam bit [3:0]    HS_CSR_BIN_WIDTH_ADDR     = 4'd3;
  localparam bit [3:0]    HS_CSR_KEY_FILTER_BITS_ADDR = 4'd4;
  localparam bit [3:0]    HS_CSR_KEY_FILTER_VAL_ADDR  = 4'd5;
  localparam bit [3:0]    HS_CSR_UNDERFLOW_CNT_ADDR = 4'd6;
  localparam bit [3:0]    HS_CSR_OVERFLOW_CNT_ADDR  = 4'd7;
  localparam bit [3:0]    HS_CSR_INTERVAL_CFG_ADDR  = 4'd8;
  localparam bit [3:0]    HS_CSR_TOTAL_HITS_ADDR    = 4'd11;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  local task automatic configure_key_filter(
    input int unsigned update_lo = HS_DEF_UPDATE_LO,
    input int unsigned update_hi = HS_DEF_UPDATE_HI,
    input int unsigned filter_lo = HS_DEF_FILTER_LO,
    input int unsigned filter_hi = HS_DEF_FILTER_HI,
    input int unsigned update_key = 0,
    input int unsigned filter_key = 0
  );
    bit [31:0] bits_word;
    bit [31:0] value_word;

    bits_word            = '0;
    bits_word[7:0]       = update_lo[7:0];
    bits_word[15:8]      = update_hi[7:0];
    bits_word[23:16]     = filter_lo[7:0];
    bits_word[31:24]     = filter_hi[7:0];
    value_word           = '0;
    value_word[15:0]     = update_key[15:0];
    value_word[31:16]    = filter_key[15:0];

    csr_write(HS_CSR_KEY_FILTER_BITS_ADDR, bits_word);
    csr_write(HS_CSR_KEY_FILTER_VAL_ADDR, value_word);
  endtask

  local task automatic write_control_apply(
    input bit key_unsigned  = 1'b1,
    input bit filter_enable = 1'b0,
    input bit filter_reject = 1'b0
  );
    bit [31:0] control_word;

    control_word       = '0;
    control_word[0]    = 1'b1;
    control_word[8]    = key_unsigned;
    control_word[12]   = filter_enable;
    control_word[13]   = filter_reject;
    csr_write(HS_CSR_CONTROL_ADDR, control_word);
  endtask

  local task automatic configure_histogram(
    input int signed   left_bound,
    input int unsigned bin_width,
    input bit          key_unsigned,
    input bit          filter_enable = 1'b0,
    input bit          filter_reject = 1'b0,
    input int unsigned interval_cfg  = HS_TEST_INTERVAL_CFG
  );
    if (interval_cfg < HS_TEST_INTERVAL_CFG) begin
      `uvm_fatal(get_type_name(), $sformatf("interval_cfg must be >= %0d, got %0d", HS_TEST_INTERVAL_CFG, interval_cfg))
    end

    csr_write(HS_CSR_LEFT_BOUND_ADDR, $unsigned(left_bound));
    csr_write(HS_CSR_BIN_WIDTH_ADDR, bin_width[31:0]);
    csr_write(HS_CSR_INTERVAL_CFG_ADDR, interval_cfg[31:0]);
    write_control_apply(key_unsigned, filter_enable, filter_reject);
    repeat (8) @(cfg.probe_vif.mon_cb);
  endtask

  local task automatic send_hit(
    input int unsigned port_index,
    input int signed   key_value,
    input int unsigned filter_value = 0,
    input int unsigned update_lo = HS_DEF_UPDATE_LO,
    input int unsigned update_hi = HS_DEF_UPDATE_HI,
    input int unsigned filter_lo = HS_DEF_FILTER_LO,
    input int unsigned filter_hi = HS_DEF_FILTER_HI
  );
    logic [HS_AVST_DATA_W-1:0] hit_word;

    hit_word = make_fill_word(
      .key_value    (key_value),
      .update_lo    (update_lo),
      .update_hi    (update_hi),
      .filter_value (filter_value),
      .filter_lo    (filter_lo),
      .filter_hi    (filter_hi)
    );
    send_fill_word(port_index, hit_word);
  endtask

  local task automatic check_bin_value(
    input string       case_id,
    input int unsigned bin_index,
    input bit [31:0]   expected_value
  );
    bit [31:0] burst_data[$];

    bin_burst_read(bin_index[7:0], 8'd1, burst_data);
    if (burst_data.size() != 1) begin
      `uvm_error(case_id, $sformatf("expected 1 burst word, got %0d", burst_data.size()))
      return;
    end

    if (burst_data[0] !== expected_value) begin
      `uvm_error(case_id, $sformatf("bin%0d expected %0d got %0d", bin_index, expected_value, burst_data[0]))
    end
  endtask

  local task automatic check_all_bins_zero(input string case_id);
    bit [31:0] burst_data[$];

    for (int start_bin = 0; start_bin < HS_N_BINS; start_bin += 16) begin
      bin_burst_read(start_bin[7:0], 8'd16, burst_data);
      if (burst_data.size() != 16) begin
        `uvm_error(case_id, $sformatf("bin burst at %0d expected 16 words, got %0d", start_bin, burst_data.size()))
        return;
      end
      for (int idx = 0; idx < burst_data.size(); idx++) begin
        if (burst_data[idx] !== 32'd0) begin
          `uvm_error(case_id, $sformatf("bin%0d expected 0 got %0d", start_bin + idx, burst_data[idx]))
        end
      end
    end
  endtask

  local task automatic check_csr(
    input string      case_id,
    input bit [3:0]   address,
    input bit [31:0]  expected_value,
    input string      csr_name
  );
    bit [31:0] actual_value;

    csr_read(address, actual_value);
    if (actual_value !== expected_value) begin
      `uvm_error(case_id, $sformatf("%s expected %0d got %0d", csr_name, expected_value, actual_value))
    end
  endtask

  local task automatic task_b041();
    `uvm_info(get_type_name(), "B041: unsigned key data[29:17]=256 maps to bin 78", UVM_LOW)
    configure_key_filter();
    configure_histogram(HS_DEF_LEFT_BOUND, HS_DEF_BIN_WIDTH, 1'b1);
    send_hit(0, 256);
    wait_pipeline_drain(128);
    wait_bank_swap();
    check_bin_value("B041", 78, 32'd1);
  endtask

  local task automatic task_b042();
    `uvm_info(get_type_name(), "B042: unsigned max 13-bit key overflows", UVM_LOW)
    configure_key_filter();
    configure_histogram(HS_DEF_LEFT_BOUND, HS_DEF_BIN_WIDTH, 1'b1);
    send_hit(0, 8191);
    wait_pipeline_drain(128);
    check_csr("B042", HS_CSR_OVERFLOW_CNT_ADDR, 32'd1, "overflow_cnt");
  endtask

  local task automatic task_b043();
    `uvm_info(get_type_name(), "B043: signed key -1 maps to bin 62", UVM_LOW)
    configure_key_filter();
    configure_histogram(HS_DEF_LEFT_BOUND, HS_DEF_BIN_WIDTH, 1'b0);
    send_hit(0, -1);
    wait_pipeline_drain(128);
    wait_bank_swap();
    check_bin_value("B043", 62, 32'd1);
  endtask

  local task automatic task_b044();
    `uvm_info(get_type_name(), "B044: signed key -4096 underflows", UVM_LOW)
    configure_key_filter();
    configure_histogram(HS_DEF_LEFT_BOUND, HS_DEF_BIN_WIDTH, 1'b0);
    send_hit(0, -4096);
    wait_pipeline_drain(128);
    check_csr("B044", HS_CSR_UNDERFLOW_CNT_ADDR, 32'd1, "underflow_cnt");
  endtask

  local task automatic task_b045();
    `uvm_info(get_type_name(), "B045: custom key bits [7:0] map 0xAB to bin 171", UVM_LOW)
    configure_key_filter(0, 7);
    configure_histogram(0, 1, 1'b1);
    send_hit(0, 171, 0, 0, 7);
    wait_pipeline_drain(128);
    wait_bank_swap();
    check_bin_value("B045", 171, 32'd1);
  endtask

  local task automatic task_b046();
    `uvm_info(get_type_name(), "B046: custom key bits [38:35] map top nibble to bin 15", UVM_LOW)
    configure_key_filter(35, 38, 35, 38);
    configure_histogram(0, 1, 1'b1);
    send_hit(0, 15, 15, 35, 38, 35, 38);
    wait_pipeline_drain(128);
    wait_bank_swap();
    check_bin_value("B046", 15, 32'd1);
  endtask

  local task automatic task_b047();
    `uvm_info(get_type_name(), "B047: single-bit key extraction maps bit 20 to bin 1", UVM_LOW)
    configure_key_filter(20, 20);
    configure_histogram(0, 1, 1'b1);
    send_hit(0, 1, 0, 20, 20);
    wait_pipeline_drain(128);
    wait_bank_swap();
    check_bin_value("B047", 1, 32'd1);
  endtask

  local task automatic task_b048();
    `uvm_info(get_type_name(), "B048: unsigned key plus port 3 offset maps to bin 71", UVM_LOW)
    configure_key_filter();
    configure_histogram(HS_DEF_LEFT_BOUND, HS_DEF_BIN_WIDTH, 1'b1);
    send_hit(3, 50);
    wait_pipeline_drain(128);
    wait_bank_swap();
    check_bin_value("B048", 71, 32'd1);
  endtask

  local task automatic task_b049();
    `uvm_info(get_type_name(), "B049: signed key plus port 2 offset maps to bin 65", UVM_LOW)
    configure_key_filter();
    configure_histogram(HS_DEF_LEFT_BOUND, HS_DEF_BIN_WIDTH, 1'b0);
    send_hit(2, -10);
    wait_pipeline_drain(128);
    wait_bank_swap();
    check_bin_value("B049", 65, 32'd1);
  endtask

  local task automatic task_b050();
    `uvm_info(get_type_name(), "B050: skipped, LCG PRNG support not available yet", UVM_LOW)
  endtask

  local task automatic task_b051();
    `uvm_info(get_type_name(), "B051: skipped, LCG PRNG support not available yet", UVM_LOW)
  endtask

  local task automatic task_b052();
    `uvm_info(get_type_name(), "B052: filter mismatch blocks bin update but total_hits still increments", UVM_LOW)
    configure_key_filter(HS_DEF_UPDATE_LO, HS_DEF_UPDATE_HI, HS_DEF_FILTER_LO, HS_DEF_FILTER_HI, 0, 16'h0005);
    configure_histogram(HS_DEF_LEFT_BOUND, HS_DEF_BIN_WIDTH, 1'b1, 1'b1, 1'b0);
    send_hit(0, 0, 3);
    wait_pipeline_drain(128);
    check_csr("B052", HS_CSR_TOTAL_HITS_ADDR, 32'd1, "total_hits");
    wait_bank_swap();
    check_all_bins_zero("B052");
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);

    wait_reset_release();
    wait_initial_clear();

    task_b041();
    issue_measure_clear();
    task_b042();
    issue_measure_clear();
    task_b043();
    issue_measure_clear();
    task_b044();
    issue_measure_clear();
    task_b045();
    issue_measure_clear();
    task_b046();
    issue_measure_clear();
    task_b047();
    issue_measure_clear();
    task_b048();
    issue_measure_clear();
    task_b049();
    issue_measure_clear();
    task_b050();
    issue_measure_clear();
    task_b051();
    issue_measure_clear();
    task_b052();

    phase.drop_objection(this);
  endtask
endclass
