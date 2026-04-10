class hist_filter_test extends hist_base_test;
  `uvm_component_utils(hist_filter_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG      = 1024;
  localparam bit [4:0]    HS_CSR_CONTROL_ADDR       = 5'd2;
  localparam bit [4:0]    HS_CSR_LEFT_BOUND_ADDR    = 5'd3;
  localparam bit [4:0]    HS_CSR_BIN_WIDTH_ADDR     = 5'd5;
  localparam bit [4:0]    HS_CSR_KEY_FILTER_BITS_ADDR = 5'd6;
  localparam bit [4:0]    HS_CSR_KEY_FILTER_VAL_ADDR  = 5'd7;
  localparam bit [4:0]    HS_CSR_INTERVAL_CFG_ADDR  = 5'd10;
  localparam bit [4:0]    HS_CSR_TOTAL_HITS_ADDR    = 5'd13;

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

  local task automatic check_csr(
    input string      case_id,
    input bit [4:0]   address,
    input bit [31:0]  expected_value,
    input string      csr_name
  );
    bit [31:0] actual_value;

    csr_read(address, actual_value);
    if (actual_value !== expected_value) begin
      `uvm_error(case_id, $sformatf("%s expected %0d got %0d", csr_name, expected_value, actual_value))
    end
  endtask

  local task automatic task_b053();
    `uvm_info(get_type_name(), "B053: filter disabled lets all hits pass", UVM_LOW)
    configure_key_filter();
    configure_histogram(0, 16, 1'b1, 1'b0, 1'b0);
    send_hit(0, 0, 0);
    send_hit(0, 0, 1);
    send_hit(0, 0, 7);
    send_hit(0, 0, 15);
    wait_pipeline_drain(128);
    check_csr("B053", HS_CSR_TOTAL_HITS_ADDR, 32'd4, "total_hits");
    wait_bank_swap();
    check_bin_value("B053", 0, 32'd4);
  endtask

  local task automatic task_b054();
    `uvm_info(get_type_name(), "B054: filter pass mode accepts matching hit", UVM_LOW)
    configure_key_filter(HS_DEF_UPDATE_LO, HS_DEF_UPDATE_HI, HS_DEF_FILTER_LO, HS_DEF_FILTER_HI, 0, 16'h0005);
    configure_histogram(0, 16, 1'b1, 1'b1, 1'b0);
    send_hit(0, 0, 5);
    wait_pipeline_drain(128);
    check_csr("B054", HS_CSR_TOTAL_HITS_ADDR, 32'd1, "total_hits");
    wait_bank_swap();
    check_bin_value("B054", 0, 32'd1);
  endtask

  local task automatic task_b055();
    `uvm_info(get_type_name(), "B055: filter pass mode blocks non-matching hit", UVM_LOW)
    configure_key_filter(HS_DEF_UPDATE_LO, HS_DEF_UPDATE_HI, HS_DEF_FILTER_LO, HS_DEF_FILTER_HI, 0, 16'h0005);
    configure_histogram(0, 16, 1'b1, 1'b1, 1'b0);
    send_hit(0, 0, 3);
    wait_pipeline_drain(128);
    check_csr("B055", HS_CSR_TOTAL_HITS_ADDR, 32'd1, "total_hits");
    wait_bank_swap();
    check_bin_value("B055", 0, 32'd0);
  endtask

  local task automatic task_b056();
    `uvm_info(get_type_name(), "B056: filter reject mode blocks matching hit", UVM_LOW)
    configure_key_filter(HS_DEF_UPDATE_LO, HS_DEF_UPDATE_HI, HS_DEF_FILTER_LO, HS_DEF_FILTER_HI, 0, 16'h0005);
    configure_histogram(0, 16, 1'b1, 1'b1, 1'b1);
    send_hit(0, 0, 5);
    wait_pipeline_drain(128);
    check_csr("B056", HS_CSR_TOTAL_HITS_ADDR, 32'd1, "total_hits");
    wait_bank_swap();
    check_bin_value("B056", 0, 32'd0);
  endtask

  local task automatic task_b057();
    `uvm_info(get_type_name(), "B057: filter reject mode accepts non-matching hit", UVM_LOW)
    configure_key_filter(HS_DEF_UPDATE_LO, HS_DEF_UPDATE_HI, HS_DEF_FILTER_LO, HS_DEF_FILTER_HI, 0, 16'h0005);
    configure_histogram(0, 16, 1'b1, 1'b1, 1'b1);
    send_hit(0, 0, 3);
    wait_pipeline_drain(128);
    check_csr("B057", HS_CSR_TOTAL_HITS_ADDR, 32'd1, "total_hits");
    wait_bank_swap();
    check_bin_value("B057", 0, 32'd1);
  endtask

  local task automatic task_b058();
    `uvm_info(get_type_name(), "B058: skipped, custom filter range programming deferred", UVM_LOW)
  endtask

  local task automatic task_b059();
    `uvm_info(get_type_name(), "B059: filter pass mode keeps only four matching hits", UVM_LOW)
    configure_key_filter(HS_DEF_UPDATE_LO, HS_DEF_UPDATE_HI, HS_DEF_FILTER_LO, HS_DEF_FILTER_HI, 0, 16'h0005);
    configure_histogram(0, 16, 1'b1, 1'b1, 1'b0);
    repeat (4) begin
      send_hit(0, 0, 5);
      send_hit(0, 0, 3);
    end
    wait_pipeline_drain(128);
    check_csr("B059", HS_CSR_TOTAL_HITS_ADDR, 32'd8, "total_hits");
    wait_bank_swap();
    check_bin_value("B059", 0, 32'd4);
  endtask

  local task automatic task_b060();
    `uvm_info(get_type_name(), "B060: filter reject mode keeps only four non-matching hits", UVM_LOW)
    configure_key_filter(HS_DEF_UPDATE_LO, HS_DEF_UPDATE_HI, HS_DEF_FILTER_LO, HS_DEF_FILTER_HI, 0, 16'h0005);
    configure_histogram(0, 16, 1'b1, 1'b1, 1'b1);
    repeat (4) begin
      send_hit(0, 0, 5);
      send_hit(0, 0, 3);
    end
    wait_pipeline_drain(128);
    check_csr("B060", HS_CSR_TOTAL_HITS_ADDR, 32'd8, "total_hits");
    wait_bank_swap();
    check_bin_value("B060", 0, 32'd4);
  endtask

  local task automatic task_b061();
    `uvm_info(get_type_name(), "B061: skipped, FIFO occupancy stress not implemented", UVM_LOW)
  endtask

  local task automatic task_b062();
    `uvm_info(get_type_name(), "B062: filter value zero matches zero field", UVM_LOW)
    configure_key_filter(HS_DEF_UPDATE_LO, HS_DEF_UPDATE_HI, HS_DEF_FILTER_LO, HS_DEF_FILTER_HI, 0, 16'h0000);
    configure_histogram(0, 16, 1'b1, 1'b1, 1'b0);
    send_hit(0, 0, 0);
    wait_pipeline_drain(128);
    check_csr("B062", HS_CSR_TOTAL_HITS_ADDR, 32'd1, "total_hits");
    wait_bank_swap();
    check_bin_value("B062", 0, 32'd1);
  endtask

  local task automatic task_b063();
    `uvm_info(get_type_name(), "B063: skipped, boundary-aligned filter field case deferred", UVM_LOW)
  endtask

  local task automatic task_b064();
    `uvm_info(get_type_name(), "B064: skipped, apply-toggle traffic phasing not implemented", UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);

    wait_reset_release();
    wait_initial_clear();

    task_b053();
    issue_measure_clear();
    task_b054();
    issue_measure_clear();
    task_b055();
    issue_measure_clear();
    task_b056();
    issue_measure_clear();
    task_b057();
    issue_measure_clear();
    task_b058();
    issue_measure_clear();
    task_b059();
    issue_measure_clear();
    task_b060();
    issue_measure_clear();
    task_b061();
    issue_measure_clear();
    task_b062();
    issue_measure_clear();
    task_b063();
    issue_measure_clear();
    task_b064();

    phase.drop_objection(this);
  endtask
endclass
