class hist_edge_filter_test extends hist_base_test;
  `uvm_component_utils(hist_edge_filter_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 8192;
  localparam bit [4:0] CSR_CONTROL     = 5'd2;
  localparam bit [4:0] CSR_LEFT_BOUND  = 5'd3;
  localparam bit [4:0] CSR_BIN_WIDTH   = 5'd5;
  localparam bit [4:0] CSR_KEY_BITS    = 5'd6;
  localparam bit [4:0] CSR_KEY_VAL     = 5'd7;
  localparam bit [4:0] CSR_INTERVAL    = 5'd10;
  localparam bit [4:0] CSR_TOTAL_HITS  = 5'd13;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  local task automatic configure_key_filter(
    input int unsigned update_lo  = HS_DEF_UPDATE_LO,
    input int unsigned update_hi  = HS_DEF_UPDATE_HI,
    input int unsigned filter_lo  = HS_DEF_FILTER_LO,
    input int unsigned filter_hi  = HS_DEF_FILTER_HI,
    input int unsigned update_key = 0,
    input int unsigned filter_key = 0
  );
    bit [31:0] bits_word, value_word;
    bits_word            = '0;
    bits_word[7:0]       = update_lo[7:0];
    bits_word[15:8]      = update_hi[7:0];
    bits_word[23:16]     = filter_lo[7:0];
    bits_word[31:24]     = filter_hi[7:0];
    value_word           = '0;
    value_word[15:0]     = update_key[15:0];
    value_word[31:16]    = filter_key[15:0];
    csr_write(CSR_KEY_BITS, bits_word);
    csr_write(CSR_KEY_VAL, value_word);
  endtask

  local task automatic configure_histogram(
    input int signed   left_bound,
    input int unsigned bin_width,
    input bit          key_unsigned,
    input bit          filter_enable = 1'b0,
    input bit          filter_reject = 1'b0,
    input int unsigned interval_cfg  = HS_TEST_INTERVAL_CFG
  );
    csr_write(CSR_LEFT_BOUND, $unsigned(left_bound));
    csr_write(CSR_BIN_WIDTH, bin_width[31:0]);
    csr_write(CSR_INTERVAL, interval_cfg[31:0]);
    begin
      bit [31:0] control_word;
      control_word       = '0;
      control_word[0]    = 1'b1;
      control_word[8]    = key_unsigned;
      control_word[12]   = filter_enable;
      control_word[13]   = filter_reject;
      csr_write(CSR_CONTROL, control_word);
    end
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

  local task automatic check_bin(
    input string       case_id,
    input int unsigned bin_index,
    input bit [31:0]   expected
  );
    bit [31:0] burst_data[$];
    bin_burst_read(bin_index[7:0], 8'd1, burst_data);
    if (burst_data[0] !== expected)
      `uvm_error(case_id, $sformatf("bin%0d expected %0d got %0d", bin_index, expected, burst_data[0]))
  endtask

  // E137: Filter disabled — all hits pass regardless of data
  local task automatic task_e137();
    `uvm_info(get_type_name(), "E137: filter disabled, all hits pass", UVM_LOW)
    configure_key_filter(.filter_key(16'h0005));
    configure_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1),
                        .filter_enable(1'b0), .filter_reject(1'b0));
    send_hit(0, 0, .filter_value(0));    // filter field != key
    send_hit(0, 0, .filter_value(5));    // filter field = key
    send_hit(0, 0, .filter_value(15));   // filter field != key
    wait_pipeline_drain(256);
    begin
      bit [31:0] th;
      csr_read(CSR_TOTAL_HITS, th);
      if (th !== 32'd3)
        `uvm_error("E137", $sformatf("total_hits expected 3 got %0d", th))
    end
    wait_bank_swap();
    check_bin("E137", 0, 32'd3);
  endtask

  // E138: Filter accept mode — field matches key exactly
  local task automatic task_e138();
    `uvm_info(get_type_name(), "E138: filter accept, field matches key=5", UVM_LOW)
    configure_key_filter(.filter_key(16'h0005));
    configure_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1),
                        .filter_enable(1'b1), .filter_reject(1'b0));
    send_hit(0, 0, .filter_value(5));
    wait_pipeline_drain(256);
    wait_bank_swap();
    check_bin("E138", 0, 32'd1);
  endtask

  // E139: Filter accept mode — field differs by 1 bit → rejected
  local task automatic task_e139();
    `uvm_info(get_type_name(), "E139: filter accept, field=4 (key=5, differs by 1 bit)", UVM_LOW)
    configure_key_filter(.filter_key(16'h0005));
    configure_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1),
                        .filter_enable(1'b1), .filter_reject(1'b0));
    send_hit(0, 0, .filter_value(4));  // 4 = 0b0100, 5 = 0b0101, differ in bit 0
    wait_pipeline_drain(256);
    wait_bank_swap();
    check_bin("E139", 0, 32'd0);
  endtask

  // E140: Filter reject mode — field matches key → rejected
  local task automatic task_e140();
    `uvm_info(get_type_name(), "E140: filter reject, field=key=5 → hit rejected", UVM_LOW)
    configure_key_filter(.filter_key(16'h0005));
    configure_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1),
                        .filter_enable(1'b1), .filter_reject(1'b1));
    send_hit(0, 0, .filter_value(5));
    wait_pipeline_drain(256);
    wait_bank_swap();
    check_bin("E140", 0, 32'd0);
  endtask

  // E141: Filter reject mode — field does NOT match key → passes
  local task automatic task_e141();
    `uvm_info(get_type_name(), "E141: filter reject, field=3 != key=5 → hit passes", UVM_LOW)
    configure_key_filter(.filter_key(16'h0005));
    configure_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1),
                        .filter_enable(1'b1), .filter_reject(1'b1));
    send_hit(0, 0, .filter_value(3));
    wait_pipeline_drain(256);
    wait_bank_swap();
    check_bin("E141", 0, 32'd1);
  endtask

  // E142: Filter key at MSB edge: filter_hi=38, filter_lo=35
  local task automatic task_e142();
    `uvm_info(get_type_name(), "E142: filter field at MSB [38:35], match key=0xF", UVM_LOW)
    configure_key_filter(.filter_lo(35), .filter_hi(38), .filter_key(16'h000F));
    configure_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1),
                        .filter_enable(1'b1), .filter_reject(1'b0));
    send_hit(0, 0, .filter_value(15), .filter_lo(35), .filter_hi(38));
    wait_pipeline_drain(256);
    wait_bank_swap();
    check_bin("E142", 0, 32'd1);
  endtask

  // E143: Filter key at LSB edge: filter_lo=0, filter_hi=3
  local task automatic task_e143();
    logic [HS_AVST_DATA_W-1:0] word;
    `uvm_info(get_type_name(), "E143: filter field at LSB [3:0], match key=0xA", UVM_LOW)
    configure_key_filter(.filter_lo(0), .filter_hi(3), .filter_key(16'h000A));
    configure_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1),
                        .filter_enable(1'b1), .filter_reject(1'b0));
    // Build word manually: key in [29:17]=0, filter in [3:0]=0xA
    word = '0;
    word[3:0] = 4'hA;
    send_fill_word(0, word);
    wait_pipeline_drain(256);
    wait_bank_swap();
    check_bin("E143", 0, 32'd1);
  endtask

  // E144: Filter with key_lo > key_hi (degenerate)
  // extract_unsigned loop has bit_hi - bit_lo underflow → field is 0 or broken
  local task automatic task_e144();
    `uvm_info(get_type_name(), "E144: filter_lo > filter_hi (degenerate) — skipped (undefined behavior)", UVM_LOW)
  endtask

  // E145: Filter key width wider than field — 4-bit field, 16-bit key with upper bits set
  local task automatic task_e145();
    `uvm_info(get_type_name(), "E145: 4-bit field vs 16-bit key with upper bits nonzero", UVM_LOW)
    // filter_lo=35, filter_hi=38 → 4-bit field. filter_key = 0xFF0A (upper bits nonzero).
    // Extracted field = 4 bits, resized to 16 bits = 0x000A. Key = 0xFF0A. Won't match.
    configure_key_filter(.filter_lo(35), .filter_hi(38), .filter_key(16'hFF0A));
    configure_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1),
                        .filter_enable(1'b1), .filter_reject(1'b0));
    send_hit(0, 0, .filter_value(10), .filter_lo(35), .filter_hi(38));  // field = 0xA
    wait_pipeline_drain(256);
    wait_bank_swap();
    // Should NOT match because field (0x000A) != key (0xFF0A)
    check_bin("E145", 0, 32'd0);
  endtask

  // E146: Filter field all zeros vs filter key all zeros
  local task automatic task_e146();
    `uvm_info(get_type_name(), "E146: zero field matches zero key in accept mode", UVM_LOW)
    configure_key_filter(.filter_key(16'h0000));
    configure_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1),
                        .filter_enable(1'b1), .filter_reject(1'b0));
    send_hit(0, 0, .filter_value(0));
    wait_pipeline_drain(256);
    wait_bank_swap();
    check_bin("E146", 0, 32'd1);
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_e137(); issue_measure_clear();
    task_e138(); issue_measure_clear();
    task_e139(); issue_measure_clear();
    task_e140(); issue_measure_clear();
    task_e141(); issue_measure_clear();
    task_e142(); issue_measure_clear();
    task_e143(); issue_measure_clear();
    task_e144(); issue_measure_clear();
    task_e145(); issue_measure_clear();
    task_e146();

    phase.drop_objection(this);
  endtask
endclass
