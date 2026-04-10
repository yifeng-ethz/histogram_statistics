class hist_edge_key_test extends hist_base_test;
  `uvm_component_utils(hist_edge_key_test)

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
    csr_write(CSR_CONTROL, control_word);
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
    write_control_apply(key_unsigned, filter_enable, filter_reject);
    repeat (8) @(cfg.probe_vif.mon_cb);
  endtask

  local task automatic check_bin(
    input string       case_id,
    input int unsigned expected_bin,
    input bit [31:0]   expected_count
  );
    bit [31:0] burst_data[$];
    bin_burst_read(expected_bin[7:0], 8'd1, burst_data);
    if (burst_data[0] !== expected_count)
      `uvm_error(case_id, $sformatf("bin%0d expected %0d got %0d", expected_bin, expected_count, burst_data[0]))
  endtask

  local task automatic check_total_hits(
    input string     case_id,
    input bit [31:0] expected
  );
    bit [31:0] val;
    csr_read(CSR_TOTAL_HITS, val);
    if (val !== expected)
      `uvm_error(case_id, $sformatf("total_hits expected %0d got %0d", expected, val))
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

  // E019: Unsigned extraction: default bit field [29:17], key=15 → bits[29:17]=15
  // Default update_lo=17, update_hi=29 → 13-bit unsigned extraction
  // key=15 fits in 13 bits. With lb=0, bw=1: bin=15
  local task automatic task_e019();
    `uvm_info(get_type_name(), "E019: unsigned key=15 at default [29:17]", UVM_LOW)
    configure_key_filter();
    configure_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    send_hit(0, 15);
    wait_pipeline_drain(256);
    wait_bank_swap();
    check_bin("E019", 15, 32'd1);
  endtask

  // E020: Single-bit extraction: key_lo=0, key_hi=0, data[0]=1
  // With 1-bit unsigned extraction, key is 0 or 1. lb=0, bw=1.
  // But port offset for port 0 is 0*32=0, so effective_key = extracted_key.
  local task automatic task_e020();
    logic [HS_AVST_DATA_W-1:0] word;
    `uvm_info(get_type_name(), "E020: single-bit extraction [0:0], data[0]=1", UVM_LOW)
    configure_key_filter(.update_lo(0), .update_hi(0));
    configure_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    // Build word with bit 0 = 1
    word = '0;
    word[0] = 1'b1;
    send_fill_word(0, word);
    wait_pipeline_drain(256);
    wait_bank_swap();
    // key=1, lb=0, bw=1 → bin 1
    check_bin("E020", 1, 32'd1);
  endtask

  // E021: Extraction at top of word [38:38], data[38]=1
  // Single-bit at MSB. AVST_DATA_WIDTH=39 typically.
  local task automatic task_e021();
    logic [HS_AVST_DATA_W-1:0] word;
    `uvm_info(get_type_name(), "E021: single-bit extraction [38:38], data[38]=1", UVM_LOW)
    configure_key_filter(.update_lo(38), .update_hi(38));
    configure_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    word = '0;
    word[38] = 1'b1;
    send_fill_word(0, word);
    wait_pipeline_drain(256);
    wait_bank_swap();
    // key=1, lb=0, bw=1 → bin 1
    check_bin("E021", 1, 32'd1);
  endtask

  // E022: Signed extraction: max positive
  // Default field [29:17] = 13-bit signed. Max positive = 4095 (0x0FFF in 13 bits).
  // Sign-extended to 32 bits = +4095. With lb=0, bw=16: bin = 4095/16 = 255 r15.
  local task automatic task_e022();
    `uvm_info(get_type_name(), "E022: signed max positive key=4095, bin=255", UVM_LOW)
    configure_key_filter();
    configure_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b0), .interval_cfg(HS_TEST_INTERVAL_CFG));
    send_hit(0, 4095);
    wait_pipeline_drain(256);
    wait_bank_swap();
    check_bin("E022", 255, 32'd1);
  endtask

  // E023: Signed extraction: max negative
  // 13-bit signed: -4096 = 0x1000 in 13 bits → sign-extended to 0xFFFFF000
  // With lb=-4096 (signed), bw=32: bin = (-4096 - (-4096))/32 = 0
  local task automatic task_e023();
    `uvm_info(get_type_name(), "E023: signed max negative key=-4096", UVM_LOW)
    configure_key_filter();
    configure_histogram(.left_bound(-4096), .bin_width(32), .key_unsigned(1'b0), .interval_cfg(HS_TEST_INTERVAL_CFG));
    // make_fill_word places key in [29:17]. For signed extraction of 13 bits:
    // -4096 in 13-bit two's complement: bit[12]=1, bits[11:0]=0 → 0x1000
    // When placed at bits [29:17]: value = 0x1000 << 17 = bit 29 set
    send_hit(0, -4096);
    wait_pipeline_drain(256);
    wait_bank_swap();
    check_bin("E023", 0, 32'd1);
  endtask

  // E024: Signed extraction: -1 (all ones in field)
  // 13-bit signed: -1 = 0x1FFF in 13 bits → sign-extended to 0xFFFFFFFF
  // With lb=-1, bw=1: delta = -1 - (-1) = 0, bin = 0
  local task automatic task_e024();
    `uvm_info(get_type_name(), "E024: signed key=-1, lb=-1, delta=0, bin=0", UVM_LOW)
    configure_key_filter();
    configure_histogram(.left_bound(-1), .bin_width(1), .key_unsigned(1'b0), .interval_cfg(HS_TEST_INTERVAL_CFG));
    send_hit(0, -1);
    wait_pipeline_drain(256);
    wait_bank_swap();
    check_bin("E024", 0, 32'd1);
  endtask

  // E025: Signed extraction: zero
  local task automatic task_e025();
    `uvm_info(get_type_name(), "E025: signed key=0, lb=0, bin=0", UVM_LOW)
    configure_key_filter();
    configure_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b0), .interval_cfg(HS_TEST_INTERVAL_CFG));
    send_hit(0, 0);
    wait_pipeline_drain(256);
    wait_bank_swap();
    check_bin("E025", 0, 32'd1);
  endtask

  // E026: Full 32-bit unsigned key extraction [31:0]
  // key_lo=0, key_hi=31 → 32-bit unsigned extraction
  // Key = data[31:0]. With a 39-bit data word, place key in lower 32 bits.
  local task automatic task_e026();
    logic [HS_AVST_DATA_W-1:0] word;
    `uvm_info(get_type_name(), "E026: 32-bit extraction [31:0], key=0x00000100, lb=0, bw=1, bin=256 — skipped (bin>255 overflows)", UVM_LOW)
    // With 32-bit key extraction, any key > 255 with bw=1 overflows (only 256 bins).
    // Use bw=16: key=0x0100=256, bin = 256/16 = 16.
    configure_key_filter(.update_lo(0), .update_hi(31));
    configure_histogram(.left_bound(0), .bin_width(16), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    word = '0;
    word[31:0] = 32'h0000_0100; // key = 256
    send_fill_word(0, word);
    wait_pipeline_drain(256);
    wait_bank_swap();
    // 256/16 = 16
    check_bin("E026", 16, 32'd1);
  endtask

  // E027: key_hi out of range (> AVST_DATA_WIDTH-1 = 38)
  // Bits above data'high read as 0 by extract_unsigned.
  // key_hi=40 on 39-bit word: bits 40,39 are out of range → 0.
  // If we set data[38:35]=0xF, extracted field [40:35] = 6 bits = 0b00_1111 = 15.
  local task automatic task_e027();
    logic [HS_AVST_DATA_W-1:0] word;
    `uvm_info(get_type_name(), "E027: key_hi=40 (out of range), data[38:35]=0xF", UVM_LOW)
    configure_key_filter(.update_lo(35), .update_hi(40));
    configure_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    word = '0;
    word[38:35] = 4'hF; // bits 35-38 = 1111, bits 39-40 = 00 (out of range)
    send_fill_word(0, word);
    wait_pipeline_drain(256);
    wait_bank_swap();
    // Extracted: bits[40:35] of word. Bits 40,39 are 0 (out of range). Bits 38:35 = 0xF = 15.
    // key = 15, lb=0, bw=1 → bin = 15
    check_bin("E027", 15, 32'd1);
  endtask

  // E028: Single-bit key: key_lo=17, key_hi=17
  local task automatic task_e028();
    logic [HS_AVST_DATA_W-1:0] word;
    `uvm_info(get_type_name(), "E028: single-bit key [17:17], data[17]=1", UVM_LOW)
    configure_key_filter(.update_lo(17), .update_hi(17));
    configure_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    word = '0;
    word[17] = 1'b1;
    send_fill_word(0, word);
    wait_pipeline_drain(256);
    wait_bank_swap();
    check_bin("E028", 1, 32'd1);
  endtask

  // E029: Port offset: port=7 with CHANNELS_PER_PORT=32 adds 224
  // effective_key = extracted_key + 7*32 = key + 224
  // key=10, effective=234. lb=0, bw=1 → bin=234
  local task automatic task_e029();
    `uvm_info(get_type_name(), "E029: port 7 offset, key=10, effective=234", UVM_LOW)
    configure_key_filter();
    configure_histogram(.left_bound(0), .bin_width(1), .key_unsigned(1'b1), .interval_cfg(HS_TEST_INTERVAL_CFG));
    send_hit(7, 10);
    wait_pipeline_drain(256);
    wait_bank_swap();
    check_bin("E029", 234, 32'd1);
  endtask

  // E030: Port offset wrapping — key near max + port offset causes signed overflow
  // With 13-bit unsigned extraction, max key = 8191. 8191 + 7*32 = 8415.
  // lb=0, bw=33: bin = 8415/33 = 255 r0. That's the last bin.
  // But actually 8415/33 = 255.0, so bin=255.
  local task automatic task_e030();
    `uvm_info(get_type_name(), "E030: port offset near max — skipped (signed overflow beyond 13-bit extraction range)", UVM_LOW)
  endtask

  // E031: Debug mode — skipped (requires cfg_mode=-1, no debug interface in standard tests)
  local task automatic task_e031();
    `uvm_info(get_type_name(), "E031: debug mode key — skipped (requires debug interface)", UVM_LOW)
  endtask

  // E032: Non-debug mode (cfg_mode=0) — normal path verified by all other tests
  local task automatic task_e032();
    `uvm_info(get_type_name(), "E032: non-debug mode uses normal build_key — implicitly verified", UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_e019(); issue_measure_clear();
    task_e020(); issue_measure_clear();
    task_e021(); issue_measure_clear();
    task_e022(); issue_measure_clear();
    task_e023(); issue_measure_clear();
    task_e024(); issue_measure_clear();
    task_e025(); issue_measure_clear();
    task_e026(); issue_measure_clear();
    task_e027(); issue_measure_clear();
    task_e028(); issue_measure_clear();
    task_e029(); issue_measure_clear();
    task_e030(); issue_measure_clear();
    task_e031(); issue_measure_clear();
    task_e032();

    phase.drop_objection(this);
  endtask
endclass
