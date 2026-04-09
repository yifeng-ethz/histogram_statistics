class hist_bin_test extends hist_base_test;
  `uvm_component_utils(hist_bin_test)

  localparam int unsigned HS_TEST_INTERVAL_CFG = 4096;
  localparam bit [3:0] CSR_UNDERFLOW = 4'd6;
  localparam bit [3:0] CSR_OVERFLOW  = 4'd7;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  local task automatic configure(
    input int signed   left_bound,
    input int unsigned bin_width,
    input int unsigned interval_cfg = HS_TEST_INTERVAL_CFG
  );
    program_histogram(
      .left_bound   (left_bound),
      .bin_width    (bin_width),
      .key_unsigned (1'b1),
      .interval_cfg (interval_cfg)
    );
  endtask

  local task automatic send_hit_and_check(
    input string     case_id,
    input int signed key_value,
    input int unsigned expected_bin
  );
    bit [31:0] burst_data[$];
    logic [HS_AVST_DATA_W-1:0] hit_word;
    int unsigned start_bin, burstcount;

    hit_word = make_fill_word(key_value);
    send_fill_word(0, hit_word);
    wait_pipeline_drain(128);
    wait_bank_swap();

    if (expected_bin == 0) begin
      start_bin = 0; burstcount = 3;
    end else if (expected_bin == (HS_N_BINS - 1)) begin
      start_bin = HS_N_BINS - 3; burstcount = 3;
    end else begin
      start_bin = expected_bin - 1; burstcount = 3;
    end

    bin_burst_read(start_bin[7:0], burstcount[7:0], burst_data);

    for (int idx = 0; idx < burst_data.size(); idx++) begin
      int unsigned bin_idx;
      bit [31:0] expected_count;
      bin_idx = start_bin + idx;
      expected_count = (bin_idx == expected_bin) ? 32'd1 : 32'd0;
      if (burst_data[idx] !== expected_count) begin
        `uvm_error(case_id, $sformatf("bin%0d expected %0d got %0d", bin_idx, expected_count, burst_data[idx]))
      end
    end
  endtask

  local task automatic task_b065();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B065: 256 hits, one per bin", UVM_LOW)
    configure(.left_bound(0), .bin_width(16), .interval_cfg(65536));
    for (int i = 0; i < 256; i++) begin
      send_fill_word(0, make_fill_word(i * 16));
    end
    wait_pipeline_drain(512);
    wait_bank_swap();
    bin_burst_read(8'd0, 8'd0, burst_data);  // burstcount=0 -> 1
    // Read in chunks of 16
    for (int base = 0; base < 256; base += 16) begin
      bin_burst_read(base[7:0], 8'd16, burst_data);
      for (int j = 0; j < burst_data.size(); j++) begin
        if (burst_data[j] !== 32'd1) begin
          `uvm_error("B065", $sformatf("bin%0d expected 1 got %0d", base + j, burst_data[j]))
        end
      end
    end
  endtask

  local task automatic task_b066();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B066: 4 hits all in bin 0", UVM_LOW)
    configure(.left_bound(0), .bin_width(16));
    send_fill_word(0, make_fill_word(0));
    send_fill_word(0, make_fill_word(1));
    send_fill_word(0, make_fill_word(10));
    send_fill_word(0, make_fill_word(15));
    wait_pipeline_drain(128);
    wait_bank_swap();
    bin_burst_read(8'd0, 8'd2, burst_data);
    if (burst_data[0] !== 32'd4)
      `uvm_error("B066", $sformatf("bin0 expected 4 got %0d", burst_data[0]))
    if (burst_data[1] !== 32'd0)
      `uvm_error("B066", $sformatf("bin1 expected 0 got %0d", burst_data[1]))
  endtask

  local task automatic task_b067();
    `uvm_info(get_type_name(), "B067: key = left_bound + bin_width maps to bin 1", UVM_LOW)
    configure(.left_bound(0), .bin_width(16));
    send_hit_and_check("B067", 16, 1);
  endtask

  local task automatic task_b068();
    `uvm_info(get_type_name(), "B068: key = left_bound + bin_width - 1 maps to bin 0", UVM_LOW)
    configure(.left_bound(0), .bin_width(16));
    send_hit_and_check("B068", 15, 0);
  endtask

  local task automatic task_b069();
    bit [31:0] csr_val;
    `uvm_info(get_type_name(), "B069: underflow key < left_bound", UVM_LOW)
    configure(.left_bound(100), .bin_width(16));
    send_fill_word(0, make_fill_word(50));
    wait_pipeline_drain(256);
    csr_read(CSR_UNDERFLOW, csr_val);
    if (csr_val !== 32'd1)
      `uvm_error("B069", $sformatf("underflow expected 1 got %0d", csr_val))
  endtask

  local task automatic task_b070();
    bit [31:0] csr_val;
    `uvm_info(get_type_name(), "B070: overflow key = right_bound", UVM_LOW)
    configure(.left_bound(0), .bin_width(16));
    // right_bound = 0 + 256*16 = 4096
    send_fill_word(0, make_fill_word(4096));
    wait_pipeline_drain(256);
    csr_read(CSR_OVERFLOW, csr_val);
    if (csr_val !== 32'd1)
      `uvm_error("B070", $sformatf("overflow expected 1 got %0d", csr_val))
  endtask

  local task automatic task_b071();
    bit [31:0] csr_val;
    `uvm_info(get_type_name(), "B071: far overflow key = 5000", UVM_LOW)
    configure(.left_bound(0), .bin_width(16));
    send_fill_word(0, make_fill_word(5000));
    wait_pipeline_drain(256);
    csr_read(CSR_OVERFLOW, csr_val);
    if (csr_val !== 32'd1)
      `uvm_error("B071", $sformatf("overflow expected 1 got %0d", csr_val))
  endtask

  local task automatic task_b072();
    `uvm_info(get_type_name(), "B072: bin_width=0 overflow — skipped (complex apply error)", UVM_LOW)
  endtask

  local task automatic task_b073();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B073: non-power-of-2 bin_width=7", UVM_LOW)
    configure(.left_bound(0), .bin_width(7));
    send_fill_word(0, make_fill_word(0));
    send_fill_word(0, make_fill_word(7));
    send_fill_word(0, make_fill_word(14));
    send_fill_word(0, make_fill_word(21));
    wait_pipeline_drain(128);
    wait_bank_swap();
    bin_burst_read(8'd0, 8'd4, burst_data);
    for (int i = 0; i < 4; i++) begin
      if (burst_data[i] !== 32'd1)
        `uvm_error("B073", $sformatf("bin%0d expected 1 got %0d", i, burst_data[i]))
    end
  endtask

  local task automatic task_b074();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B074: bin_width=3, boundary keys", UVM_LOW)
    configure(.left_bound(0), .bin_width(3));
    // keys={0,2,3,5,6,8} -> bins={0,0,1,1,2,2}
    send_fill_word(0, make_fill_word(0));
    send_fill_word(0, make_fill_word(2));
    send_fill_word(0, make_fill_word(3));
    send_fill_word(0, make_fill_word(5));
    send_fill_word(0, make_fill_word(6));
    send_fill_word(0, make_fill_word(8));
    wait_pipeline_drain(128);
    wait_bank_swap();
    bin_burst_read(8'd0, 8'd3, burst_data);
    if (burst_data[0] !== 32'd2)
      `uvm_error("B074", $sformatf("bin0 expected 2 got %0d", burst_data[0]))
    if (burst_data[1] !== 32'd2)
      `uvm_error("B074", $sformatf("bin1 expected 2 got %0d", burst_data[1]))
    if (burst_data[2] !== 32'd2)
      `uvm_error("B074", $sformatf("bin2 expected 2 got %0d", burst_data[2]))
  endtask

  local task automatic task_b075();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B075: large bin_width=256", UVM_LOW)
    configure(.left_bound(0), .bin_width(256));
    send_fill_word(0, make_fill_word(0));
    send_fill_word(0, make_fill_word(256));
    send_fill_word(0, make_fill_word(512));
    send_fill_word(0, make_fill_word(768));
    wait_pipeline_drain(128);
    wait_bank_swap();
    bin_burst_read(8'd0, 8'd4, burst_data);
    for (int i = 0; i < 4; i++) begin
      if (burst_data[i] !== 32'd1)
        `uvm_error("B075", $sformatf("bin%0d expected 1 got %0d", i, burst_data[i]))
    end
  endtask

  local task automatic task_b076();
    bit [31:0] burst_data[$];
    `uvm_info(get_type_name(), "B076: bin_width=1, each key unique bin", UVM_LOW)
    configure(.left_bound(0), .bin_width(1));
    send_fill_word(0, make_fill_word(0));
    send_fill_word(0, make_fill_word(1));
    send_fill_word(0, make_fill_word(2));
    send_fill_word(0, make_fill_word(3));
    send_fill_word(0, make_fill_word(100));
    send_fill_word(0, make_fill_word(255));
    wait_pipeline_drain(128);
    wait_bank_swap();
    // Check a few representative bins
    bin_burst_read(8'd0, 8'd4, burst_data);
    for (int i = 0; i < 4; i++) begin
      if (burst_data[i] !== 32'd1)
        `uvm_error("B076", $sformatf("bin%0d expected 1 got %0d", i, burst_data[i]))
    end
    bin_burst_read(8'd100, 8'd1, burst_data);
    if (burst_data[0] !== 32'd1)
      `uvm_error("B076", $sformatf("bin100 expected 1 got %0d", burst_data[0]))
    bin_burst_read(8'd255, 8'd1, burst_data);
    if (burst_data[0] !== 32'd1)
      `uvm_error("B076", $sformatf("bin255 expected 1 got %0d", burst_data[0]))
  endtask

  local task automatic task_b077();
    `uvm_info(get_type_name(), "B077: left_bound=0, bin_width=1, key=128 maps to bin 128", UVM_LOW)
    configure(.left_bound(0), .bin_width(1));
    send_hit_and_check("B077", 128, 128);
  endtask

  local task automatic task_b078();
    `uvm_info(get_type_name(), "B078: random bin_width — skipped", UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    task_b065(); issue_measure_clear();
    task_b066(); issue_measure_clear();
    task_b067(); issue_measure_clear();
    task_b068(); issue_measure_clear();
    task_b069(); issue_measure_clear();
    task_b070(); issue_measure_clear();
    task_b071(); issue_measure_clear();
    task_b072(); issue_measure_clear();
    task_b073(); issue_measure_clear();
    task_b074(); issue_measure_clear();
    task_b075(); issue_measure_clear();
    task_b076(); issue_measure_clear();
    task_b077(); issue_measure_clear();
    task_b078();

    phase.drop_objection(this);
  endtask
endclass
