class hist_v3_direct_input_test extends hist_base_test;
  `uvm_component_utils(hist_v3_direct_input_test)

  localparam int unsigned V3_CLK_HZ            = 125_000_000;
  localparam int unsigned V3_PINGPONG_1MS      = 125_000;
  localparam int unsigned V3_RUNNING_5MS       = 625_000;
  localparam int unsigned V3_TYPE0_HITS_ASIC   = 500;
  localparam int unsigned V3_TYPE0_LANE_GAP    = 624;

  int unsigned selected_ch[16];

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  task automatic check_control_source(input bit [1:0] expected_source, input bit [3:0] expected_mode);
    bit [31:0] control_v;
    csr_read(5'd2, control_v);
    if (control_v[17:16] !== expected_source) begin
      `uvm_error("V3CSR", $sformatf("CONTROL source mismatch dut=%0d expected=%0d word=0x%08h", control_v[17:16], expected_source, control_v))
    end
    if (control_v[7:4] !== expected_mode) begin
      `uvm_error("V3CSR", $sformatf("CONTROL mode mismatch dut=%0d expected=%0d word=0x%08h", control_v[7:4], expected_mode, control_v))
    end
    if (control_v[24]) begin
      `uvm_error("V3CSR", $sformatf("CONTROL error set unexpectedly: 0x%08h", control_v))
    end
  endtask

  task automatic run_type0_lane(input int unsigned lane_idx);
    hist_fill_seq seq;
    int unsigned asic_id;
    int unsigned hit_idx;
    int unsigned lane_hit_count;

    seq = hist_fill_seq::type_id::create($sformatf("v3_type0_lane_%0d", lane_idx));
    seq.port_index        = lane_idx;
    seq.use_raw_words     = 1'b1;
    seq.inter_gap_cycles  = V3_TYPE0_LANE_GAP;
    lane_hit_count        = V3_TYPE0_HITS_ASIC * 2;

    for (int unsigned idx = 0; idx < lane_hit_count; idx++) begin
      asic_id = lane_idx + ((idx & 1) ? 8 : 0);
      hit_idx = idx >> 1;
      seq.raw_words.push_back(make_type0_fill_word(
        asic_id,
        selected_ch[asic_id],
        (hit_idx + (asic_id * 37)) & 15'h7fff,
        hit_idx[4:0],
        (hit_idx * 3) & 15'h7fff,
        1'b0
      ));
      seq.channels.push_back(asic_id[HS_AVST_CH_W-1:0]);
    end

    seq.start(env.fill_agents[lane_idx].seqr);
  endtask

  task automatic run_type0_5ms_rate();
    bit [31:0] total_v;
    bit [31:0] last_total_v;
    bit [31:0] dropped_v;
    bit [31:0] coal_v;

    for (int unsigned asic = 0; asic < 16; asic++) begin
      selected_ch[asic] = (asic * 11 + 7) % 32;
    end

    send_ctrl_word(9'h002); // RUN_PREPARE
    program_key_fields(
      .update_lo(HS_TYPE0_TCC_LO[7:0]),
      .update_hi(HS_TYPE0_TCC_HI[7:0]),
      .filter_lo(HS_TYPE0_ASIC_LO[7:0]),
      .filter_hi(HS_TYPE0_ASIC_HI[7:0]),
      .filter_key(16'h0000)
    );
    program_histogram(
      .left_bound(0),
      .bin_width(128),
      .mode(4'h0),
      .source_select(HS_SOURCE_TYPE0),
      .key_unsigned(1'b1),
      .filter_enable(1'b0),
      .interval_cfg(V3_PINGPONG_1MS)
    );
    check_control_source(HS_SOURCE_TYPE0, 4'h0);
    send_ctrl_word(9'h004); // SYNC
    repeat (8) @(cfg.probe_vif.mon_cb);
    send_ctrl_word(9'h008); // RUNNING

    fork
      run_type0_lane(0);
      run_type0_lane(1);
      run_type0_lane(2);
      run_type0_lane(3);
      run_type0_lane(4);
      run_type0_lane(5);
      run_type0_lane(6);
      run_type0_lane(7);
      begin
        repeat (V3_PINGPONG_1MS * 3) @(cfg.probe_vif.mon_cb);
        csr_read(5'd17, last_total_v);
        if (last_total_v == 0) begin
          `uvm_error("V3TYPE0", "LAST_INTERVAL_TOTAL_HITS stayed zero during Type0 5 ms run")
        end
      end
    join

    wait_pipeline_drain(2048);
    csr_read(5'd17, last_total_v);
    csr_read(5'd13, total_v);
    csr_read(5'd14, dropped_v);
    csr_read(5'd15, coal_v);
    if ((last_total_v == 0) && (total_v == 0)) begin
      `uvm_error("V3TYPE0", $sformatf("Type0 direct rate path produced no observable hits last=%0d live=%0d", last_total_v, total_v))
    end
    if (dropped_v != 0) begin
      `uvm_error("V3TYPE0", $sformatf("Type0 100 kHz/ASIC run dropped hits: %0d", dropped_v))
    end
    if (coal_v[31:16] != 0) begin
      `uvm_error("V3TYPE0", $sformatf("Type0 bunched coalescer overflowed: coal_status=0x%08h", coal_v))
    end
    `uvm_info("V3TYPE0", $sformatf("5 ms direct Type0 evidence: last_interval_total=%0d live_total=%0d dropped=%0d coal_status=0x%08h", last_total_v, total_v, dropped_v, coal_v), UVM_LOW)
  endtask

  task automatic send_type1_burst(
    input bit [1:0] source_select,
    input int unsigned hit_count,
    input bit delay_mode,
    input int unsigned delay_cycles,
    input int unsigned asic_base
  );
    logic [HS_TS_W-1:0] true_ts_v;
    int unsigned asic_v;

    for (int unsigned idx = 0; idx < hit_count; idx++) begin
      asic_v = (asic_base + idx) & 4'hf;
      if (delay_mode) begin
        true_ts_v = cfg.probe_vif.mon_cb.gts - delay_cycles;
      end else begin
        true_ts_v = '0;
      end
      send_type1_hit(
        source_select,
        asic_v,
        (idx * 5 + 3) % 32,
        (idx + 32) & 13'h1fff,
        true_ts_v,
        idx[2:0],
        idx[4:0],
        idx[8:0]
      );
    end
  endtask

  task automatic run_type1_case(
    input bit [1:0] source_select,
    input bit delay_mode,
    input int unsigned delay_cycles
  );
    bit [31:0] total_before_v;
    bit [31:0] total_after_v;
    bit [31:0] dropped_v;
    bit [31:0] coal_v;
    string case_name_v;

    case_name_v = $sformatf("source=%0d mode=%s", source_select, delay_mode ? "delay" : "rate");
    issue_measure_clear();
    program_key_fields(
      .update_lo(HS_TYPE1_TCC8N_LO[7:0]),
      .update_hi(HS_TYPE1_TCC8N_HI[7:0]),
      .filter_lo(HS_TYPE1_ASIC_LO[7:0]),
      .filter_hi(HS_TYPE1_ASIC_HI[7:0]),
      .filter_key(16'h0004)
    );
    program_histogram(
      .left_bound(0),
      .bin_width(delay_mode ? 1 : 16),
      .mode(delay_mode ? 4'h1 : 4'h0),
      .source_select(source_select),
      .key_unsigned(1'b1),
      .filter_enable(1'b0),
      .filter_reject(1'b0),
      .interval_cfg(V3_CLK_HZ)
    );
    check_control_source(source_select, delay_mode ? 4'h1 : 4'h0);
    csr_read(5'd13, total_before_v);
    send_type1_burst(source_select, 256, delay_mode, delay_cycles, source_select * 3);
    wait_pipeline_drain(2048);
    csr_read(5'd13, total_after_v);
    csr_read(5'd14, dropped_v);
    csr_read(5'd15, coal_v);
    if (total_after_v <= total_before_v) begin
      `uvm_error("V3TYPE1", $sformatf("Type1 %s produced no hit count before=%0d after=%0d", case_name_v, total_before_v, total_after_v))
    end
    if (dropped_v != 0) begin
      `uvm_error("V3TYPE1", $sformatf("Type1 %s dropped hits: %0d", case_name_v, dropped_v))
    end
    if (coal_v[31:16] != 0) begin
      `uvm_error("V3TYPE1", $sformatf("Type1 %s bunched coalescer overflowed: coal_status=0x%08h", case_name_v, coal_v))
    end
    `uvm_info("V3TYPE1", $sformatf("direct Type1 evidence %s: delta_total=%0d dropped=%0d coal_status=0x%08h", case_name_v, total_after_v - total_before_v, dropped_v, coal_v), UVM_LOW)
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    run_type0_5ms_rate();
    run_type1_case(HS_SOURCE_TYPE1_UP, 1'b0, 0);
    run_type1_case(HS_SOURCE_TYPE1_UP, 1'b1, 96);
    run_type1_case(HS_SOURCE_TYPE1_DOWN, 1'b0, 0);
    run_type1_case(HS_SOURCE_TYPE1_DOWN, 1'b1, 144);

    phase.drop_objection(this);
  endtask
endclass
