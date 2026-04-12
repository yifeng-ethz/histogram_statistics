class hist_real_hit_rand_test extends hist_base_test;
  `uvm_component_utils(hist_real_hit_rand_test)

  localparam bit [4:0] CSR_UNDERFLOW   = 5'd8;
  localparam bit [4:0] CSR_OVERFLOW    = 5'd9;
  localparam bit [4:0] CSR_TOTAL_HITS  = 5'd13;
  localparam bit [4:0] CSR_COAL_STATUS = 5'd15;
  localparam int unsigned HS_INTERVAL_CFG = 8192;

  int unsigned hits_per_phase;
  string profile_name;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    if (!$value$plusargs("HIST_REAL_HITS=%d", hits_per_phase)) begin
      hits_per_phase = 256;
    end
    if (!$value$plusargs("HIST_REAL_PROFILE=%s", profile_name)) begin
      profile_name = "triad";
    end
  endfunction

  function automatic int unsigned encode_signed_9(input int signed value);
    return int'(value) & 9'h1ff;
  endfunction

  local task automatic random_hit_fields(
    output int unsigned port_index,
    output int unsigned asic_id,
    output int unsigned channel_id,
    output int unsigned tcc_8n,
    output int unsigned tcc_1n6,
    output int unsigned tfine,
    output int unsigned et_1n6
  );
    port_index = $urandom_range(HS_N_PORTS - 1, 0);
    asic_id    = $urandom_range(15, 0);
    channel_id = $urandom_range(31, 0);
    tcc_8n     = $urandom_range((1 << 13) - 1, 0);
    tcc_1n6    = $urandom_range((1 << 3) - 1, 0);
    tfine      = $urandom_range((1 << 5) - 1, 0);
    et_1n6     = $urandom_range((1 << 9) - 1, 0);
  endtask

  local task automatic sample_bin_window(input string case_id);
    bit [31:0] burst_data[$];

    bin_burst_read(8'd0, 8'd32, burst_data);
    `uvm_info(case_id, $sformatf("sampled %0d frozen-bank bins", burst_data.size()), UVM_LOW)
  endtask

  local task automatic phase_unsigned_tcc();
    bit [31:0] total_hits;
    bit [31:0] underflow_hits;
    bit [31:0] overflow_hits;
    bit [31:0] coal_status;
    int unsigned port_index;
    int unsigned asic_id;
    int unsigned channel_id;
    int unsigned tcc_8n;
    int unsigned tcc_1n6;
    int unsigned tfine;
    int unsigned et_1n6;

    `uvm_info(get_type_name(), "real-hit phase A: unsigned tcc_8n histogram", UVM_LOW)
    program_key_fields(
      .update_lo(HS_TYPE1_TCC8N_LO[7:0]),
      .update_hi(HS_TYPE1_TCC8N_HI[7:0]),
      .filter_lo(HS_TYPE1_ASIC_LO[7:0]),
      .filter_hi(HS_TYPE1_ASIC_HI[7:0])
    );
    program_histogram(
      .left_bound   (128),
      .bin_width    (4),
      .key_unsigned (1'b1),
      .filter_enable(1'b0),
      .filter_reject(1'b0),
      .interval_cfg (HS_INTERVAL_CFG)
    );

    for (int idx = 0; idx < hits_per_phase; idx++) begin
      random_hit_fields(port_index, asic_id, channel_id, tcc_8n, tcc_1n6, tfine, et_1n6);
      tcc_8n = $urandom_range(2047, 0);
      send_type1_hit(port_index, asic_id, channel_id, tcc_8n, tcc_1n6, tfine, et_1n6);
      repeat ($urandom_range(2, 0)) @(cfg.probe_vif.mon_cb);
    end

    wait_pipeline_drain(2048);
    csr_read(CSR_TOTAL_HITS, total_hits);
    csr_read(CSR_UNDERFLOW, underflow_hits);
    csr_read(CSR_OVERFLOW, overflow_hits);
    csr_read(CSR_COAL_STATUS, coal_status);
    `uvm_info(
      "REAL_A",
      $sformatf(
        "hits=%0d total=%0d underflow=%0d overflow=%0d occ_max=%0d",
        hits_per_phase,
        total_hits,
        underflow_hits,
        overflow_hits,
        coal_status[15:8]
      ),
      UVM_LOW
    )

    wait_bank_swap();
    sample_bin_window("REAL_A");
  endtask

  local task automatic phase_filtered_asic();
    bit [31:0] underflow_hits;
    bit [31:0] overflow_hits;
    bit [31:0] coal_status;
    int unsigned port_index;
    int unsigned asic_id;
    int unsigned channel_id;
    int unsigned tcc_8n;
    int unsigned tcc_1n6;
    int unsigned tfine;
    int unsigned et_1n6;

    `uvm_info(get_type_name(), "real-hit phase B: unsigned et_1n6 with ASIC filter", UVM_LOW)
    program_key_fields(
      .update_lo(HS_TYPE1_ET1N6_LO[7:0]),
      .update_hi(HS_TYPE1_ET1N6_HI[7:0]),
      .filter_lo(HS_TYPE1_ASIC_LO[7:0]),
      .filter_hi(HS_TYPE1_ASIC_HI[7:0]),
      .filter_key(16'd5)
    );
    program_histogram(
      .left_bound   (128),
      .bin_width    (1),
      .key_unsigned (1'b1),
      .filter_enable(1'b1),
      .filter_reject(1'b0),
      .interval_cfg (HS_INTERVAL_CFG)
    );

    for (int idx = 0; idx < hits_per_phase; idx++) begin
      random_hit_fields(port_index, asic_id, channel_id, tcc_8n, tcc_1n6, tfine, et_1n6);
      et_1n6 = $urandom_range(511, 0);
      send_type1_hit(port_index, asic_id, channel_id, tcc_8n, tcc_1n6, tfine, et_1n6);
      repeat ($urandom_range(1, 0)) @(cfg.probe_vif.mon_cb);
    end

    wait_pipeline_drain(2048);
    csr_read(CSR_UNDERFLOW, underflow_hits);
    csr_read(CSR_OVERFLOW, overflow_hits);
    csr_read(CSR_COAL_STATUS, coal_status);
    `uvm_info(
      "REAL_B",
      $sformatf(
        "hits=%0d underflow=%0d overflow=%0d occ_max=%0d overflow_cnt=%0d",
        hits_per_phase,
        underflow_hits,
        overflow_hits,
        coal_status[15:8],
        coal_status[31:16]
      ),
      UVM_LOW
    )

    wait_bank_swap();
    sample_bin_window("REAL_B");
  endtask

  local task automatic phase_signed_filter_reject();
    bit [31:0] underflow_hits;
    bit [31:0] overflow_hits;
    bit [31:0] coal_status;
    int unsigned port_index;
    int unsigned asic_id;
    int unsigned channel_id;
    int unsigned tcc_8n;
    int unsigned tcc_1n6;
    int unsigned tfine;
    int unsigned et_1n6;

    `uvm_info(get_type_name(), "real-hit phase C: signed et_1n6 with channel reject filter", UVM_LOW)
    program_key_fields(
      .update_lo(HS_TYPE1_ET1N6_LO[7:0]),
      .update_hi(HS_TYPE1_ET1N6_HI[7:0]),
      .filter_lo(HS_TYPE1_CH_LO[7:0]),
      .filter_hi(HS_TYPE1_CH_HI[7:0]),
      .filter_key(16'd7)
    );
    program_histogram(
      .left_bound   (-64),
      .bin_width    (1),
      .key_unsigned (1'b0),
      .filter_enable(1'b1),
      .filter_reject(1'b1),
      .interval_cfg (HS_INTERVAL_CFG)
    );

    for (int idx = 0; idx < hits_per_phase; idx++) begin
      random_hit_fields(port_index, asic_id, channel_id, tcc_8n, tcc_1n6, tfine, et_1n6);
      et_1n6 = $urandom_range(511, 0);
      send_type1_hit(port_index, asic_id, channel_id, tcc_8n, tcc_1n6, tfine, et_1n6);
      if ((idx % 8) == 0) begin
        repeat ($urandom_range(2, 0)) @(cfg.probe_vif.mon_cb);
      end
    end

    wait_pipeline_drain(2048);
    csr_read(CSR_UNDERFLOW, underflow_hits);
    csr_read(CSR_OVERFLOW, overflow_hits);
    csr_read(CSR_COAL_STATUS, coal_status);
    `uvm_info(
      "REAL_C",
      $sformatf(
        "hits=%0d underflow=%0d overflow=%0d occ_max=%0d",
        hits_per_phase,
        underflow_hits,
        overflow_hits,
        coal_status[15:8]
      ),
      UVM_LOW
    )

    wait_bank_swap();
    sample_bin_window("REAL_C");
  endtask

  local task automatic phase_boundary_sweep();
    bit [31:0] total_hits;
    bit [31:0] underflow_hits;
    bit [31:0] overflow_hits;
    bit [31:0] coal_status;
    int unsigned port_index;
    int unsigned asic_id;
    int unsigned channel_id;
    int unsigned tcc_8n;
    int unsigned tcc_1n6;
    int unsigned tfine;
    int unsigned et_1n6;
    int signed boundary_values[12];

    boundary_values = '{-128, -65, -64, -63, -1, 0, 1, 63, 64, 127, 191, 255};

    `uvm_info(get_type_name(), "real-hit profile boundary: signed ET boundary harvest", UVM_LOW)
    program_key_fields(
      .update_lo(HS_TYPE1_ET1N6_LO[7:0]),
      .update_hi(HS_TYPE1_ET1N6_HI[7:0]),
      .filter_lo(HS_TYPE1_CH_LO[7:0]),
      .filter_hi(HS_TYPE1_CH_HI[7:0]),
      .filter_key(16'd7)
    );
    program_histogram(
      .left_bound   (-64),
      .bin_width    (1),
      .key_unsigned (1'b0),
      .filter_enable(1'b1),
      .filter_reject(1'b1),
      .interval_cfg (2048)
    );

    for (int idx = 0; idx < hits_per_phase; idx++) begin
      random_hit_fields(port_index, asic_id, channel_id, tcc_8n, tcc_1n6, tfine, et_1n6);
      port_index = 0;
      channel_id = (idx % 8 == 0) ? 7 : ((idx % 31) + 1);
      et_1n6 = encode_signed_9(boundary_values[idx % $size(boundary_values)]);
      send_type1_hit(port_index, asic_id, channel_id, tcc_8n, tcc_1n6, tfine, et_1n6);
      if ((idx % 16) == 15) begin
        repeat (2) @(cfg.probe_vif.mon_cb);
      end
    end

    wait_pipeline_drain(2048);
    csr_read(CSR_TOTAL_HITS, total_hits);
    csr_read(CSR_UNDERFLOW, underflow_hits);
    csr_read(CSR_OVERFLOW, overflow_hits);
    csr_read(CSR_COAL_STATUS, coal_status);
    `uvm_info(
      "REAL_BOUNDARY",
      $sformatf(
        "hits=%0d total=%0d underflow=%0d overflow=%0d occ_max=%0d",
        hits_per_phase,
        total_hits,
        underflow_hits,
        overflow_hits,
        coal_status[15:8]
      ),
      UVM_LOW
    )

    wait_bank_swap();
    sample_bin_window("REAL_BOUNDARY");
  endtask

  local task automatic phase_queue_harvest();
    bit [31:0] total_hits;
    bit [31:0] underflow_hits;
    bit [31:0] overflow_hits;
    bit [31:0] coal_status;
    int unsigned port_index;
    int unsigned asic_id;
    int unsigned channel_id;
    int unsigned tcc_8n;
    int unsigned tcc_1n6;
    int unsigned tfine;
    int unsigned et_1n6;
    int unsigned bin_bucket;

    `uvm_info(get_type_name(), "real-hit profile queue: multi-port queue/coalescing harvest", UVM_LOW)
    program_key_fields(
      .update_lo(HS_TYPE1_TCC8N_LO[7:0]),
      .update_hi(HS_TYPE1_TCC8N_HI[7:0]),
      .filter_lo(HS_TYPE1_ASIC_LO[7:0]),
      .filter_hi(HS_TYPE1_ASIC_HI[7:0])
    );
    program_histogram(
      .left_bound   (128),
      .bin_width    (4),
      .key_unsigned (1'b1),
      .filter_enable(1'b0),
      .filter_reject(1'b0),
      .interval_cfg (2048)
    );

    for (int idx = 0; idx < hits_per_phase; idx++) begin
      random_hit_fields(port_index, asic_id, channel_id, tcc_8n, tcc_1n6, tfine, et_1n6);
      port_index = 0;
      bin_bucket = idx % 32;
      if ((idx % 6) >= 4) begin
        tcc_8n = 128 + (bin_bucket * 4);
      end else begin
        tcc_8n = 128 + (((bin_bucket + (idx % 2)) % 32) * 4);
      end
      asic_id = port_index;
      channel_id = idx % 32;
      send_type1_hit(port_index, asic_id, channel_id, tcc_8n, tcc_1n6, tfine, et_1n6);
      if ((idx % 32) == 31) begin
        repeat (1) @(cfg.probe_vif.mon_cb);
      end
    end

    wait_pipeline_drain(2048);
    csr_read(CSR_TOTAL_HITS, total_hits);
    csr_read(CSR_UNDERFLOW, underflow_hits);
    csr_read(CSR_OVERFLOW, overflow_hits);
    csr_read(CSR_COAL_STATUS, coal_status);
    `uvm_info(
      "REAL_QUEUE",
      $sformatf(
        "hits=%0d total=%0d underflow=%0d overflow=%0d occ_max=%0d overflow_cnt=%0d",
        hits_per_phase,
        total_hits,
        underflow_hits,
        overflow_hits,
        coal_status[15:8],
        coal_status[31:16]
      ),
      UVM_LOW
    )

    wait_bank_swap();
    sample_bin_window("REAL_QUEUE");
  endtask

  local task automatic phase_interval_mix();
    bit [31:0] total_hits;
    bit [31:0] underflow_hits;
    bit [31:0] overflow_hits;
    int unsigned port_index;
    int unsigned asic_id;
    int unsigned channel_id;
    int unsigned tcc_8n;
    int unsigned tcc_1n6;
    int unsigned tfine;
    int unsigned et_1n6;
    int unsigned chunk_hits;

    chunk_hits = (hits_per_phase < 4) ? 1 : ((hits_per_phase + 3) / 4);
    `uvm_info(get_type_name(), "real-hit profile interval: short-interval swap/apply harvest", UVM_LOW)

    for (int phase_idx = 0; phase_idx < 4; phase_idx++) begin
      case (phase_idx)
        0: begin
          program_key_fields(
            .update_lo(HS_TYPE1_TCC8N_LO[7:0]),
            .update_hi(HS_TYPE1_TCC8N_HI[7:0]),
            .filter_lo(HS_TYPE1_ASIC_LO[7:0]),
            .filter_hi(HS_TYPE1_ASIC_HI[7:0])
          );
          program_histogram(
            .left_bound   (0),
            .bin_width    (2),
            .key_unsigned (1'b1),
            .filter_enable(1'b0),
            .filter_reject(1'b0),
            .interval_cfg (4096)
          );
        end
        1: begin
          program_key_fields(
            .update_lo(HS_TYPE1_ET1N6_LO[7:0]),
            .update_hi(HS_TYPE1_ET1N6_HI[7:0]),
            .filter_lo(HS_TYPE1_ASIC_LO[7:0]),
            .filter_hi(HS_TYPE1_ASIC_HI[7:0]),
            .filter_key(16'd5)
          );
          program_histogram(
            .left_bound   (128),
            .bin_width    (1),
            .key_unsigned (1'b1),
            .filter_enable(1'b1),
            .filter_reject(1'b0),
            .interval_cfg (6144)
          );
        end
        2: begin
          program_key_fields(
            .update_lo(HS_TYPE1_ET1N6_LO[7:0]),
            .update_hi(HS_TYPE1_ET1N6_HI[7:0]),
            .filter_lo(HS_TYPE1_CH_LO[7:0]),
            .filter_hi(HS_TYPE1_CH_HI[7:0]),
            .filter_key(16'd7)
          );
          program_histogram(
            .left_bound   (-64),
            .bin_width    (1),
            .key_unsigned (1'b0),
            .filter_enable(1'b1),
            .filter_reject(1'b1),
            .interval_cfg (4096)
          );
        end
        default: begin
          program_key_fields(
            .update_lo(HS_TYPE1_TCC8N_LO[7:0]),
            .update_hi(HS_TYPE1_TCC8N_HI[7:0]),
            .filter_lo(HS_TYPE1_ASIC_LO[7:0]),
            .filter_hi(HS_TYPE1_ASIC_HI[7:0])
          );
          program_histogram(
            .left_bound   (128),
            .bin_width    (4),
            .key_unsigned (1'b1),
            .filter_enable(1'b0),
            .filter_reject(1'b0),
            .interval_cfg (8192)
          );
        end
      endcase

      for (int idx = 0; idx < chunk_hits; idx++) begin
        random_hit_fields(port_index, asic_id, channel_id, tcc_8n, tcc_1n6, tfine, et_1n6);
        port_index = (phase_idx + idx) % HS_N_PORTS;
        case (phase_idx)
          0: tcc_8n = (idx * 2) % 64;
          1: begin
            asic_id = (idx % 2 == 0) ? 5 : (idx % 16);
            et_1n6 = 128 + (idx % 64);
          end
          2: begin
            channel_id = idx % 16;
            et_1n6 = encode_signed_9((idx % 5 == 0) ? -96 : ((idx % 128) - 64));
          end
          default: tcc_8n = 128 + ((idx * 5) % 256);
        endcase
        send_type1_hit(port_index, asic_id, channel_id, tcc_8n, tcc_1n6, tfine, et_1n6);
        if ((idx % 12) == 11) begin
          repeat (1) @(cfg.probe_vif.mon_cb);
        end
      end

      wait_pipeline_drain(256);
      csr_read(CSR_TOTAL_HITS, total_hits);
      csr_read(CSR_UNDERFLOW, underflow_hits);
      csr_read(CSR_OVERFLOW, overflow_hits);
      `uvm_info(
        "REAL_INTERVAL",
        $sformatf(
          "phase=%0d chunk_hits=%0d total=%0d underflow=%0d overflow=%0d",
          phase_idx,
          chunk_hits,
          total_hits,
          underflow_hits,
          overflow_hits
        ),
        UVM_LOW
      )
      wait_bank_swap();
      sample_bin_window($sformatf("REAL_INTERVAL_%0d", phase_idx));
    end
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    wait_reset_release();
    wait_initial_clear();

    `uvm_info(
      get_type_name(),
      $sformatf("profile=%s hits_per_phase=%0d", profile_name, hits_per_phase),
      UVM_LOW
    )

    if ((profile_name == "triad") || (profile_name == "default")) begin
      phase_unsigned_tcc();
      issue_measure_clear();
      phase_filtered_asic();
      issue_measure_clear();
      phase_signed_filter_reject();
      issue_measure_clear();
    end else if (profile_name == "boundary") begin
      phase_boundary_sweep();
      issue_measure_clear();
    end else if (profile_name == "queue") begin
      phase_queue_harvest();
      issue_measure_clear();
    end else if (profile_name == "interval") begin
      phase_interval_mix();
      issue_measure_clear();
    end else if (profile_name == "all") begin
      phase_unsigned_tcc();
      issue_measure_clear();
      phase_filtered_asic();
      issue_measure_clear();
      phase_signed_filter_reject();
      issue_measure_clear();
      phase_boundary_sweep();
      issue_measure_clear();
      phase_queue_harvest();
      issue_measure_clear();
      phase_interval_mix();
      issue_measure_clear();
    end else begin
      `uvm_fatal(get_type_name(), $sformatf("unknown HIST_REAL_PROFILE=%s", profile_name))
    end

    phase.drop_objection(this);
  endtask
endclass
