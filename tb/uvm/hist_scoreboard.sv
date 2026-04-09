class hist_scoreboard extends uvm_scoreboard;
  `uvm_component_utils(hist_scoreboard)

  hist_env_cfg                                 cfg;
  hist_cfg_state                               shadow_cfg;
  hist_cfg_state                               active_cfg;
  int unsigned                                 ref_bins[HS_N_BINS];
  int unsigned                                 ref_bank_bins[2][HS_N_BINS];
  int unsigned                                 ref_total_hits;
  int unsigned                                 ref_dropped_hits;
  int unsigned                                 ref_underflow_count;
  int unsigned                                 ref_overflow_count;

  uvm_analysis_imp_csr  #(hist_csr_txn,   hist_scoreboard) csr_imp;
  uvm_analysis_imp_bin  #(hist_bin_txn,   hist_scoreboard) bin_imp;
  uvm_analysis_imp_fill #(hist_fill_txn,  hist_scoreboard) fill_imp;
  uvm_analysis_imp_dbg  #(hist_debug_txn, hist_scoreboard) dbg_imp;
  uvm_analysis_imp_snoop#(hist_snoop_txn, hist_scoreboard) snoop_imp;

  hist_model_item       ingress_q[HS_N_PORTS][$];
  hist_model_item       fifo_q[HS_N_PORTS][$];
  hist_expected_evt     expected_evt_q[$];
  bit                   stats_reset_pending;
  bit                   last_burst_active;
  bit                   expected_read_bank_q[$];
  int unsigned          post_clear_drain;  // suppress ghost pipeline events after registered clear

  function new(string name, uvm_component parent);
    super.new(name, parent);
    csr_imp   = new("csr_imp", this);
    bin_imp   = new("bin_imp", this);
    fill_imp  = new("fill_imp", this);
    dbg_imp   = new("dbg_imp", this);
    snoop_imp = new("snoop_imp", this);
  endfunction

  function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    if (!uvm_config_db#(hist_env_cfg)::get(this, "", "env_cfg", cfg)) begin
      `uvm_fatal(get_type_name(), "missing env_cfg")
    end
    shadow_cfg = hist_cfg_state::type_id::create("shadow_cfg");
    active_cfg = hist_cfg_state::type_id::create("active_cfg");
    reset_model();
  endfunction

  function void reset_model();
    shadow_cfg.reset_defaults();
    active_cfg.reset_defaults();
    ref_total_hits        = 0;
    ref_dropped_hits      = 0;
    ref_underflow_count   = 0;
    ref_overflow_count    = 0;
    stats_reset_pending   = 1'b0;
    last_burst_active     = 1'b0;
    expected_read_bank_q.delete();
    expected_evt_q.delete();
    foreach (ingress_q[idx]) begin
      ingress_q[idx].delete();
      fifo_q[idx].delete();
    end
    foreach (ref_bins[idx]) begin
      ref_bins[idx]        = 0;
      ref_bank_bins[0][idx] = 0;
      ref_bank_bins[1][idx] = 0;
    end
  endfunction

  function bit filter_match(logic [HS_AVST_DATA_W-1:0] data_word);
    int unsigned field_v;
    bit          match_v;

    if (!active_cfg.filter_enable) begin
      return 1'b1;
    end

    field_v = hist_extract_unsigned(
      data_word,
      active_cfg.filter_key_high,
      active_cfg.filter_key_low
    );
    match_v = (field_v[15:0] == active_cfg.filter_key);
    if (active_cfg.filter_reject) begin
      return !match_v;
    end
    return match_v;
  endfunction

  function int signed build_fill_key(logic [HS_AVST_DATA_W-1:0] data_word);
    if (active_cfg.key_unsigned) begin
      return int'(hist_extract_unsigned(
        data_word,
        active_cfg.update_key_high,
        active_cfg.update_key_low
      ));
    end

    return hist_extract_signed(
      data_word,
      active_cfg.update_key_high,
      active_cfg.update_key_low
    );
  endfunction

  function int signed build_debug_key(int signed mode_value, bit [15:0] debug_data);
    logic signed [15:0] signed_data_v;

    if (mode_value == -1) begin
      signed_data_v = debug_data;
      return int'(signed_data_v);
    end

    return int'(debug_data);
  endfunction

  function hist_expected_evt predict_result(
    input hist_model_item item,
    input int unsigned port_index
  );
    hist_expected_evt evt;
    int signed        effective_key_v;
    int signed        delta_v;
    int signed        bin_index_v;

    evt = hist_expected_evt::type_id::create("evt");
    effective_key_v = item.key_value + (port_index * HS_CHANNELS_PER_PORT);
    delta_v         = effective_key_v - active_cfg.left_bound;

    evt.port_index    = port_index;
    evt.effective_key = effective_key_v;

    if (effective_key_v < active_cfg.left_bound) begin
      evt.kind = HIST_EVT_UNDERFLOW;
      return evt;
    end

    if (effective_key_v >= active_cfg.right_bound) begin
      evt.kind = HIST_EVT_OVERFLOW;
      return evt;
    end

    if (active_cfg.bin_width == 0) begin
      bin_index_v = delta_v;
    end else begin
      bin_index_v = delta_v / int'(active_cfg.bin_width);
    end

    if ((bin_index_v < 0) || (bin_index_v >= HS_N_BINS)) begin
      evt.kind = HIST_EVT_OVERFLOW;
      return evt;
    end

    evt.kind      = HIST_EVT_BIN;
    evt.bin_index = bin_index_v;
    return evt;
  endfunction

  function void apply_shadow_cfg();
    active_cfg.copy(shadow_cfg);
  endfunction

  function void note_csr_write(hist_csr_txn txn);
    bit commit_ok_v;

    case (txn.address)
      4'd0: begin
        shadow_cfg.mode          = txn.writedata[7:4];
        shadow_cfg.key_unsigned  = txn.writedata[8];
        shadow_cfg.filter_enable = txn.writedata[12];
        shadow_cfg.filter_reject = txn.writedata[13];
        if (txn.writedata[0]) begin
          commit_ok_v = 1'b1;
          if (shadow_cfg.bin_width == 16'd0) begin
            if (shadow_cfg.right_bound <= shadow_cfg.left_bound) begin
              commit_ok_v = 1'b0;
            end
          end else begin
            shadow_cfg.right_bound = shadow_cfg.left_bound + (int'(shadow_cfg.bin_width) * HS_N_BINS);
          end
          if (commit_ok_v) begin
            apply_shadow_cfg();
          end
        end
      end
      4'd1: shadow_cfg.left_bound = int'($signed(txn.writedata));
      4'd2: shadow_cfg.right_bound = int'($signed(txn.writedata));
      4'd3: shadow_cfg.bin_width = txn.writedata[15:0];
      4'd4: begin
        shadow_cfg.update_key_low  = txn.writedata[7:0];
        shadow_cfg.update_key_high = txn.writedata[15:8];
        shadow_cfg.filter_key_low  = txn.writedata[23:16];
        shadow_cfg.filter_key_high = txn.writedata[31:24];
      end
      4'd5: begin
        shadow_cfg.update_key = txn.writedata[15:0];
        shadow_cfg.filter_key = txn.writedata[31:16];
      end
      4'd8: shadow_cfg.interval_cfg = txn.writedata;
      default: begin
      end
    endcase
  endfunction

  function void queue_fill_item(
    input int unsigned port_index,
    input logic [HS_AVST_DATA_W-1:0] data_word,
    input bit is_debug,
    input int unsigned source_index,
    input int signed key_value,
    input bit passes_filter
  );
    hist_model_item item;

    if (!passes_filter) begin
      return;
    end

    item = hist_model_item::type_id::create($sformatf("ingress_p%0d", port_index));
    item.key_value    = key_value;
    item.is_debug     = is_debug;
    item.source_index = source_index;
    item.raw_data     = data_word;
    ingress_q[port_index].push_back(item);
  endfunction

  function void write_fill(hist_fill_txn txn);
    int signed mode_v;
    int signed key_v;

    mode_v = active_cfg.mode_int();
    if (mode_v < 0) begin
      return;
    end

    ref_total_hits = hist_sat_add_u32(ref_total_hits, 1);
    key_v          = build_fill_key(txn.data);
    queue_fill_item(
      txn.port_index,
      txn.data,
      1'b0,
      txn.port_index,
      key_v,
      filter_match(txn.data)
    );
  endfunction

  function void write_dbg(hist_debug_txn txn);
    int signed mode_v;
    int signed selected_dbg_v;
    int signed key_v;

    mode_v = active_cfg.mode_int();
    if (mode_v >= 0) begin
      return;
    end

    selected_dbg_v = -mode_v;
    if (selected_dbg_v != (txn.debug_index + 1)) begin
      return;
    end

    ref_total_hits = hist_sat_add_u32(ref_total_hits, 1);
    key_v          = build_debug_key(mode_v, txn.data);
    queue_fill_item(
      0,
      '0,
      1'b1,
      txn.debug_index,
      key_v,
      1'b1
    );
  endfunction

  function void write_csr(hist_csr_txn txn);
    if (txn.write) begin
      note_csr_write(txn);
      return;
    end

    case (txn.address)
      4'd6: if (txn.readdata !== ref_underflow_count) begin
        `uvm_error("HIST_SCB", $sformatf("UNDERFLOW csr mismatch dut=%0d ref=%0d", txn.readdata, ref_underflow_count))
      end
      4'd7: if (txn.readdata !== ref_overflow_count) begin
        `uvm_error("HIST_SCB", $sformatf("OVERFLOW csr mismatch dut=%0d ref=%0d", txn.readdata, ref_overflow_count))
      end
      4'd11: if (txn.readdata !== ref_total_hits) begin
        `uvm_error("HIST_SCB", $sformatf("TOTAL_HITS csr mismatch dut=%0d ref=%0d", txn.readdata, ref_total_hits))
      end
      4'd12: if (txn.readdata !== ref_dropped_hits) begin
        `uvm_error("HIST_SCB", $sformatf("DROPPED_HITS csr mismatch dut=%0d ref=%0d", txn.readdata, ref_dropped_hits))
      end
      default: begin
      end
    endcase
  endfunction

  function void write_bin(hist_bin_txn txn);
    int unsigned read_bank_v;
    int unsigned addr_v;
    int unsigned expected_v;

    if (txn.write) begin
      return;
    end

    if (expected_read_bank_q.size() != 0) begin
      read_bank_v = expected_read_bank_q.pop_front();
    end else begin
      read_bank_v = cfg.probe_vif.active_bank ? 0 : 1;
    end
    for (int idx = 0; idx < txn.readdata.size(); idx++) begin
      addr_v = txn.address + idx;
      if (addr_v >= HS_N_BINS) begin
        break;
      end
      expected_v   = ref_bank_bins[read_bank_v][addr_v];
      ref_bins[addr_v] = expected_v;
      if (txn.readdata[idx] !== expected_v) begin
        `uvm_error(
          "HIST_SCB",
          $sformatf(
            "bin[%0d] mismatch bank=%0d dut=%0d ref=%0d",
            addr_v,
            read_bank_v,
            txn.readdata[idx],
            expected_v
          )
        )
      end
    end
  endfunction

  function void write_snoop(hist_snoop_txn txn);
  endfunction

  task run_phase(uvm_phase phase);
    hist_model_item    item;
    hist_expected_evt  evt;

    forever begin
      @(cfg.probe_vif.mon_cb);

      if (cfg.probe_vif.mon_cb.rst) begin
        reset_model();
        continue;
      end

      if (stats_reset_pending) begin
        ref_total_hits        = 0;
        ref_dropped_hits      = 0;
        ref_underflow_count   = 0;
        ref_overflow_count    = 0;
        stats_reset_pending   = 1'b0;
      end

      if (cfg.probe_vif.mon_cb.measure_clear_pulse) begin
        foreach (ref_bins[idx]) begin
          ref_bins[idx]         = 0;
          ref_bank_bins[0][idx] = 0;
          ref_bank_bins[1][idx] = 0;
        end
        foreach (ingress_q[idx]) begin
          ingress_q[idx].delete();
          fifo_q[idx].delete();
        end
        expected_evt_q.delete();
        active_cfg.copy(shadow_cfg);
        stats_reset_pending = 1'b1;
        // Registered measure_clear_pulse (Rev 1.1) allows 1 stale arb_valid
        // on the clear cycle. The 3-stage pre-divider pipeline (no clear)
        // then feeds it into the 8-stage bin_divider. Suppress ghost events
        // for 3 + 8 + 1 = 12 cycles after clear.
        post_clear_drain = 12;
      end else if (post_clear_drain > 0) begin
        post_clear_drain = post_clear_drain - 1;
      end

      if (cfg.probe_vif.mon_cb.interval_pulse) begin
        int unsigned new_active_bank_v;
        new_active_bank_v = cfg.probe_vif.mon_cb.active_bank;
        for (int idx = 0; idx < HS_N_BINS; idx++) begin
          ref_bank_bins[new_active_bank_v][idx] = 0;
        end
        stats_reset_pending = 1'b1;
      end

      if (cfg.probe_vif.mon_cb.burst_active && !last_burst_active) begin
        expected_read_bank_q.push_back(cfg.probe_vif.mon_cb.read_bank_latched);
      end
      last_burst_active = cfg.probe_vif.mon_cb.burst_active;

      for (int port_idx = 0; port_idx < HS_N_PORTS; port_idx++) begin
        if (cfg.probe_vif.mon_cb.drop_pulse[port_idx]) begin
          ref_dropped_hits = hist_sat_add_u32(ref_dropped_hits, 1);
          if (ingress_q[port_idx].size() == 0) begin
            `uvm_error("HIST_SCB", $sformatf("drop on port %0d with empty ingress model queue", port_idx))
          end else begin
            void'(ingress_q[port_idx].pop_front());
          end
        end

        if (cfg.probe_vif.mon_cb.fifo_write[port_idx]) begin
          if (ingress_q[port_idx].size() == 0) begin
            `uvm_error("HIST_SCB", $sformatf("fifo_write on port %0d with empty ingress model queue", port_idx))
          end else begin
            item = ingress_q[port_idx].pop_front();
            fifo_q[port_idx].push_back(item);
          end
        end
      end

      if (cfg.probe_vif.mon_cb.arb_valid) begin
        int unsigned arb_port_v;
        arb_port_v = cfg.probe_vif.mon_cb.arb_port;
        if (fifo_q[arb_port_v].size() == 0) begin
          if (post_clear_drain == 0 && !cfg.probe_vif.mon_cb.measure_clear_pulse)
            `uvm_error("HIST_SCB", $sformatf("arbiter popped port %0d with empty fifo model queue", arb_port_v))
        end else begin
          item = fifo_q[arb_port_v].pop_front();
          evt  = predict_result(item, arb_port_v);
          expected_evt_q.push_back(evt);
        end
      end

      if (cfg.probe_vif.mon_cb.divider_valid) begin
        if (expected_evt_q.size() == 0) begin
          if (post_clear_drain == 0 && !cfg.probe_vif.mon_cb.measure_clear_pulse)
            `uvm_error("HIST_SCB", "divider_valid with empty expected event queue")
        end else begin
          evt = expected_evt_q.pop_front();
          case (evt.kind)
            HIST_EVT_BIN: begin
              if (cfg.probe_vif.mon_cb.divider_underflow || cfg.probe_vif.mon_cb.divider_overflow) begin
                `uvm_error("HIST_SCB", $sformatf("expected bin event but divider flags uf=%0b of=%0b", cfg.probe_vif.mon_cb.divider_underflow, cfg.probe_vif.mon_cb.divider_overflow))
              end
              if (cfg.probe_vif.mon_cb.divider_bin_index !== evt.bin_index[HS_BIN_W-1:0]) begin
                `uvm_error("HIST_SCB", $sformatf("divider bin mismatch dut=%0d ref=%0d", cfg.probe_vif.mon_cb.divider_bin_index, evt.bin_index))
              end
            end
            HIST_EVT_UNDERFLOW: begin
              ref_underflow_count = hist_sat_add_u32(ref_underflow_count, 1);
              if (!cfg.probe_vif.mon_cb.divider_underflow) begin
                `uvm_error("HIST_SCB", "expected underflow event")
              end
            end
            HIST_EVT_OVERFLOW: begin
              ref_overflow_count = hist_sat_add_u32(ref_overflow_count, 1);
              if (!cfg.probe_vif.mon_cb.divider_overflow) begin
                `uvm_error("HIST_SCB", "expected overflow event")
              end
            end
            default: begin
            end
          endcase
        end
      end

      if (cfg.probe_vif.mon_cb.queue_drain_valid && cfg.probe_vif.mon_cb.queue_drain_ready) begin
        int unsigned active_bank_v;
        int unsigned bin_v;
        int unsigned count_v;

        active_bank_v = cfg.probe_vif.mon_cb.active_bank;
        bin_v         = cfg.probe_vif.mon_cb.queue_drain_bin;
        count_v       = cfg.probe_vif.mon_cb.queue_drain_count;
        ref_bank_bins[active_bank_v][bin_v] = hist_sat_add_u32(ref_bank_bins[active_bank_v][bin_v], count_v);
      end
    end
  endtask
endclass
