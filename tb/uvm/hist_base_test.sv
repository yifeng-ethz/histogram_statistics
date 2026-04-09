class hist_base_test extends uvm_test;
  `uvm_component_utils(hist_base_test)

  hist_env     env;
  hist_env_cfg cfg;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    if (!uvm_config_db#(hist_env_cfg)::get(this, "", "env_cfg", cfg)) begin
      `uvm_fatal(get_type_name(), "missing env_cfg")
    end
    uvm_config_db#(hist_env_cfg)::set(this, "env", "env_cfg", cfg);
    env = hist_env::type_id::create("env", this);
  endfunction

  task automatic wait_reset_release();
    while (cfg.probe_vif.rst !== 1'b0) begin
      @(cfg.probe_vif.mon_cb);
    end
    repeat (5) @(cfg.probe_vif.mon_cb);
  endtask

  task automatic wait_initial_clear();
    int unsigned timeout_v;
    timeout_v = 0;
    while (cfg.probe_vif.flushing === 1'b1) begin
      @(cfg.probe_vif.mon_cb);
      timeout_v++;
      if (timeout_v > 10000) begin
        `uvm_fatal(get_type_name(), "timeout waiting for initial clear")
      end
    end
    repeat (5) @(cfg.probe_vif.mon_cb);
  endtask

  task automatic wait_pipeline_drain(int unsigned cycles = 128);
    repeat (cycles) @(cfg.probe_vif.mon_cb);
  endtask

  task automatic wait_bank_swap();
    int unsigned timeout_v;
    timeout_v = 0;
    while (!cfg.probe_vif.interval_pulse) begin
      @(cfg.probe_vif.mon_cb);
      timeout_v++;
      if (timeout_v > 100000) begin
        `uvm_fatal(get_type_name(), "timeout waiting for interval_pulse")
      end
    end
    while (cfg.probe_vif.flushing) begin
      @(cfg.probe_vif.mon_cb);
    end
    repeat (4) @(cfg.probe_vif.mon_cb);
  endtask

  task automatic csr_write(bit [3:0] address, bit [31:0] data);
    hist_csr_write_seq seq;
    seq = hist_csr_write_seq::type_id::create($sformatf("csr_write_%0d", address));
    seq.address   = address;
    seq.writedata = data;
    seq.start(env.csr_agent.seqr);
  endtask

  task automatic csr_read(bit [3:0] address, output bit [31:0] data);
    hist_csr_read_seq seq;
    seq = hist_csr_read_seq::type_id::create($sformatf("csr_read_%0d", address));
    seq.address = address;
    seq.start(env.csr_agent.seqr);
    data = seq.readdata;
  endtask

  task automatic bin_burst_read(bit [7:0] address, bit [7:0] burstcount, output bit [31:0] data[$]);
    hist_bin_burst_read_seq seq;
    seq = hist_bin_burst_read_seq::type_id::create($sformatf("bin_read_%0d", address));
    seq.address    = address;
    seq.burstcount = burstcount;
    seq.start(env.bin_agent.seqr);
    data = seq.readdata;
  endtask

  task automatic issue_measure_clear();
    hist_bin_clear_seq seq;
    seq = hist_bin_clear_seq::type_id::create("measure_clear_seq");
    seq.start(env.bin_agent.seqr);
    wait_initial_clear();
  endtask

  task automatic program_histogram(
    input int signed   left_bound   = 0,
    input int unsigned bin_width    = 16,
    input bit          key_unsigned = 1'b1,
    input bit          filter_enable = 1'b0,
    input bit          filter_reject = 1'b0,
    input int unsigned interval_cfg = 64
  );
    bit [31:0] control_word;

    csr_write(4'd1, $unsigned(left_bound));
    csr_write(4'd3, bin_width[31:0]);
    csr_write(4'd8, interval_cfg[31:0]);
    control_word       = 32'h0000_0001;
    control_word[8]    = key_unsigned;
    control_word[12]   = filter_enable;
    control_word[13]   = filter_reject;
    csr_write(4'd0, control_word);
    repeat (8) @(cfg.probe_vif.mon_cb);
  endtask

  task automatic send_fill_word(
    input int unsigned                port_index,
    input logic [HS_AVST_DATA_W-1:0]  data_word,
    input bit [HS_AVST_CH_W-1:0]      channel = '0
  );
    hist_fill_seq seq;
    seq = hist_fill_seq::type_id::create($sformatf("fill_port_%0d", port_index));
    seq.port_index       = port_index;
    seq.use_raw_words    = 1'b1;
    seq.raw_words.push_back(data_word);
    seq.channels.push_back(channel);
    seq.start(env.fill_agents[port_index].seqr);
  endtask

  function logic [HS_AVST_DATA_W-1:0] make_fill_word(
    input int signed   key_value,
    input int unsigned update_lo = HS_DEF_UPDATE_LO,
    input int unsigned update_hi = HS_DEF_UPDATE_HI,
    input int unsigned filter_value = 0,
    input int unsigned filter_lo = HS_DEF_FILTER_LO,
    input int unsigned filter_hi = HS_DEF_FILTER_HI
  );
    return hist_build_fill_word(key_value, update_lo, update_hi, filter_value, filter_lo, filter_hi);
  endfunction
endclass
