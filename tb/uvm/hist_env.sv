class hist_env extends uvm_env;
  `uvm_component_utils(hist_env)

  hist_env_cfg       cfg;
  hist_csr_agent     csr_agent;
  hist_bin_agent     bin_agent;
  hist_fill_agent    fill_agents[HS_N_PORTS];
  hist_ctrl_agent    ctrl_agent;
  hist_debug_agent   dbg_agents[HS_N_DEBUG];
  hist_snoop_monitor snoop_mon;
  hist_scoreboard    scoreboard;
  hist_coverage      coverage;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    if (!uvm_config_db#(hist_env_cfg)::get(this, "", "env_cfg", cfg)) begin
      `uvm_fatal(get_type_name(), "missing env_cfg")
    end

    uvm_config_db#(hist_env_cfg)::set(this, "*", "env_cfg", cfg);

    uvm_config_db#(virtual hist_csr_if)::set(this, "csr_agent.*", "vif", cfg.csr_vif);
    uvm_config_db#(virtual hist_bin_if)::set(this, "bin_agent.*", "vif", cfg.bin_vif);
    uvm_config_db#(virtual hist_ctrl_if)::set(this, "ctrl_agent.*", "vif", cfg.ctrl_vif);
    uvm_config_db#(virtual hist_snoop_if)::set(this, "snoop_mon", "vif", cfg.snoop_vif);

    csr_agent = hist_csr_agent::type_id::create("csr_agent", this);
    bin_agent = hist_bin_agent::type_id::create("bin_agent", this);
    ctrl_agent = hist_ctrl_agent::type_id::create("ctrl_agent", this);
    snoop_mon = hist_snoop_monitor::type_id::create("snoop_mon", this);

    foreach (fill_agents[idx]) begin
      uvm_config_db#(virtual hist_fill_if)::set(this, $sformatf("fill_agent_%0d.*", idx), "vif", cfg.fill_vifs[idx]);
      uvm_config_db#(int unsigned)::set(this, $sformatf("fill_agent_%0d.*", idx), "port_index", idx);
      fill_agents[idx] = hist_fill_agent::type_id::create($sformatf("fill_agent_%0d", idx), this);
    end

    foreach (dbg_agents[idx]) begin
      uvm_config_db#(virtual hist_debug_if)::set(this, $sformatf("dbg_agent_%0d.*", idx), "vif", cfg.dbg_vifs[idx]);
      uvm_config_db#(int unsigned)::set(this, $sformatf("dbg_agent_%0d.*", idx), "debug_index", idx);
      dbg_agents[idx] = hist_debug_agent::type_id::create($sformatf("dbg_agent_%0d", idx), this);
    end

    if (cfg.enable_scoreboard) begin
      scoreboard = hist_scoreboard::type_id::create("scoreboard", this);
    end
    if (cfg.enable_coverage) begin
      coverage = hist_coverage::type_id::create("coverage", this);
    end
  endfunction

  function void connect_phase(uvm_phase phase);
    super.connect_phase(phase);

    if (scoreboard != null) begin
      csr_agent.mon.ap.connect(scoreboard.csr_imp);
      bin_agent.mon.ap.connect(scoreboard.bin_imp);
      snoop_mon.ap.connect(scoreboard.snoop_imp);
      foreach (fill_agents[idx]) begin
        fill_agents[idx].mon.ap.connect(scoreboard.fill_imp);
      end
      foreach (dbg_agents[idx]) begin
        dbg_agents[idx].mon.ap.connect(scoreboard.dbg_imp);
      end
    end

    if (coverage != null) begin
      csr_agent.mon.ap.connect(coverage.csr_imp);
      bin_agent.mon.ap.connect(coverage.bin_imp);
      snoop_mon.ap.connect(coverage.snoop_imp);
      foreach (fill_agents[idx]) begin
        fill_agents[idx].mon.ap.connect(coverage.fill_imp);
      end
      foreach (dbg_agents[idx]) begin
        dbg_agents[idx].mon.ap.connect(coverage.dbg_imp);
      end
    end
  endfunction
endclass
