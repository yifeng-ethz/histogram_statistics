class hist_coverage extends uvm_component;
  `uvm_component_utils(hist_coverage)

  hist_env_cfg cfg;
  bit [3:0]    cov_mode;
  bit          cov_key_unsigned;
  bit          cov_filter_enable;
  bit          cov_filter_reject;
  int signed   cov_left_bound;
  int unsigned cov_bin_width;
  int unsigned cov_port_index;
  int unsigned cov_debug_index;
  int unsigned cov_result_kind;
  int unsigned cov_result_bin;
  bit          last_burst_active;
  int unsigned arb_port_q[$];

  uvm_analysis_imp_csr  #(hist_csr_txn,   hist_coverage) csr_imp;
  uvm_analysis_imp_bin  #(hist_bin_txn,   hist_coverage) bin_imp;
  uvm_analysis_imp_fill #(hist_fill_txn,  hist_coverage) fill_imp;
  uvm_analysis_imp_dbg  #(hist_debug_txn, hist_coverage) dbg_imp;
  uvm_analysis_imp_snoop#(hist_snoop_txn, hist_coverage) snoop_imp;

  covergroup cg_config with function sample(
    bit [3:0] mode,
    bit key_unsigned,
    bit filter_enable,
    bit filter_reject,
    int signed left_bound,
    int unsigned bin_width
  );
    cp_mode : coverpoint mode {
      bins normal  = {4'h0};
      bins debug[] = {4'hA, 4'hB, 4'hC, 4'hD, 4'hE, 4'hF};
      bins other[] = {[4'h1:4'h9]};
    }
    cp_key_unsigned : coverpoint key_unsigned {
      bins signed_mode   = {1'b0};
      bins unsigned_mode = {1'b1};
    }
    cp_filter_enable : coverpoint filter_enable {
      bins disabled = {1'b0};
      bins enabled  = {1'b1};
    }
    cp_filter_reject : coverpoint filter_reject {
      bins accept_match = {1'b0};
      bins reject_match = {1'b1};
    }
    cp_left_bound : coverpoint left_bound {
      bins negative = {[$:-1]};
      bins zero     = {0};
      bins positive = {[1:$]};
    }
    cp_bin_width : coverpoint bin_width {
      bins zero      = {0};
      bins one       = {1};
      bins default16 = {16};
      bins narrow    = {[2:15]};
      bins wide      = {[17:$]};
    }
  endgroup

  covergroup cg_bin_result with function sample(
    int unsigned result_kind,
    int unsigned result_bin
  );
    cp_result_kind : coverpoint result_kind {
      bins bin_hit   = {0};
      bins underflow = {1};
      bins overflow  = {2};
    }
    cp_result_bin : coverpoint result_bin {
      bins first = {0};
      bins low   = {[1:63]};
      bins mid   = {[64:191]};
      bins high  = {[192:254]};
      bins last  = {255};
    }
  endgroup

  covergroup cg_port with function sample(
    int unsigned port_index,
    int unsigned debug_index
  );
    cp_port : coverpoint port_index {
      bins ports[] = {[0:HS_N_PORTS-1]};
    }
    cp_debug : coverpoint debug_index {
      bins none  = {7};
      bins debug[] = {[0:HS_N_DEBUG-1]};
    }
  endgroup

  covergroup cg_cross with function sample(
    bit [3:0] mode,
    int unsigned port_index,
    int unsigned result_kind
  );
    cp_mode : coverpoint mode {
      bins normal  = {4'h0};
      bins debug[] = {4'hA, 4'hB, 4'hC, 4'hD, 4'hE, 4'hF};
    }
    cp_port : coverpoint port_index {
      bins ports[] = {[0:HS_N_PORTS-1]};
    }
    cp_result_kind : coverpoint result_kind {
      bins hit      = {0};
      bins under    = {1};
      bins over     = {2};
    }
    x_mode_port_result : cross cp_mode, cp_port, cp_result_kind;
  endgroup

  function new(string name, uvm_component parent);
    super.new(name, parent);
    csr_imp   = new("csr_imp", this);
    bin_imp   = new("bin_imp", this);
    fill_imp  = new("fill_imp", this);
    dbg_imp   = new("dbg_imp", this);
    snoop_imp = new("snoop_imp", this);
    cg_config     = new();
    cg_bin_result = new();
    cg_port       = new();
    cg_cross      = new();
  endfunction

  function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    if (!uvm_config_db#(hist_env_cfg)::get(this, "", "env_cfg", cfg)) begin
      `uvm_fatal(get_type_name(), "missing env_cfg")
    end
    cov_mode          = 4'h0;
    cov_key_unsigned  = 1'b1;
    cov_filter_enable = 1'b0;
    cov_filter_reject = 1'b0;
    cov_left_bound    = HS_DEF_LEFT_BOUND;
    cov_bin_width     = HS_DEF_BIN_WIDTH;
    last_burst_active = 1'b0;
  endfunction

  function void sample_config();
    cg_config.sample(
      cov_mode,
      cov_key_unsigned,
      cov_filter_enable,
      cov_filter_reject,
      cov_left_bound,
      cov_bin_width
    );
  endfunction

  function void write_csr(hist_csr_txn txn);
    if (!txn.write) begin
      return;
    end

    case (txn.address)
      4'd0: begin
        cov_mode          = txn.writedata[7:4];
        cov_key_unsigned  = txn.writedata[8];
        cov_filter_enable = txn.writedata[12];
        cov_filter_reject = txn.writedata[13];
        sample_config();
      end
      4'd1: begin
        cov_left_bound = int'($signed(txn.writedata));
        sample_config();
      end
      4'd3: begin
        cov_bin_width = txn.writedata[15:0];
        sample_config();
      end
      default: begin
      end
    endcase
  endfunction

  function void write_fill(hist_fill_txn txn);
    cg_port.sample(txn.port_index, 7);
  endfunction

  function void write_dbg(hist_debug_txn txn);
    cg_port.sample(0, txn.debug_index);
  endfunction

  function void write_bin(hist_bin_txn txn);
  endfunction

  function void write_snoop(hist_snoop_txn txn);
  endfunction

  task run_phase(uvm_phase phase);
    forever begin
      @(cfg.probe_vif.mon_cb);

      if (cfg.probe_vif.mon_cb.rst) begin
        arb_port_q.delete();
        last_burst_active = 1'b0;
        continue;
      end

      if (cfg.probe_vif.mon_cb.arb_valid) begin
        arb_port_q.push_back(cfg.probe_vif.mon_cb.arb_port);
      end

      if (cfg.probe_vif.mon_cb.divider_valid) begin
        cov_result_kind = cfg.probe_vif.mon_cb.divider_underflow ? 1 :
                          cfg.probe_vif.mon_cb.divider_overflow  ? 2 : 0;
        cov_result_bin  = cfg.probe_vif.mon_cb.divider_bin_index;
        cg_bin_result.sample(cov_result_kind, cov_result_bin);
        if (arb_port_q.size() != 0) begin
          cov_port_index = arb_port_q.pop_front();
        end else begin
          cov_port_index = 0;
        end
        cg_cross.sample(cov_mode, cov_port_index, cov_result_kind);
      end
    end
  endtask
endclass
