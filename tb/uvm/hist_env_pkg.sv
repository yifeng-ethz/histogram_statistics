package hist_env_pkg;
  import uvm_pkg::*;
  `include "uvm_macros.svh"

  `uvm_analysis_imp_decl(_csr)
  `uvm_analysis_imp_decl(_bin)
  `uvm_analysis_imp_decl(_fill)
  `uvm_analysis_imp_decl(_dbg)
  `uvm_analysis_imp_decl(_snoop)

  localparam int HS_N_PORTS         = 8;
  localparam int HS_N_DEBUG         = 6;
  localparam int HS_N_BINS          = 256;
  localparam int HS_AVST_DATA_W     = 39;
  localparam int HS_AVST_CH_W       = 4;
  localparam int HS_DEF_LEFT_BOUND  = -1000;
  localparam int HS_DEF_BIN_WIDTH   = 16;
  localparam int HS_DEF_RIGHT_BOUND = HS_DEF_LEFT_BOUND + (HS_DEF_BIN_WIDTH * HS_N_BINS);
  localparam int HS_DEF_UPDATE_LO   = 17;
  localparam int HS_DEF_UPDATE_HI   = 29;
  localparam int HS_DEF_FILTER_LO   = 35;
  localparam int HS_DEF_FILTER_HI   = 38;
  localparam int HS_DEF_INTERVAL    = 125_000_000;
  localparam int HS_CHANNELS_PER_PORT = 32;
  localparam int HS_FIFO_DEPTH      = 16;
  localparam int HS_BIN_W           = $clog2(HS_N_BINS);
  localparam int HS_PORT_W          = $clog2(HS_N_PORTS);

  typedef enum int unsigned {
    HIST_EVT_BIN       = 0,
    HIST_EVT_UNDERFLOW = 1,
    HIST_EVT_OVERFLOW  = 2
  } hist_event_kind_e;

  function automatic int unsigned hist_sat_add_u32(
    input int unsigned lhs,
    input int unsigned rhs
  );
    longint unsigned sum_v;
    sum_v = longint'(lhs) + longint'(rhs);
    if (sum_v > 32'hFFFF_FFFF) begin
      return 32'hFFFF_FFFF;
    end
    return int'(sum_v[31:0]);
  endfunction

  function automatic int signed hist_mode_to_int(input bit [3:0] mode);
    logic signed [31:0] mode_v;
    mode_v = { {28{mode[3]}}, mode };
    return int'(mode_v);
  endfunction

  function automatic int unsigned hist_extract_unsigned(
    input logic [HS_AVST_DATA_W-1:0] data_word,
    input int unsigned bit_hi,
    input int unsigned bit_lo
  );
    int unsigned result_v;
    int unsigned width_v;
    result_v = 0;
    width_v  = (bit_hi >= bit_lo) ? (bit_hi - bit_lo + 1) : 0;
    for (int idx = 0; idx < width_v; idx++) begin
      if ((bit_lo + idx) < HS_AVST_DATA_W) begin
        result_v[idx] = data_word[bit_lo + idx];
      end
    end
    return result_v;
  endfunction

  function automatic int signed hist_extract_signed(
    input logic [HS_AVST_DATA_W-1:0] data_word,
    input int unsigned bit_hi,
    input int unsigned bit_lo
  );
    int unsigned raw_v;
    int unsigned width_v;
    int signed   result_v;
    raw_v   = hist_extract_unsigned(data_word, bit_hi, bit_lo);
    width_v = (bit_hi >= bit_lo) ? (bit_hi - bit_lo + 1) : 0;
    if ((width_v == 0) || (width_v >= 32)) begin
      result_v = $signed(raw_v);
    end else begin
      result_v = $signed(raw_v << (32 - width_v)) >>> (32 - width_v);
    end
    return result_v;
  endfunction

  function automatic logic [HS_AVST_DATA_W-1:0] hist_build_fill_word(
    input int signed key_value,
    input int unsigned update_lo = HS_DEF_UPDATE_LO,
    input int unsigned update_hi = HS_DEF_UPDATE_HI,
    input int unsigned filter_value = 0,
    input int unsigned filter_lo = HS_DEF_FILTER_LO,
    input int unsigned filter_hi = HS_DEF_FILTER_HI
  );
    logic [HS_AVST_DATA_W-1:0] word_v;
    logic signed [31:0] key_bits_v;
    logic [31:0] filter_bits_v;
    int unsigned key_width_v;
    int unsigned filter_width_v;

    word_v        = '0;
    key_bits_v    = key_value;
    filter_bits_v = filter_value;
    key_width_v   = (update_hi >= update_lo) ? (update_hi - update_lo + 1) : 0;
    filter_width_v = (filter_hi >= filter_lo) ? (filter_hi - filter_lo + 1) : 0;

    for (int idx = 0; idx < key_width_v; idx++) begin
      if ((update_lo + idx) < HS_AVST_DATA_W) begin
        word_v[update_lo + idx] = key_bits_v[idx];
      end
    end

    for (int idx = 0; idx < filter_width_v; idx++) begin
      if ((filter_lo + idx) < HS_AVST_DATA_W) begin
        word_v[filter_lo + idx] = filter_bits_v[idx];
      end
    end

    return word_v;
  endfunction

  class hist_env_cfg extends uvm_object;
    `uvm_object_utils(hist_env_cfg)

    virtual hist_csr_if          csr_vif;
    virtual hist_bin_if          bin_vif;
    virtual hist_fill_if         fill_vifs[HS_N_PORTS];
    virtual hist_ctrl_if         ctrl_vif;
    virtual hist_debug_if        dbg_vifs[HS_N_DEBUG];
    virtual hist_snoop_if        snoop_vif;
    virtual hist_probe_if        probe_vif;
    bit                          enable_scoreboard = 1'b1;
    bit                          enable_coverage   = 1'b1;

    function new(string name = "hist_env_cfg");
      super.new(name);
    endfunction
  endclass

  class hist_cfg_state extends uvm_object;
    `uvm_object_utils(hist_cfg_state)

    bit [3:0]      mode;
    bit            key_unsigned;
    bit            filter_enable;
    bit            filter_reject;
    int signed     left_bound;
    int signed     right_bound;
    bit [15:0]     bin_width;
    bit [7:0]      update_key_low;
    bit [7:0]      update_key_high;
    bit [7:0]      filter_key_low;
    bit [7:0]      filter_key_high;
    bit [15:0]     update_key;
    bit [15:0]     filter_key;
    bit [31:0]     interval_cfg;

    function new(string name = "hist_cfg_state");
      super.new(name);
      reset_defaults();
    endfunction

    function void reset_defaults();
      mode            = 4'h0;
      key_unsigned    = 1'b1;
      filter_enable   = 1'b0;
      filter_reject   = 1'b0;
      left_bound      = HS_DEF_LEFT_BOUND;
      right_bound     = HS_DEF_RIGHT_BOUND;
      bin_width       = HS_DEF_BIN_WIDTH[15:0];
      update_key_low  = HS_DEF_UPDATE_LO[7:0];
      update_key_high = HS_DEF_UPDATE_HI[7:0];
      filter_key_low  = HS_DEF_FILTER_LO[7:0];
      filter_key_high = HS_DEF_FILTER_HI[7:0];
      update_key      = 16'h0000;
      filter_key      = 16'h0000;
      interval_cfg    = HS_DEF_INTERVAL;
    endfunction

    function int signed mode_int();
      return hist_mode_to_int(mode);
    endfunction

    function void do_copy(uvm_object rhs);
      hist_cfg_state rhs_t;
      if (!$cast(rhs_t, rhs)) begin
        return;
      end
      mode            = rhs_t.mode;
      key_unsigned    = rhs_t.key_unsigned;
      filter_enable   = rhs_t.filter_enable;
      filter_reject   = rhs_t.filter_reject;
      left_bound      = rhs_t.left_bound;
      right_bound     = rhs_t.right_bound;
      bin_width       = rhs_t.bin_width;
      update_key_low  = rhs_t.update_key_low;
      update_key_high = rhs_t.update_key_high;
      filter_key_low  = rhs_t.filter_key_low;
      filter_key_high = rhs_t.filter_key_high;
      update_key      = rhs_t.update_key;
      filter_key      = rhs_t.filter_key;
      interval_cfg    = rhs_t.interval_cfg;
    endfunction

    function string convert2string();
      return $sformatf(
        "mode=%0d key_unsigned=%0b filter_en=%0b filter_rej=%0b left=%0d right=%0d bin_width=%0d upd[%0d:%0d]=0x%0h filt[%0d:%0d]=0x%0h interval=%0d",
        mode_int(),
        key_unsigned,
        filter_enable,
        filter_reject,
        left_bound,
        right_bound,
        bin_width,
        update_key_high,
        update_key_low,
        update_key,
        filter_key_high,
        filter_key_low,
        filter_key,
        interval_cfg
      );
    endfunction
  endclass

  class hist_csr_txn extends uvm_sequence_item;
    rand bit         write;
    rand bit [3:0]   address;
    rand bit [31:0]  writedata;
    bit [31:0]       readdata;

    `uvm_object_utils_begin(hist_csr_txn)
      `uvm_field_int(write, UVM_DEFAULT)
      `uvm_field_int(address, UVM_DEFAULT)
      `uvm_field_int(writedata, UVM_DEFAULT)
      `uvm_field_int(readdata, UVM_DEFAULT | UVM_NOCOMPARE)
    `uvm_object_utils_end

    function new(string name = "hist_csr_txn");
      super.new(name);
    endfunction
  endclass

  class hist_bin_txn extends uvm_sequence_item;
    rand bit         write;
    rand bit [7:0]   address;
    rand bit [7:0]   burstcount;
    rand bit [31:0]  writedata;
    bit [31:0]       readdata[$];

    `uvm_object_utils_begin(hist_bin_txn)
      `uvm_field_int(write, UVM_DEFAULT)
      `uvm_field_int(address, UVM_DEFAULT)
      `uvm_field_int(burstcount, UVM_DEFAULT)
      `uvm_field_int(writedata, UVM_DEFAULT)
      `uvm_field_queue_int(readdata, UVM_DEFAULT | UVM_NOCOMPARE)
    `uvm_object_utils_end

    function new(string name = "hist_bin_txn");
      super.new(name);
    endfunction
  endclass

  class hist_fill_txn extends uvm_sequence_item;
    rand bit [2:0]                port_index;
    rand logic [HS_AVST_DATA_W-1:0] data;
    rand bit [HS_AVST_CH_W-1:0]   channel;
    rand bit                      sop;
    rand bit                      eop;
    rand int unsigned             idle_cycles_after;

    `uvm_object_utils_begin(hist_fill_txn)
      `uvm_field_int(port_index, UVM_DEFAULT)
      `uvm_field_int(data, UVM_DEFAULT)
      `uvm_field_int(channel, UVM_DEFAULT)
      `uvm_field_int(sop, UVM_DEFAULT)
      `uvm_field_int(eop, UVM_DEFAULT)
      `uvm_field_int(idle_cycles_after, UVM_DEFAULT)
    `uvm_object_utils_end

    function new(string name = "hist_fill_txn");
      super.new(name);
    endfunction
  endclass

  class hist_ctrl_txn extends uvm_sequence_item;
    rand bit [8:0] data;

    `uvm_object_utils_begin(hist_ctrl_txn)
      `uvm_field_int(data, UVM_DEFAULT)
    `uvm_object_utils_end

    function new(string name = "hist_ctrl_txn");
      super.new(name);
    endfunction
  endclass

  class hist_debug_txn extends uvm_sequence_item;
    rand bit [2:0]  debug_index;
    rand bit [15:0] data;

    `uvm_object_utils_begin(hist_debug_txn)
      `uvm_field_int(debug_index, UVM_DEFAULT)
      `uvm_field_int(data, UVM_DEFAULT)
    `uvm_object_utils_end

    function new(string name = "hist_debug_txn");
      super.new(name);
    endfunction
  endclass

  class hist_snoop_txn extends uvm_sequence_item;
    bit                         valid;
    logic [HS_AVST_DATA_W-1:0]  data;
    bit                         sop;
    bit                         eop;
    bit [HS_AVST_CH_W-1:0]      channel;

    `uvm_object_utils_begin(hist_snoop_txn)
      `uvm_field_int(valid, UVM_DEFAULT)
      `uvm_field_int(data, UVM_DEFAULT)
      `uvm_field_int(sop, UVM_DEFAULT)
      `uvm_field_int(eop, UVM_DEFAULT)
      `uvm_field_int(channel, UVM_DEFAULT)
    `uvm_object_utils_end

    function new(string name = "hist_snoop_txn");
      super.new(name);
    endfunction
  endclass

  class hist_model_item extends uvm_object;
    int signed                key_value;
    bit                       is_debug;
    int unsigned              source_index;
    logic [HS_AVST_DATA_W-1:0] raw_data;

    `uvm_object_utils(hist_model_item)

    function new(string name = "hist_model_item");
      super.new(name);
    endfunction
  endclass

  class hist_expected_evt extends uvm_object;
    hist_event_kind_e kind;
    int signed        effective_key;
    int unsigned      bin_index;
    int unsigned      port_index;

    `uvm_object_utils(hist_expected_evt)

    function new(string name = "hist_expected_evt");
      super.new(name);
      kind          = HIST_EVT_BIN;
      effective_key = 0;
      bin_index     = 0;
      port_index    = 0;
    endfunction
  endclass

  class hist_csr_sequencer extends uvm_sequencer #(hist_csr_txn);
    `uvm_component_utils(hist_csr_sequencer)
    function new(string name, uvm_component parent);
      super.new(name, parent);
    endfunction
  endclass

  class hist_bin_sequencer extends uvm_sequencer #(hist_bin_txn);
    `uvm_component_utils(hist_bin_sequencer)
    function new(string name, uvm_component parent);
      super.new(name, parent);
    endfunction
  endclass

  class hist_fill_sequencer extends uvm_sequencer #(hist_fill_txn);
    `uvm_component_utils(hist_fill_sequencer)
    function new(string name, uvm_component parent);
      super.new(name, parent);
    endfunction
  endclass

  class hist_ctrl_sequencer extends uvm_sequencer #(hist_ctrl_txn);
    `uvm_component_utils(hist_ctrl_sequencer)
    function new(string name, uvm_component parent);
      super.new(name, parent);
    endfunction
  endclass

  class hist_debug_sequencer extends uvm_sequencer #(hist_debug_txn);
    `uvm_component_utils(hist_debug_sequencer)
    function new(string name, uvm_component parent);
      super.new(name, parent);
    endfunction
  endclass

  class hist_csr_driver extends uvm_driver #(hist_csr_txn);
    `uvm_component_utils(hist_csr_driver)

    virtual hist_csr_if vif;

    function new(string name, uvm_component parent);
      super.new(name, parent);
    endfunction

    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      if (!uvm_config_db#(virtual hist_csr_if)::get(this, "", "vif", vif)) begin
        `uvm_fatal(get_type_name(), "missing csr vif")
      end
    endfunction

    task run_phase(uvm_phase phase);
      hist_csr_txn req;
      hist_csr_txn rsp;

      vif.init_master();
      forever begin
        seq_item_port.get_next_item(req);
        rsp = hist_csr_txn::type_id::create("rsp");
        rsp.copy(req);

        @(vif.drv_cb);
        vif.drv_cb.address   <= req.address;
        vif.drv_cb.writedata <= req.writedata;
        vif.drv_cb.write     <= req.write;
        vif.drv_cb.read      <= !req.write;

        @(vif.drv_cb);
        vif.drv_cb.write     <= 1'b0;
        vif.drv_cb.read      <= 1'b0;
        vif.drv_cb.address   <= '0;
        vif.drv_cb.writedata <= '0;

        if (!req.write) begin
          @(vif.drv_cb);
          rsp.readdata = vif.drv_cb.readdata;
        end

        rsp.set_id_info(req);
        seq_item_port.item_done(rsp);
      end
    endtask
  endclass

  class hist_bin_driver extends uvm_driver #(hist_bin_txn);
    `uvm_component_utils(hist_bin_driver)

    virtual hist_bin_if vif;

    function new(string name, uvm_component parent);
      super.new(name, parent);
    endfunction

    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      if (!uvm_config_db#(virtual hist_bin_if)::get(this, "", "vif", vif)) begin
        `uvm_fatal(get_type_name(), "missing bin vif")
      end
    endfunction

    task run_phase(uvm_phase phase);
      hist_bin_txn req;
      hist_bin_txn rsp;
      int unsigned expected_count;

      vif.init_master();
      forever begin
        seq_item_port.get_next_item(req);
        rsp = hist_bin_txn::type_id::create("rsp");
        rsp.copy(req);

        @(vif.drv_cb);
        vif.drv_cb.address    <= req.address;
        vif.drv_cb.writedata  <= req.writedata;
        vif.drv_cb.write      <= req.write;
        vif.drv_cb.read       <= !req.write;
        vif.drv_cb.burstcount <= (req.burstcount == 0) ? 8'd1 : req.burstcount;

        @(vif.drv_cb);
        vif.drv_cb.write      <= 1'b0;
        vif.drv_cb.read       <= 1'b0;
        vif.drv_cb.address    <= '0;
        vif.drv_cb.writedata  <= '0;
        vif.drv_cb.burstcount <= 'd1;

        if (req.write) begin
          do begin
            @(vif.drv_cb);
          end while (!vif.drv_cb.writeresponsevalid);
        end else begin
          expected_count = (req.burstcount == 0) ? 1 : req.burstcount;
          while (rsp.readdata.size() < expected_count) begin
            @(vif.drv_cb);
            if (vif.drv_cb.readdatavalid) begin
              rsp.readdata.push_back(vif.drv_cb.readdata);
            end
          end
        end

        rsp.set_id_info(req);
        seq_item_port.item_done(rsp);
      end
    endtask
  endclass

  class hist_fill_driver extends uvm_driver #(hist_fill_txn);
    `uvm_component_utils(hist_fill_driver)

    virtual hist_fill_if vif;
    int unsigned         port_index;

    function new(string name, uvm_component parent);
      super.new(name, parent);
    endfunction

    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      if (!uvm_config_db#(virtual hist_fill_if)::get(this, "", "vif", vif)) begin
        `uvm_fatal(get_type_name(), "missing fill vif")
      end
      if (!uvm_config_db#(int unsigned)::get(this, "", "port_index", port_index)) begin
        port_index = 0;
      end
    endfunction

    task run_phase(uvm_phase phase);
      hist_fill_txn req;
      hist_fill_txn rsp;

      vif.init_source();
      forever begin
        seq_item_port.get_next_item(req);
        rsp = hist_fill_txn::type_id::create("rsp");
        rsp.copy(req);
        rsp.port_index = port_index[2:0];

        @(vif.drv_cb);
        vif.drv_cb.valid   <= 1'b1;
        vif.drv_cb.data    <= req.data;
        vif.drv_cb.sop     <= req.sop;
        vif.drv_cb.eop     <= req.eop;
        vif.drv_cb.channel <= req.channel;

        do begin
          @(vif.drv_cb);
        end while (!vif.drv_cb.ready);

        vif.drv_cb.valid   <= 1'b0;
        vif.drv_cb.data    <= '0;
        vif.drv_cb.sop     <= 1'b0;
        vif.drv_cb.eop     <= 1'b0;
        vif.drv_cb.channel <= '0;

        repeat (req.idle_cycles_after) begin
          @(vif.drv_cb);
        end

        rsp.set_id_info(req);
        seq_item_port.item_done(rsp);
      end
    endtask
  endclass

  class hist_ctrl_driver extends uvm_driver #(hist_ctrl_txn);
    `uvm_component_utils(hist_ctrl_driver)

    virtual hist_ctrl_if vif;

    function new(string name, uvm_component parent);
      super.new(name, parent);
    endfunction

    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      if (!uvm_config_db#(virtual hist_ctrl_if)::get(this, "", "vif", vif)) begin
        `uvm_fatal(get_type_name(), "missing ctrl vif")
      end
    endfunction

    task run_phase(uvm_phase phase);
      hist_ctrl_txn req;
      hist_ctrl_txn rsp;

      vif.init_source();
      forever begin
        seq_item_port.get_next_item(req);
        rsp = hist_ctrl_txn::type_id::create("rsp");
        rsp.copy(req);

        @(vif.drv_cb);
        vif.drv_cb.data  <= req.data;
        vif.drv_cb.valid <= 1'b1;
        @(vif.drv_cb);
        vif.drv_cb.valid <= 1'b0;
        vif.drv_cb.data  <= '0;

        rsp.set_id_info(req);
        seq_item_port.item_done(rsp);
      end
    endtask
  endclass

  class hist_debug_driver extends uvm_driver #(hist_debug_txn);
    `uvm_component_utils(hist_debug_driver)

    virtual hist_debug_if vif;
    int unsigned          debug_index;

    function new(string name, uvm_component parent);
      super.new(name, parent);
    endfunction

    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      if (!uvm_config_db#(virtual hist_debug_if)::get(this, "", "vif", vif)) begin
        `uvm_fatal(get_type_name(), "missing debug vif")
      end
      if (!uvm_config_db#(int unsigned)::get(this, "", "debug_index", debug_index)) begin
        debug_index = 0;
      end
    endfunction

    task run_phase(uvm_phase phase);
      hist_debug_txn req;
      hist_debug_txn rsp;

      vif.init_source();
      forever begin
        seq_item_port.get_next_item(req);
        rsp = hist_debug_txn::type_id::create("rsp");
        rsp.copy(req);
        rsp.debug_index = debug_index[2:0];

        @(vif.drv_cb);
        vif.drv_cb.valid <= 1'b1;
        vif.drv_cb.data  <= req.data;
        @(vif.drv_cb);
        vif.drv_cb.valid <= 1'b0;
        vif.drv_cb.data  <= '0;

        rsp.set_id_info(req);
        seq_item_port.item_done(rsp);
      end
    endtask
  endclass

  class hist_csr_monitor extends uvm_component;
    `uvm_component_utils(hist_csr_monitor)

    virtual hist_csr_if             vif;
    uvm_analysis_port #(hist_csr_txn) ap;

    function new(string name, uvm_component parent);
      super.new(name, parent);
      ap = new("ap", this);
    endfunction

    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      if (!uvm_config_db#(virtual hist_csr_if)::get(this, "", "vif", vif)) begin
        `uvm_fatal(get_type_name(), "missing csr vif")
      end
    endfunction

    task run_phase(uvm_phase phase);
      hist_csr_txn txn;
      bit          pending_read;
      bit [3:0]    pending_addr;

      pending_read = 1'b0;
      forever begin
        @(vif.mon_cb);
        if (pending_read) begin
          txn = hist_csr_txn::type_id::create("txn");
          txn.write    = 1'b0;
          txn.address  = pending_addr;
          txn.readdata = vif.mon_cb.readdata;
          ap.write(txn);
          pending_read = 1'b0;
        end

        if (vif.mon_cb.write) begin
          txn = hist_csr_txn::type_id::create("txn");
          txn.write     = 1'b1;
          txn.address   = vif.mon_cb.address;
          txn.writedata = vif.mon_cb.writedata;
          ap.write(txn);
        end

        if (vif.mon_cb.read) begin
          pending_read = 1'b1;
          pending_addr = vif.mon_cb.address;
        end
      end
    endtask
  endclass

  class hist_bin_monitor extends uvm_component;
    `uvm_component_utils(hist_bin_monitor)

    virtual hist_bin_if              vif;
    uvm_analysis_port #(hist_bin_txn) ap;

    function new(string name, uvm_component parent);
      super.new(name, parent);
      ap = new("ap", this);
    endfunction

    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      if (!uvm_config_db#(virtual hist_bin_if)::get(this, "", "vif", vif)) begin
        `uvm_fatal(get_type_name(), "missing bin vif")
      end
    endfunction

    task run_phase(uvm_phase phase);
      hist_bin_txn txn;
      hist_bin_txn burst_txn;
      int unsigned expected_count;

      expected_count = 0;
      forever begin
        @(vif.mon_cb);
        if (vif.mon_cb.write) begin
          txn = hist_bin_txn::type_id::create("txn");
          txn.write     = 1'b1;
          txn.address   = vif.mon_cb.address;
          txn.writedata = vif.mon_cb.writedata;
          txn.burstcount = vif.mon_cb.burstcount;
          ap.write(txn);
        end

        if (vif.mon_cb.read) begin
          burst_txn = hist_bin_txn::type_id::create("burst_txn");
          burst_txn.write      = 1'b0;
          burst_txn.address    = vif.mon_cb.address;
          burst_txn.burstcount = (vif.mon_cb.burstcount == 0) ? 8'd1 : vif.mon_cb.burstcount;
          expected_count       = burst_txn.burstcount;
        end

        if ((expected_count != 0) && vif.mon_cb.readdatavalid) begin
          burst_txn.readdata.push_back(vif.mon_cb.readdata);
          if (burst_txn.readdata.size() == expected_count) begin
            ap.write(burst_txn);
            expected_count = 0;
          end
        end
      end
    endtask
  endclass

  class hist_fill_monitor extends uvm_component;
    `uvm_component_utils(hist_fill_monitor)

    virtual hist_fill_if               vif;
    int unsigned                       port_index;
    uvm_analysis_port #(hist_fill_txn) ap;

    function new(string name, uvm_component parent);
      super.new(name, parent);
      ap = new("ap", this);
    endfunction

    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      if (!uvm_config_db#(virtual hist_fill_if)::get(this, "", "vif", vif)) begin
        `uvm_fatal(get_type_name(), "missing fill vif")
      end
      if (!uvm_config_db#(int unsigned)::get(this, "", "port_index", port_index)) begin
        port_index = 0;
      end
    endfunction

    task run_phase(uvm_phase phase);
      hist_fill_txn txn;

      forever begin
        @(vif.mon_cb);
        if (vif.mon_cb.valid && vif.mon_cb.ready) begin
          txn = hist_fill_txn::type_id::create("txn");
          txn.port_index = port_index[2:0];
          txn.data       = vif.mon_cb.data;
          txn.channel    = vif.mon_cb.channel;
          txn.sop        = vif.mon_cb.sop;
          txn.eop        = vif.mon_cb.eop;
          txn.idle_cycles_after = 0;
          ap.write(txn);
        end
      end
    endtask
  endclass

  class hist_debug_monitor extends uvm_component;
    `uvm_component_utils(hist_debug_monitor)

    virtual hist_debug_if              vif;
    int unsigned                       debug_index;
    uvm_analysis_port #(hist_debug_txn) ap;

    function new(string name, uvm_component parent);
      super.new(name, parent);
      ap = new("ap", this);
    endfunction

    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      if (!uvm_config_db#(virtual hist_debug_if)::get(this, "", "vif", vif)) begin
        `uvm_fatal(get_type_name(), "missing debug vif")
      end
      if (!uvm_config_db#(int unsigned)::get(this, "", "debug_index", debug_index)) begin
        debug_index = 0;
      end
    endfunction

    task run_phase(uvm_phase phase);
      hist_debug_txn txn;

      forever begin
        @(vif.mon_cb);
        if (vif.mon_cb.valid) begin
          txn = hist_debug_txn::type_id::create("txn");
          txn.debug_index = debug_index[2:0];
          txn.data        = vif.mon_cb.data;
          ap.write(txn);
        end
      end
    endtask
  endclass

  class hist_snoop_monitor extends uvm_component;
    `uvm_component_utils(hist_snoop_monitor)

    virtual hist_snoop_if             vif;
    uvm_analysis_port #(hist_snoop_txn) ap;

    function new(string name, uvm_component parent);
      super.new(name, parent);
      ap = new("ap", this);
    endfunction

    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      if (!uvm_config_db#(virtual hist_snoop_if)::get(this, "", "vif", vif)) begin
        `uvm_fatal(get_type_name(), "missing snoop vif")
      end
    endfunction

    task run_phase(uvm_phase phase);
      hist_snoop_txn txn;

      forever begin
        @(vif.mon_cb);
        if (vif.mon_cb.valid && vif.mon_cb.ready) begin
          txn = hist_snoop_txn::type_id::create("txn");
          txn.valid   = 1'b1;
          txn.data    = vif.mon_cb.data;
          txn.sop     = vif.mon_cb.sop;
          txn.eop     = vif.mon_cb.eop;
          txn.channel = vif.mon_cb.channel;
          ap.write(txn);
        end
      end
    endtask
  endclass

  class hist_csr_agent extends uvm_agent;
    `uvm_component_utils(hist_csr_agent)

    hist_csr_sequencer seqr;
    hist_csr_driver    drv;
    hist_csr_monitor   mon;

    function new(string name, uvm_component parent);
      super.new(name, parent);
    endfunction

    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      mon = hist_csr_monitor::type_id::create("mon", this);
      if (get_is_active() == UVM_ACTIVE) begin
        seqr = hist_csr_sequencer::type_id::create("seqr", this);
        drv  = hist_csr_driver::type_id::create("drv", this);
      end
    endfunction

    function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);
      if (get_is_active() == UVM_ACTIVE) begin
        drv.seq_item_port.connect(seqr.seq_item_export);
      end
    endfunction
  endclass

  class hist_bin_agent extends uvm_agent;
    `uvm_component_utils(hist_bin_agent)

    hist_bin_sequencer seqr;
    hist_bin_driver    drv;
    hist_bin_monitor   mon;

    function new(string name, uvm_component parent);
      super.new(name, parent);
    endfunction

    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      mon = hist_bin_monitor::type_id::create("mon", this);
      if (get_is_active() == UVM_ACTIVE) begin
        seqr = hist_bin_sequencer::type_id::create("seqr", this);
        drv  = hist_bin_driver::type_id::create("drv", this);
      end
    endfunction

    function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);
      if (get_is_active() == UVM_ACTIVE) begin
        drv.seq_item_port.connect(seqr.seq_item_export);
      end
    endfunction
  endclass

  class hist_fill_agent extends uvm_agent;
    `uvm_component_utils(hist_fill_agent)

    hist_fill_sequencer seqr;
    hist_fill_driver    drv;
    hist_fill_monitor   mon;

    function new(string name, uvm_component parent);
      super.new(name, parent);
    endfunction

    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      mon = hist_fill_monitor::type_id::create("mon", this);
      if (get_is_active() == UVM_ACTIVE) begin
        seqr = hist_fill_sequencer::type_id::create("seqr", this);
        drv  = hist_fill_driver::type_id::create("drv", this);
      end
    endfunction

    function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);
      if (get_is_active() == UVM_ACTIVE) begin
        drv.seq_item_port.connect(seqr.seq_item_export);
      end
    endfunction
  endclass

  class hist_ctrl_agent extends uvm_agent;
    `uvm_component_utils(hist_ctrl_agent)

    hist_ctrl_sequencer seqr;
    hist_ctrl_driver    drv;

    function new(string name, uvm_component parent);
      super.new(name, parent);
    endfunction

    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      if (get_is_active() == UVM_ACTIVE) begin
        seqr = hist_ctrl_sequencer::type_id::create("seqr", this);
        drv  = hist_ctrl_driver::type_id::create("drv", this);
      end
    endfunction

    function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);
      if (get_is_active() == UVM_ACTIVE) begin
        drv.seq_item_port.connect(seqr.seq_item_export);
      end
    endfunction
  endclass

  class hist_debug_agent extends uvm_agent;
    `uvm_component_utils(hist_debug_agent)

    hist_debug_sequencer seqr;
    hist_debug_driver    drv;
    hist_debug_monitor   mon;

    function new(string name, uvm_component parent);
      super.new(name, parent);
    endfunction

    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      mon = hist_debug_monitor::type_id::create("mon", this);
      if (get_is_active() == UVM_ACTIVE) begin
        seqr = hist_debug_sequencer::type_id::create("seqr", this);
        drv  = hist_debug_driver::type_id::create("drv", this);
      end
    endfunction

    function void connect_phase(uvm_phase phase);
      super.connect_phase(phase);
      if (get_is_active() == UVM_ACTIVE) begin
        drv.seq_item_port.connect(seqr.seq_item_export);
      end
    endfunction
  endclass

  class hist_bin_burst_read_seq extends uvm_sequence #(hist_bin_txn);
    `uvm_object_utils(hist_bin_burst_read_seq)

    rand bit [7:0] address;
    rand bit [7:0] burstcount;
    bit [31:0]     readdata[$];

    function new(string name = "hist_bin_burst_read_seq");
      super.new(name);
    endfunction

    task body();
      hist_bin_txn req;
      hist_bin_txn rsp;

      req = hist_bin_txn::type_id::create("req");
      start_item(req);
      req.write      = 1'b0;
      req.address    = address;
      req.burstcount = burstcount;
      req.writedata  = '0;
      finish_item(req);

      get_response(rsp);
      readdata = rsp.readdata;
    endtask
  endclass

  class hist_bin_clear_seq extends uvm_sequence #(hist_bin_txn);
    `uvm_object_utils(hist_bin_clear_seq)

    rand bit [7:0] address;

    function new(string name = "hist_bin_clear_seq");
      super.new(name);
      address = '0;
    endfunction

    task body();
      hist_bin_txn req;
      req = hist_bin_txn::type_id::create("req");
      start_item(req);
      req.write      = 1'b1;
      req.address    = address;
      req.burstcount = 8'd1;
      req.writedata  = 32'h0000_0000;
      finish_item(req);
    endtask
  endclass

  `include "hist_scoreboard.sv"
  `include "hist_coverage.sv"
  `include "hist_env.sv"
  `include "sequences/hist_fill_seq.sv"
  `include "sequences/hist_csr_seq.sv"
  `include "hist_base_test.sv"
  `include "tests/hist_smoke_test.sv"
  `include "tests/hist_single_hit_test.sv"
  `include "tests/hist_csr_test.sv"
  `include "tests/hist_cfg_test.sv"
  `include "tests/hist_key_test.sv"
  `include "tests/hist_filter_test.sv"
  `include "tests/hist_bin_test.sv"
  `include "tests/hist_multiport_test.sv"
  `include "tests/hist_bank_test.sv"
  `include "tests/hist_coalesce_test.sv"
  `include "tests/hist_snoop_test.sv"
  `include "tests/hist_debug_test.sv"
  `include "tests/hist_stats_test.sv"
  `include "tests/hist_burst_test.sv"

  // DV_EDGE tests
  `include "tests/hist_edge_divider_test.sv"
  `include "tests/hist_edge_key_test.sv"
  `include "tests/hist_edge_stats_test.sv"
  `include "tests/hist_edge_csr_test.sv"
  `include "tests/hist_edge_filter_test.sv"
  `include "tests/hist_edge_interval_test.sv"
  `include "tests/hist_edge_config_test.sv"
  `include "tests/hist_edge_fifo_test.sv"
  `include "tests/hist_edge_queue_test.sv"
  `include "tests/hist_edge_sram_test.sv"
  `include "tests/hist_edge_arbiter_test.sv"
  `include "tests/hist_edge_ingress_test.sv"
  `include "tests/hist_edge_ram_test.sv"
  `include "tests/hist_edge_pkg_test.sv"

  // DV_ERROR tests
  `include "tests/hist_error_reset_test.sv"
  `include "tests/hist_error_clear_test.sv"
  `include "tests/hist_error_config_test.sv"
  `include "tests/hist_error_fifo_test.sv"
  `include "tests/hist_error_queue_test.sv"
  `include "tests/hist_error_pipeline_test.sv"
  `include "tests/hist_error_illegal_test.sv"
  `include "tests/hist_error_compound_test.sv"

  // DV_PROF tests
  `include "tests/hist_prof_spt_test.sv"
  `include "tests/hist_prof_mpt_test.sv"
  `include "tests/hist_prof_coe_test.sv"
  `include "tests/hist_prof_pps_test.sv"
  `include "tests/hist_prof_bpc_test.sv"
  `include "tests/hist_prof_sok_test.sv"
  `include "tests/hist_prof_brp_test.sv"
  `include "tests/hist_prof_ccl_test.sv"
  `include "tests/hist_prof_sed_test.sv"
  `include "tests/hist_prof_mia_test.sv"
  `include "tests/hist_prof_pib_test.sv"
  `include "tests/hist_prof_qst_test.sv"

  // DV_CROSS tests
  `include "tests/hist_cross_test.sv"

endpackage
