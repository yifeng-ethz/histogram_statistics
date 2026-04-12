class hist_csr_test extends hist_base_test;
  `uvm_component_utils(hist_csr_test)

  localparam bit [4:0] HS_CSR_CONTROL_ADDR         = 5'd2;
  localparam bit [4:0] HS_CSR_LEFT_BOUND_ADDR      = 5'd3;
  localparam bit [4:0] HS_CSR_RIGHT_BOUND_ADDR     = 5'd4;
  localparam bit [4:0] HS_CSR_BIN_WIDTH_ADDR       = 5'd5;
  localparam bit [4:0] HS_CSR_KEY_FILTER_BITS_ADDR = 5'd6;
  localparam bit [4:0] HS_CSR_KEY_FILTER_VAL_ADDR  = 5'd7;
  localparam bit [4:0] HS_CSR_UNDERFLOW_CNT_ADDR   = 5'd8;
  localparam bit [4:0] HS_CSR_OVERFLOW_CNT_ADDR    = 5'd9;
  localparam bit [4:0] HS_CSR_INTERVAL_CFG_ADDR    = 5'd10;
  localparam bit [4:0] HS_CSR_BANK_STATUS_ADDR     = 5'd11;
  localparam bit [4:0] HS_CSR_PORT_STATUS_ADDR     = 5'd12;
  localparam bit [4:0] HS_CSR_TOTAL_HITS_ADDR      = 5'd13;
  localparam bit [4:0] HS_CSR_DROPPED_HITS_ADDR    = 5'd14;
  localparam bit [4:0] HS_CSR_VERSION_ADDR         = 5'd1;
  localparam bit [4:0] HS_CSR_COAL_STATUS_ADDR     = 5'd15;
  localparam bit [4:0] HS_CSR_SCRATCH_ADDR         = 5'd16;

  localparam bit [31:0] HS_B013_CONTROL_DEFAULT    = 32'h0000_0100;
  localparam bit [31:0] HS_B014_LEFT_BOUND_VALUE   = 32'hFFFF_FC18;
  localparam bit [31:0] HS_B015_RIGHT_BOUND_VALUE  = 32'h0000_0F00;
  localparam bit [31:0] HS_B016_BIN_WIDTH_VALUE    = 32'h0000_0020;
  localparam bit [31:0] HS_B017_KEY_BITS_VALUE     = 32'h2623_1D11;
  localparam bit [31:0] HS_B018_KEY_VALUE_VALUE    = 32'h000F_000A;
  localparam bit [31:0] HS_B021_INTERVAL_CFG_VALUE = 32'h000F_4240;
  localparam bit [31:0] HS_B022_BANK_STATUS_MASK   = 32'h0000_0003;
  localparam bit [31:0] HS_B023_PORT_STATUS_MASK   = 32'h0000_00FF;
  localparam bit [31:0] HS_B023_PORT_STATUS_VALUE  = 32'h0000_00FF;
  localparam bit [31:0] HS_B026_VERSION_VALUE      = 32'h1A00_0000;
  localparam bit [31:0] HS_B028_SCRATCH_VALUE      = 32'hDEAD_BEEF;

  function new(string name, uvm_component parent);
    super.new(name, parent);
  endfunction

  local task automatic check_csr_equal(
    input string     case_id,
    input bit [4:0]  address,
    input bit [31:0] expected_value,
    input string     csr_name
  );
    bit [31:0] actual_value;

    csr_read(address, actual_value);
    if (actual_value !== expected_value) begin
      `uvm_error(case_id, $sformatf("%s expected 0x%08h got 0x%08h", csr_name, expected_value, actual_value))
    end
  endtask

  local task automatic check_csr_mask(
    input string     case_id,
    input bit [4:0]  address,
    input bit [31:0] mask,
    input bit [31:0] expected_value,
    input string     csr_name
  );
    bit [31:0] actual_value;

    csr_read(address, actual_value);
    if ((actual_value & mask) !== expected_value) begin
      `uvm_error(
        case_id,
        $sformatf(
          "%s mask 0x%08h expected 0x%08h got 0x%08h",
          csr_name,
          mask,
          expected_value,
          (actual_value & mask)
        )
      )
    end
  endtask

  local task automatic write_and_check_csr(
    input string     case_id,
    input bit [4:0]  address,
    input bit [31:0] write_value,
    input bit [31:0] expected_value,
    input string     csr_name
  );
    csr_write(address, write_value);
    check_csr_equal(case_id, address, expected_value, csr_name);
  endtask

  local task automatic task_b013();
    `uvm_info(get_type_name(), "B013: read CONTROL at reset default", UVM_LOW)
    check_csr_equal("B013", HS_CSR_CONTROL_ADDR, HS_B013_CONTROL_DEFAULT, "control");
  endtask

  local task automatic task_b014();
    `uvm_info(get_type_name(), "B014: write and read LEFT_BOUND", UVM_LOW)
    write_and_check_csr("B014", HS_CSR_LEFT_BOUND_ADDR, HS_B014_LEFT_BOUND_VALUE, HS_B014_LEFT_BOUND_VALUE, "left_bound");
  endtask

  local task automatic task_b015();
    `uvm_info(get_type_name(), "B015: write and read RIGHT_BOUND", UVM_LOW)
    write_and_check_csr("B015", HS_CSR_RIGHT_BOUND_ADDR, HS_B015_RIGHT_BOUND_VALUE, HS_B015_RIGHT_BOUND_VALUE, "right_bound");
  endtask

  local task automatic task_b016();
    `uvm_info(get_type_name(), "B016: write and read BIN_WIDTH", UVM_LOW)
    write_and_check_csr("B016", HS_CSR_BIN_WIDTH_ADDR, HS_B016_BIN_WIDTH_VALUE, HS_B016_BIN_WIDTH_VALUE, "bin_width");
  endtask

  local task automatic task_b017();
    `uvm_info(get_type_name(), "B017: write and read KEY_FILTER_BITS", UVM_LOW)
    write_and_check_csr("B017", HS_CSR_KEY_FILTER_BITS_ADDR, HS_B017_KEY_BITS_VALUE, HS_B017_KEY_BITS_VALUE, "key_filter_bits");
  endtask

  local task automatic task_b018();
    `uvm_info(get_type_name(), "B018: write and read KEY_FILTER_VAL", UVM_LOW)
    write_and_check_csr("B018", HS_CSR_KEY_FILTER_VAL_ADDR, HS_B018_KEY_VALUE_VALUE, HS_B018_KEY_VALUE_VALUE, "key_filter_val");
  endtask

  local task automatic task_b019();
    `uvm_info(get_type_name(), "B019: read UNDERFLOW_CNT at idle", UVM_LOW)
    check_csr_equal("B019", HS_CSR_UNDERFLOW_CNT_ADDR, 32'd0, "underflow_cnt");
  endtask

  local task automatic task_b020();
    `uvm_info(get_type_name(), "B020: read OVERFLOW_CNT at idle", UVM_LOW)
    check_csr_equal("B020", HS_CSR_OVERFLOW_CNT_ADDR, 32'd0, "overflow_cnt");
  endtask

  local task automatic task_b021();
    `uvm_info(get_type_name(), "B021: write and read INTERVAL_CFG", UVM_LOW)
    if (HS_B021_INTERVAL_CFG_VALUE < 32'd1024) begin
      `uvm_fatal(get_type_name(), $sformatf("interval_cfg must be >= 1024, got %0d", HS_B021_INTERVAL_CFG_VALUE))
    end
    write_and_check_csr("B021", HS_CSR_INTERVAL_CFG_ADDR, HS_B021_INTERVAL_CFG_VALUE, HS_B021_INTERVAL_CFG_VALUE, "interval_cfg");
  endtask

  local task automatic task_b022();
    `uvm_info(get_type_name(), "B022: read BANK_STATUS at idle", UVM_LOW)
    check_csr_mask("B022", HS_CSR_BANK_STATUS_ADDR, HS_B022_BANK_STATUS_MASK, 32'd0, "bank_status");
  endtask

  local task automatic task_b023();
    `uvm_info(get_type_name(), "B023: read PORT_STATUS at idle", UVM_LOW)
    check_csr_mask("B023", HS_CSR_PORT_STATUS_ADDR, HS_B023_PORT_STATUS_MASK, HS_B023_PORT_STATUS_VALUE, "port_status");
  endtask

  local task automatic task_b024();
    `uvm_info(get_type_name(), "B024: read TOTAL_HITS at idle", UVM_LOW)
    check_csr_equal("B024", HS_CSR_TOTAL_HITS_ADDR, 32'd0, "total_hits");
  endtask

  local task automatic task_b025();
    `uvm_info(get_type_name(), "B025: read DROPPED_HITS at idle", UVM_LOW)
    check_csr_equal("B025", HS_CSR_DROPPED_HITS_ADDR, 32'd0, "dropped_hits");
  endtask

  local task automatic task_b026();
    `uvm_info(get_type_name(), "B026: read VERSION", UVM_LOW)
    check_csr_equal("B026", HS_CSR_VERSION_ADDR, HS_B026_VERSION_VALUE, "version");
  endtask

  local task automatic task_b027();
    `uvm_info(get_type_name(), "B027: read COAL_STATUS at idle", UVM_LOW)
    check_csr_equal("B027", HS_CSR_COAL_STATUS_ADDR, 32'd0, "coal_status");
  endtask

  local task automatic task_b028();
    `uvm_info(get_type_name(), "B028: write and read SCRATCH", UVM_LOW)
    write_and_check_csr("B028", HS_CSR_SCRATCH_ADDR, HS_B028_SCRATCH_VALUE, HS_B028_SCRATCH_VALUE, "scratch");
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);

    wait_reset_release();
    wait_initial_clear();

    task_b013();
    task_b014();
    task_b015();
    task_b016();
    task_b017();
    task_b018();
    task_b019();
    task_b020();
    task_b021();
    task_b022();
    task_b023();
    task_b024();
    task_b025();
    task_b026();
    task_b027();
    task_b028();

    phase.drop_objection(this);
  endtask
endclass
