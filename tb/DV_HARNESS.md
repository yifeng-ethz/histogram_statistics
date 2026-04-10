# DV Harness: histogram_statistics_v2 UVM Environment

**Parent:** [DV_PLAN.md](DV_PLAN.md)
**Date:** 2026-04-09
**Status:** Planning

---

## 1. Environment Topology

```
┌─────────────────────────────────────────────────────────────────────┐
│                        hist_env (uvm_env)                           │
│                                                                     │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────────────────┐  │
│  │ hist_csr_agt │  │ hist_bin_agt │  │ hist_fill_agt (×8 ports) │  │
│  │  (AVMM CSR)  │  │ (AVMM burst) │  │   (AVST fill_in)         │  │
│  │ drv|mon|sqr  │  │ drv|mon|sqr  │  │   drv|mon|sqr per port   │  │
│  └──────┬───────┘  └──────┬───────┘  └────────────┬─────────────┘  │
│         │                 │                        │                │
│         v                 v                        v                │
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │                 hist_scoreboard (uvm_scoreboard)             │    │
│  │  - Software reference histogram model                       │    │
│  │  - Tracks: config shadow, per-bin counts, stats counters    │    │
│  │  - Compare: on burst read completion or interval boundary   │    │
│  └─────────────────────────────────────────────────────────────┘    │
│                                                                     │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────────────────┐  │
│  │ hist_ctrl_agt│  │ hist_dbg_agt │  │ hist_snoop_mon           │  │
│  │ (AVST ctrl)  │  │ (×6 debug)   │  │ (AVST fill_out passive)  │  │
│  │ drv|sqr      │  │ drv|sqr      │  │ mon only                 │  │
│  └──────────────┘  └──────────────┘  └──────────────────────────┘  │
│                                                                     │
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │                 hist_coverage (uvm_subscriber)               │    │
│  │  - Functional covergroups per DV_CROSS.md                   │    │
│  │  - Sampled from monitor transactions                        │    │
│  └─────────────────────────────────────────────────────────────┘    │
│                                                                     │
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │                 SVA bind modules                             │    │
│  │  hist_avmm_sva | hist_avst_sva | hist_fifo_sva |            │    │
│  │  hist_pipeline_sva                                          │    │
│  └─────────────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────────────┘
```

---

## 2. Transaction Classes

### 2.1 `hist_csr_txn` (AVMM CSR transaction)

```systemverilog
class hist_csr_txn extends uvm_sequence_item;
    rand bit        rw;           // 0=read, 1=write
    rand bit [3:0]  address;
    rand bit [31:0] writedata;
    bit [31:0]      readdata;     // filled by monitor on read
    // Constraints
    constraint c_addr { address inside {[0:15]}; }
endclass
```

### 2.2 `hist_bin_txn` (AVMM hist_bin transaction)

```systemverilog
class hist_bin_txn extends uvm_sequence_item;
    rand bit        rw;           // 0=burst_read, 1=write (measure_clear)
    rand bit [7:0]  address;
    rand bit [7:0]  burstcount;   // 1..256 for reads
    rand bit [31:0] writedata;    // 0x00000000 for measure_clear
    bit [31:0]      readdata[];   // filled by monitor
    // Constraints
    constraint c_burst { burstcount inside {[1:256]}; }
    constraint c_clear { rw == 1 -> writedata == 32'h0; }
endclass
```

### 2.3 `hist_fill_txn` (AVST fill_in transaction)

```systemverilog
class hist_fill_txn extends uvm_sequence_item;
    rand bit [2:0]  port_index;   // 0..7
    rand bit [38:0] data;         // AVST_DATA_WIDTH=39
    rand bit [3:0]  channel;
    rand bit        sop;
    rand bit        eop;
    // Derived fields (computed by scoreboard)
    int             expected_key;
    int             expected_bin;
    bit             expected_filtered;
endclass
```

### 2.4 `hist_ctrl_txn` (AVST ctrl transaction)

```systemverilog
class hist_ctrl_txn extends uvm_sequence_item;
    rand bit [8:0] data;          // run control word
endclass
```

### 2.5 `hist_config_txn` (virtual transaction for scoreboard)

```systemverilog
class hist_config_txn extends uvm_sequence_item;
    // Shadow of all RW CSR fields — used by scoreboard to track config state
    bit [3:0]  mode;
    bit        key_unsigned;
    bit        filter_enable, filter_reject;
    int        left_bound;
    int        right_bound;
    bit [15:0] bin_width;
    bit [7:0]  update_key_lo, update_key_hi;
    bit [7:0]  filter_key_lo, filter_key_hi;
    bit [15:0] update_key, filter_key;
    bit [31:0] interval_cfg;
endclass
```

---

## 3. Agent Architecture

### 3.1 AVMM CSR Agent (`hist_csr_agent`)

| Component | Role |
|-----------|------|
| `hist_csr_driver` | Drives `avs_csr_write`, `avs_csr_writedata`, `avs_csr_address`, `avs_csr_read`. Waits for `avs_csr_waitrequest` (always 0 in this DUT). |
| `hist_csr_monitor` | Samples bus on each clock. On `avs_csr_read`, captures `avs_csr_readdata` one cycle later (registered output). Publishes `hist_csr_txn` to analysis port. |
| `hist_csr_sequencer` | Standard UVM sequencer. |

**Key detail:** CSR readdata is registered (1-cycle latency). The monitor must sample readdata on the cycle AFTER `avs_csr_read` is asserted.

### 3.2 AVMM Hist-Bin Agent (`hist_bin_agent`)

| Component | Role |
|-----------|------|
| `hist_bin_driver` | Drives burst read (`avs_hist_bin_read`, `_address`, `_burstcount`) and measure_clear write (`avs_hist_bin_write`, `_writedata`=0). |
| `hist_bin_monitor` | Counts `avs_hist_bin_readdatavalid` pulses, collects readdata into burst array. Publishes complete `hist_bin_txn` when burst is done. |
| `hist_bin_sequencer` | Standard UVM sequencer. |

**Key detail:** Burst reads may be deferred if the update pipeline is active (non-pingpong mode). The driver must not assume immediate start.

### 3.3 AVST Fill Agent (`hist_fill_agent`)

One agent instance per port (8 total), or a single agent with port selection.

| Component | Role |
|-----------|------|
| `hist_fill_driver` | Drives `asi_fill_in_N_valid`, `_data`, `_channel`, `_sop`, `_eop`. Respects `_ready` handshake. |
| `hist_fill_monitor` | Samples accepted transfers (valid & ready). Publishes `hist_fill_txn`. |
| `hist_fill_sequencer` | Standard UVM sequencer. |

**Key detail:** Port 0 with SNOOP_EN: ready comes from `aso_hist_fill_out_ready`, not from the DUT's internal logic. The driver must handle backpressure from the snoop consumer.

### 3.4 AVST Ctrl Agent (`hist_ctrl_agent`)

Driver-only (no monitor needed for stimulus-only interface).

### 3.5 Debug Agent (`hist_dbg_agent`)

6 instances, one per debug interface. Driver-only. Activated only when cfg_mode < 0.

### 3.6 Snoop Monitor (`hist_snoop_mon`)

Passive monitor on `aso_hist_fill_out`. Verifies passthrough matches port 0 input when SNOOP_EN.

---

## 4. Scoreboard Architecture

### 4.1 Reference Model

The scoreboard maintains a software-equivalent histogram:

```
class hist_scoreboard extends uvm_scoreboard;
    // Active config (shadows cfg_apply mechanism)
    hist_config_txn active_cfg;
    bit             apply_pending;

    // Per-bin counters (256 bins, 32-bit)
    int unsigned    ref_bins[256];

    // Stats counters
    int unsigned    ref_total_hits;
    int unsigned    ref_dropped_hits;
    int unsigned    ref_underflow_count;
    int unsigned    ref_overflow_count;

    // Methods
    function void process_fill(hist_fill_txn txn);
        // 1. Check filter: match_filter() equivalent
        // 2. Extract key: build_key() equivalent
        // 3. Add port offset: key += port_index * CHANNELS_PER_PORT
        // 4. Compute bin: restoring division (key - left_bound) / bin_width
        // 5. Check bounds: underflow if key < left_bound, overflow if key >= right_bound
        // 6. Update ref_bins[bin_index]
    endfunction

    function void compare_bins(hist_bin_txn burst);
        // Compare burst.readdata[] against ref_bins[]
        // Report mismatches as UVM_ERROR
    endfunction
endclass
```

### 4.2 Comparison Points

| Trigger | What is Compared |
|---------|-----------------|
| Burst read complete | `burst.readdata[i]` vs `ref_bins[addr+i]` for each word in burst |
| CSR read of TOTAL_HITS | `readdata` vs `ref_total_hits` |
| CSR read of DROPPED_HITS | `readdata` vs `ref_dropped_hits` |
| CSR read of UNDERFLOW_CNT | `readdata` vs `ref_underflow_count` |
| CSR read of OVERFLOW_CNT | `readdata` vs `ref_overflow_count` |
| Interval pulse (observed) | All ref counters reset to 0 (matching RTL behavior) |

### 4.3 Known Modeling Complexities

1. **Port offset**: The RTL adds `port_index * 32` to each key in `divider_pipe`. The scoreboard must replicate this.
2. **Stats reset on interval/clear**: Stats counters reset one cycle after `measure_clear_pulse` or `interval_pulse`. The scoreboard must track this timing.
3. **Coalescing**: Multiple hits to the same bin within a short window are coalesced. The scoreboard doesn't need to model coalescing — it just increments `ref_bins` per accepted hit. The coalescing is transparent (same final count).
4. **FIFO drop**: When `fifo_full` and a new hit arrives, `drop_pulse` fires. The scoreboard must track this per-port and NOT increment `ref_bins` for dropped hits.
5. **Signed vs unsigned key**: `build_key()` has two paths. The scoreboard must replicate the sign-extension logic exactly.

---

## 5. Functional Coverage (`hist_coverage`)

Detailed covergroups are specified in [DV_CROSS.md](DV_CROSS.md). Summary of coverpoint categories:

| Category | What is Covered |
|----------|----------------|
| CSR access | All 16 addresses × read/write |
| Config space | mode, key_unsigned, filter_enable/reject, bin_width values |
| Key range | Key values at bounds, mid-range, min, max |
| Bin mapping | Bin indices 0, 1, ..., N_BINS-1, underflow, overflow |
| Port activity | Per-port hit injection, multi-port simultaneous |
| Queue state | Occupancy levels: empty, low, mid, high, full, overflow |
| Bank state | Active bank 0/1, flushing, not flushing |
| Pipeline state | FIFO full/empty per port, arbiter grant per port |

---

## 6. SVA Modules

All SVA modules are `bind`-instantiated in `tb_top.sv`.

### 6.1 `hist_avmm_sva`

| Assertion | Property |
|-----------|----------|
| `csr_no_waitrequest` | `avs_csr_waitrequest == 0` always (DUT declares waitrequest=0) |
| `csr_read_latency_1` | After `avs_csr_read`, `csr_readdata_reg` updates on next posedge |
| `bin_no_waitrequest` | `avs_hist_bin_waitrequest == 0` always |
| `bin_burst_count` | Burst read produces exactly `burstcount` readdatavalid pulses |
| `bin_no_xz_readdata` | `avs_hist_bin_readdata` has no X/Z when `readdatavalid` is high |
| `csr_no_xz_readdata` | `avs_csr_readdata` has no X/Z after reset deasserts |

### 6.2 `hist_avst_sva`

| Assertion | Property |
|-----------|----------|
| `fill_valid_stable` | `valid` does not deassert while `ready=0` (Avalon-ST rule) |
| `fill_data_stable` | `data`, `channel`, `sop`, `eop` stable while `valid=1 && ready=0` |
| `snoop_matches_input` | When SNOOP_EN, `fill_out_data == fill_in_data` on accepted transfers |
| `ctrl_always_ready` | `asi_ctrl_ready == 1` always (DUT never backpressures ctrl) |

### 6.3 `hist_fifo_sva`

| Assertion | Property |
|-----------|----------|
| `fifo_no_silent_overflow` | `fifo_full && fifo_write → drop_pulse` (no silent data loss) |
| `fifo_level_consistent` | `fifo_level == push_count - pop_count` (modular) |
| `fifo_empty_no_read` | `fifo_empty → !fifo_read` (arbiter doesn't read empty FIFO) |
| `fifo_level_max_monotonic` | `fifo_level_max` never decreases (except on clear/reset) |

### 6.4 `hist_pipeline_sva`

| Assertion | Property |
|-----------|----------|
| `divider_pipeline_valid_prop` | Valid propagates through exactly BIN_INDEX_WIDTH+1 stages |
| `queue_occupancy_bounded` | `queue_level <= QUEUE_DEPTH` always |
| `queue_drain_only_when_head_valid` | `drain_valid → queue_head_valid_q` |
| `pingpong_no_simultaneous_rw` | Bank A port-a not written while being read for update (separate port A/B) |
| `pingpong_clear_completes` | After `i_clear`, `flushing` goes low within 2*N_BINS+10 cycles |
| `apply_blocks_ingress` | `cfg_apply_pending → all ingress_accept == 0` |
| `apply_resolves` | `cfg_apply_pending` eventually clears (within bounded cycles) |

---

## 7. DPI Integration Plan (Future)

### 7.1 Purpose

DPI-C functions will wrap a C reference model for the histogram computation. This allows:
- Bit-exact comparison against a C implementation (independent of SV scoreboard)
- Future TLM integration where the C model is shared between simulation and firmware validation

### 7.2 Interface

```c
// C-side reference model
typedef struct {
    int      left_bound;
    int      right_bound;
    uint16_t bin_width;
    uint8_t  key_lo, key_hi;
    uint8_t  key_unsigned;
    uint8_t  filter_enable, filter_reject;
    uint8_t  filter_lo, filter_hi;
    uint16_t filter_key;
    uint32_t bins[256];
    uint32_t total_hits, dropped_hits;
    uint32_t underflow_count, overflow_count;
} hist_ref_model_t;

// DPI imports
void hist_ref_init(hist_ref_model_t* model, /* config params */);
int  hist_ref_process_hit(hist_ref_model_t* model, uint64_t data, int port, int channels_per_port);
void hist_ref_clear(hist_ref_model_t* model);
int  hist_ref_get_bin(hist_ref_model_t* model, int bin_index);
```

### 7.3 Integration Timeline

- **Phase 1**: SV-only scoreboard (no DPI). Validates correctness.
- **Phase 2**: C reference model developed separately, compiled as shared library.
- **Phase 3**: DPI import calls added to scoreboard. C model runs in parallel with SV model; cross-check.

### 7.4 Compilation

```makefile
# Compile C model
gcc -shared -fPIC -o libhist_ref.so hist_ref_model.c

# Link with vsim
$(VSIM) -sv_lib libhist_ref ...
```

---

## 8. Makefile Structure

```makefile
QUESTA_HOME ?= /data1/intelFPGA_pro/23.1/questa_fse
export LM_LICENSE_FILE := 8161@lic-mentor.ethz.ch

VLIB = $(QUESTA_HOME)/bin/vlib
VMAP = $(QUESTA_HOME)/bin/vmap
VCOM = $(QUESTA_HOME)/bin/vcom
VLOG = $(QUESTA_HOME)/bin/vlog
VSIM = $(QUESTA_HOME)/bin/vsim

IP_SRC     = $(realpath ../..)
INTEL_LIBS = $(QUESTA_HOME)/intel/vhdl
UVM_HOME   = $(QUESTA_HOME)/verilog_src/uvm-1.2
WORK       = work_hist_uvm
TB_TOP     = tb_top

TEST ?= hist_smoke_test
SEED ?= random

.PHONY: compile run run_cov clean

compile: $(WORK)/_vmake

$(WORK)/_vmake: ...
    $(VLIB) $(WORK)
    $(VMAP) $(WORK) $(WORK)
    $(VMAP) lpm $(INTEL_LIBS)/220model
    # VHDL sources
    $(VCOM) -work $(WORK) -2008 $(IP_SRC)/rtl/histogram_statistics_v2_pkg.vhd
    $(VCOM) -work $(WORK) -2008 $(IP_SRC)/rtl/true_dual_port_ram_single_clock.vhd
    $(VCOM) -work $(WORK) -2008 $(IP_SRC)/rtl/alt_dpram/alt_dpram_true.vhd
    $(VCOM) -work $(WORK) -2008 $(IP_SRC)/rtl/hit_fifo.vhd
    $(VCOM) -work $(WORK) -2008 $(IP_SRC)/rtl/rr_arbiter.vhd
    $(VCOM) -work $(WORK) -2008 $(IP_SRC)/rtl/bin_divider.vhd
    $(VCOM) -work $(WORK) -2008 $(IP_SRC)/rtl/coalescing_queue.vhd
    $(VCOM) -work $(WORK) -2008 $(IP_SRC)/rtl/pingpong_sram.vhd
    $(VCOM) -work $(WORK) -2008 $(IP_SRC)/rtl/histogram_statistics_v2.vhd
    # UVM package
    $(VLOG) -work $(WORK) -sv $(UVM_HOME)/src/uvm_pkg.sv +incdir+$(UVM_HOME)/src
    # Testbench
    $(VLOG) -work $(WORK) -sv +incdir+. tb_top.sv hist_env_pkg.sv
    @touch $@

run: compile
    $(VSIM) -c -work $(WORK) -t ps -voptargs=+acc \
      -suppress 19 -suppress 3009 -suppress 3473 \
      +UVM_TESTNAME=$(TEST) -sv_seed $(SEED) \
      -do "run -all; quit -f" $(TB_TOP)

run_cov: compile
    $(VSIM) -c -work $(WORK) -t ps -voptargs=+acc \
      -suppress 19 -suppress 3009 -suppress 3473 \
      -coverage \
      +UVM_TESTNAME=$(TEST) -sv_seed $(SEED) \
      -do "coverage save -onexit $(TEST)_$(SEED).ucdb; run -all; quit -f" $(TB_TOP)

run_vcd: compile
    $(VSIM) -c -work $(WORK) -t ps -voptargs=+acc \
      -suppress 19 -suppress 3009 -suppress 3473 \
      +UVM_TESTNAME=$(TEST) -sv_seed $(SEED) \
      -do "vcd file $(TEST).vcd; vcd add -r /tb_top/dut/*; run -all; vcd flush; quit -f" $(TB_TOP)

merge_cov:
    $(QUESTA_HOME)/bin/vcover merge merged.ucdb *.ucdb
    $(QUESTA_HOME)/bin/vcover report -html -output cov_html merged.ucdb

clean:
    rm -rf $(WORK) transcript vsim.wlf *.ucdb *.vcd cov_html
```
