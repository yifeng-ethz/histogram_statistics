# DV Plan: histogram_statistics_v2 (Standalone IP)

**DUT:** `histogram_statistics_v2` (Rev 1.2)
**IP source:** `mu3e-ip-cores/histogram_statistics/histogram_statistics_v2.vhd`
**Author:** Yifeng Wang (yifenwan@phys.ethz.ch)
**Date:** 2026-04-09
**Status:** Approved for implementation and signoff updates (chief architect signoff recorded 2026-04-12)

---

## 1. DUT Overview

Multi-port online histogram with configurable bins, coalescing queue, and ping-pong readout. Accepts up to 8 Avalon-ST input ports, extracts a configurable key field, maps keys to bin indices via a pipelined restoring divider, coalesces concurrent updates in a queue, and stores counts in dual-bank M10K SRAM with automatic interval-based bank swap.

### Data Path

```
ingress_comb → ingress_stage_reg → hit_fifo (×8) → rr_arbiter
    → divider_pipe (3 stages) → bin_divider (8 pipeline stages)
    → queue_hit_pipe → coalescing_queue → pingpong_sram
```

### Sub-Components

| Component | File | Key Parameters |
|-----------|------|---------------|
| `histogram_statistics_v2` | `histogram_statistics_v2.vhd` | N_BINS=256, N_PORTS=8, SAR_TICK_WIDTH=32, SAR_KEY_WIDTH=16, COAL_QUEUE_DEPTH=256 |
| `hit_fifo` | `hit_fifo.vhd` | DATA_WIDTH=32, FIFO_ADDR_WIDTH=4 (depth=16) |
| `rr_arbiter` | `rr_arbiter.vhd` | N_PORTS=8, DATA_WIDTH=32 |
| `bin_divider` | `bin_divider.vhd` | TICK_WIDTH=32, BIN_INDEX_WIDTH=8, COUNT_WIDTH=8 |
| `coalescing_queue` | `coalescing_queue.vhd` | N_BINS=256, QUEUE_DEPTH=256, KICK_WIDTH=8 |
| `pingpong_sram` | `pingpong_sram.vhd` | N_BINS=256, COUNT_WIDTH=32, UPDATE_WIDTH=8 |
| `true_dual_port_ram_single_clock` | `true_dual_port_ram_single_clock.vhd` | DATA_WIDTH=32, ADDR_WIDTH=8 |

### Interfaces

| Interface | Type | Width | Description |
|-----------|------|-------|-------------|
| `avs_csr` | AVMM slave | 5-bit addr, 32-bit data | CSR register file (17 registers, standard identity header at words 0-1) |
| `avs_hist_bin` | AVMM slave | 8-bit addr, 32-bit data | Histogram bin readout (burst capable) + measure_clear |
| `asi_hist_fill_in` | AVST sink | 39-bit data, 4-bit channel | Port 0 ingress (with snoop passthrough) |
| `asi_fill_in_1..7` | AVST sink | 39-bit data, 4-bit channel | Ports 1-7 ingress |
| `aso_hist_fill_out` | AVST source | 39-bit data, 4-bit channel | Snoop passthrough (port 0 only, when SNOOP_EN) |
| `asi_ctrl` | AVST sink | 9-bit data | Run control input |
| `asi_debug_1..6` | AVST sink | 16-bit data | Debug inputs (selected by negative cfg_mode) |
| `i_interval_reset` | conduit | 1-bit | External interval reset trigger |
| `i_rst` / `i_clk` | clock/reset | 1-bit | System clock and synchronous reset |

---

## 2. CSR Register Map

| Addr | Name | R/W | Description |
|------|------|-----|-------------|
| 0 | UID | R | Software-visible IP identifier. Default ASCII "HIST" (0x48495354). |
| 1 | META | RW/R | Read-multiplexed metadata. Write selector: 0=VERSION, 1=DATE, 2=GIT, 3=INSTANCE_ID. |
| 2 | CONTROL | RW | [0] apply, [1] apply_pending (RO), [7:4] mode, [8] key_unsigned, [12] filter_enable, [13] filter_reject, [24] error (RO), [31:28] error_info (RO) |
| 3 | LEFT_BOUND | RW | Signed 32-bit left histogram bound |
| 4 | RIGHT_BOUND | RW | Signed 32-bit right bound (auto-computed if bin_width != 0) |
| 5 | BIN_WIDTH | RW | [15:0] unsigned bin width (0 = use direct bounds) |
| 6 | KEY_FILTER_BITS | RW | [7:0] update_key_lo, [15:8] update_key_hi, [23:16] filter_key_lo, [31:24] filter_key_hi |
| 7 | KEY_FILTER_VAL | RW | [15:0] update_key, [31:16] filter_key |
| 8 | UNDERFLOW_CNT | R | 32-bit underflow counter (saturating, reset on interval/clear) |
| 9 | OVERFLOW_CNT | R | 32-bit overflow counter (saturating, reset on interval/clear) |
| 10 | INTERVAL_CFG | RW | 32-bit interval clock count for ping-pong bank swap |
| 11 | BANK_STATUS | R | [0] active_bank, [1] flushing, [15:8] flush_addr |
| 12 | PORT_STATUS | R | [7:0] per-port FIFO empty, [23:16] max FIFO fill level |
| 13 | TOTAL_HITS | R | 32-bit total accepted hits (saturating, reset on interval/clear) |
| 14 | DROPPED_HITS | R | 32-bit dropped hits (saturating, reset on interval/clear) |
| 15 | COAL_STATUS | R | [7:0] queue_occupancy, [15:8] occupancy_max, [31:16] overflow_count |
| 16 | SCRATCH | RW | Scratch register |

---

## 3. Verification Targets

| ID | Feature | RTL Location | Observability |
|----|---------|-------------|---------------|
| F01 | Single-port fill | ingress_comb, hit_fifo, bin_divider, pingpong_sram | hist_bin readdata |
| F02 | Multi-port fill | ingress_comb ×8, rr_arbiter, coalescing_queue | bin counts, total_hits CSR |
| F03 | Filter pass/reject | match_filter() in ingress_comb | total_hits vs dropped_hits |
| F04 | Key extraction | build_key() signed/unsigned paths | ingress_stage_key probe |
| F05 | Bin divider mapping | bin_divider restoring division pipeline | per-bin readback |
| F06 | Underflow/overflow | bin_divider boundary checks | CSR 8, 9 |
| F07 | Ping-pong bank swap | pingpong_sram timer-driven swap | bank_status CSR, bin data |
| F08 | Coalescing queue | coalescing_queue same-bin merge | per-bin count accuracy |
| F09 | FIFO backpressure | hit_fifo full → drop_pulse | dropped_hits CSR |
| F10 | CSR apply mechanism | cfg_apply_request → cfg_apply_pending → shadow copy | CONTROL readback bit[1] |
| F11 | Measure clear | clear_pulse → measure_clear_pulse → full module reset | bin readback all-zero |
| F12 | Snoop passthrough | port 0 → fill_out (SNOOP_EN=true) | fill_out valid/data |
| F13 | Debug mode | negative cfg_mode selects asi_debug_N → port 0 | bin counts from debug data |
| F14 | Burst read | pingpong_sram burst read state machine | readdatavalid count |
| F15 | Statistics counters | stats_pipe → stats_reg (total, dropped, underflow, overflow) | CSR readback |
| F16 | Port offset | divider_pipe: key + port_index * CHANNELS_PER_PORT | bin index shifted by port |
| F17 | Config error detection | csr_reg: bin_width=0 + right_bound <= left_bound | error bit, error_info |
| F18 | Interval reset | i_interval_reset → measure_clear_comb → measure_clear_pulse | full pipeline flush |
| F19 | Coalescing queue overflow | queue_room_c = 0 → overflow_count increment | COAL_STATUS CSR |
| F20 | Kick counter saturation | kick_ram entry at KICK_MAX (255) → no further increment | correct bin count |
| F21 | Identity header (UID) | csr_read_comb word 0 returns IP_UID generic | CSR word 0 readback = 0x48495354 |
| F22 | Identity header (META mux) | csr_meta_sel selects VERSION/DATE/GIT/INSTANCE_ID | CSR word 1 readback cycles through 4 pages |
| F23 | UID immutability | Write to word 0 is ignored (read-only) | CSR word 0 unchanged after write |

---

## 4. Test Group Files

| File | ID Range | Cases | Method Mix |
|------|----------|-------|------------|
| [DV_BASIC.md](DV_BASIC.md) | B001-B999 | 128+ | Directed (D), some promoted to constrained-random (R) |
| [DV_EDGE.md](DV_EDGE.md) | E001-E999 | 128+ | Directed (D) |
| [DV_PROF.md](DV_PROF.md) | P001-P999 | 128+ | Constrained-random (R), Sweep (S), Soak (K) |
| [DV_ERROR.md](DV_ERROR.md) | X001-X999 | 128+ | Directed (D), Fault injection (F) |

---

## 5. Cross-Reference Documents

| File | Content |
|------|---------|
| [DV_HARNESS.md](DV_HARNESS.md) | UVM env architecture, agent topology, DPI integration plan, SVA modules |
| [DV_CROSS.md](DV_CROSS.md) | Crossing coverage conditions for transaction-based long-running tests |

---

## 6. Simulator & License

| Item | Value |
|------|-------|
| Simulator | Questa FSE 2022.4 (`/data1/intelFPGA_pro/23.1/questa_fse/`) |
| License | Full Mentor floating (`8161@lic-mentor.ethz.ch`) |
| Capabilities | `rand`/`constraint`, `covergroup`/`cross`, DPI-C, full UVM 1.2 |
| Intel VHDL libs | `$(QUESTA_HOME)/intel/vhdl/220model` |

---

## 7. Pass/Fail Criteria

- All BASIC tests pass with zero scoreboard mismatches
- All EDGE tests correctly detect and handle boundary conditions
- PROF tests: `sum(bins) == total_hits` for every completed interval
- ERROR tests: DUT recovers to known-good state after reset; no simulation hangs
- No `$fatal`, no UVM_ERROR, no X/Z on output ports after reset deasserts
- SVA: zero assertion failures across all tests
- Code coverage: statement > 95%, branch > 90%, toggle > 80%
- Functional coverage: all DV_CROSS.md coverpoints > 95%

---

## 8. Key RTL Behaviors (Reference for Test Authors)

### 8.1 Stats Counter Reset

Statistics counters (total_hits, dropped_hits, underflow_count, overflow_count) are reset by `stats_reset_pulse_d1`, which fires one cycle after either `measure_clear_pulse` or `interval_pulse`. This means:
- Bank swap (interval_pulse) resets all stats counters
- Measure clear (clear_pulse or i_interval_reset) also resets stats counters
- Reading stats must happen BEFORE the next interval/clear event

### 8.2 Port Offset

The divider_pipe adds `port_index * CHANNELS_PER_PORT` (default 32) to each key before feeding the bin_divider. This means port 0 keys are unshifted, port 1 keys are shifted by +32, port 2 by +64, etc. The histogram bounds must account for this offset when multiple ports are active.

### 8.3 Config Apply Mechanism

Writing `apply=1` to CSR CONTROL sets `cfg_apply_request`, which sets `cfg_apply_pending`. While pending, `ingress_accept` is blocked (no new hits accepted). When all `ingress_stage_valid` signals are clear, the shadow config is copied to active config and `cfg_apply_pending` clears.

### 8.4 Measure Clear vs Bank Swap

- `measure_clear_pulse` (from hist_bin write of 0x00000000 or i_interval_reset) resets the ENTIRE pingpong_sram module: clears both banks, resets timer, resets all pipeline state.
- Bank swap (interval_pulse from internal timer) only swaps the active bank and clears the newly active bank. Pipeline state is preserved.

### 8.5 CSR Read Latency

CSR reads go through `csr_read_comb` (combinational mux) → `csr_read_reg` (registered output). The registered output means CSR reads have **1 cycle of latency**: the data returned on a read is from the cycle when `avs_csr_read` was asserted, but it appears on `avs_csr_readdata` one cycle later. The hw.tcl must declare `readLatency=1` for correct Qsys integration.

### 8.6 Coalescing Queue Drain

The coalescing queue drains when `i_drain_ready=1` (from pingpong_sram) and the queue head is valid. The drain outputs `(bin, count)` pairs where count is the accumulated kick count for that bin. The kick counter is reset to 0 on drain, and the `queued` flag is cleared. If a new hit arrives for the same bin in the same cycle as a drain, the hit starts a new queue entry (queued_effective=0 path).
