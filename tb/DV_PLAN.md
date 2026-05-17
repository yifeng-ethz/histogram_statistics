# DV Plan: histogram_statistics_v2 (Standalone IP)

**DUT:** `histogram_statistics_v2` (Rev 26.3 / FEB V3 direct Type0-Type1 input update)
**IP source:** `mu3e-ip-cores/histogram_statistics/histogram_statistics_v2.vhd`
**Author:** Yifeng Wang (yifenwan@phys.ethz.ch)
**Date:** 2026-05-17
**Status:** V3 direct-input payload proof is present, but standalone IP DV closure is open on BUG-010-H. Bridge-based histogram stimulus is not signoff evidence for FEB V3.

---

## 1. DUT Overview

Multi-port online histogram with configurable bins, coalescing queue, and ping-pong readout. For FEB V3 the public ingress is explicit: eight Type0 lanes, one Type1 up bank, and one Type1 down bank. A CSR source-select chooses Type0, Type1 up, or Type1 down before the existing FIFO, arbiter, divider, coalescer, and dual-bank M10K SRAM update path.

### Data Path

```
ingress_comb → ingress_stage_reg → hit_fifo (×8) → rr_arbiter
    → divider_pipe (3 stages) → bin_divider (2 stages when POWER2_BIN_WIDTH_ONLY=true)
    → queue_hit_pipe → coalescing_queue → pingpong_sram
```

### Sub-Components

| Component | File | Key Parameters |
|-----------|------|---------------|
| `histogram_statistics_v2` | `histogram_statistics_v2.vhd` | N_BINS=256, N_PORTS=8, SAR_TICK_WIDTH=21, MAX_COUNT_BITS=20, COAL_QUEUE_DEPTH=4 |
| `hit_fifo` | `hit_fifo.vhd` | DATA_WIDTH=21, FIFO_ADDR_WIDTH=2, V3 delivered depth=4 |
| `rr_arbiter` | `rr_arbiter.vhd` | N_PORTS=8, DATA_WIDTH=32 |
| `bin_divider` | `bin_divider.vhd` | TICK_WIDTH=21, BIN_INDEX_WIDTH=8, power-of-two shift mapper in V3 |
| `coalescing_queue` | `coalescing_queue.vhd` | QUEUE_DEPTH=4, KICK_WIDTH=4 in V3 |
| `pingpong_sram` | `pingpong_sram.vhd` | N_BINS=256, COUNT_WIDTH=20, UPDATE_WIDTH=4 in V3 |
| `true_dual_port_ram_single_clock` | `true_dual_port_ram_single_clock.vhd` | DATA_WIDTH=20, ADDR_WIDTH=8 |

### Interfaces

| Interface | Type | Width | Description |
|-----------|------|-------|-------------|
| `avs_csr` | AVMM slave | 5-bit addr, 32-bit data | CSR register file (17 registers, standard identity header at words 0-1) |
| `avs_hist_bin` | AVMM slave | 8-bit addr, 32-bit data | Histogram bin readout (burst capable) + measure_clear |
| `asi_type0_lane0..7` | AVST sink | 45-bit Type0 data, 4-bit channel | Direct Type0 rate/key source, all eight lanes active when source-select=0 |
| `asi_type1_up` | AVST sink | 39-bit Type1 data, 4-bit channel | Direct MTS up-bank source, active when source-select=1 |
| `asi_type1_up_ts` | conduit | 48-bit timestamp | True hit timestamp sideband for Type1 up delay mode |
| `asi_type1_down` | AVST sink | 39-bit Type1 data, 4-bit channel | Direct MTS down-bank source, active when source-select=2 |
| `asi_type1_down_ts` | conduit | 48-bit timestamp | True hit timestamp sideband for Type1 down delay mode |
| `aso_hist_fill_out` | AVST source | 45-bit padded data, 4-bit channel | Optional Type0 lane0 snoop only when Type0 is selected and SNOOP_EN is true |
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
| 2 | CONTROL | RW | [0] apply, [1] apply_pending (RO), [7:4] mode, [8] key_unsigned, [12] filter_enable, [13] filter_reject, [17:16] source_select (0=Type0, 1=Type1 up, 2=Type1 down), [24] error (RO), [31:28] error_info (RO) |
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
| F05 | Bin divider mapping | bin_divider shift mapper for power-of-two widths, legacy divider only when enabled | per-bin readback, CONTROL error_info=5 |
| F06 | Underflow/overflow | bin_divider boundary checks | CSR 8, 9 |
| F07 | Ping-pong bank swap | pingpong_sram timer-driven swap | bank_status CSR, bin data |
| F08 | Coalescing queue | coalescing_queue same-bin merge | per-bin count accuracy |
| F09 | FIFO backpressure | hit_fifo full → drop_pulse | dropped_hits CSR |
| P03 | Wire-speed burst absorption | single-port passive tap burst at one hit/clock | total/dropped/PORT_STATUS CSR |
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
| F20 | Kick counter saturation | kick_ram entry at KICK_MAX (15 in V3) → no further increment | correct bin count |
| F21 | Identity header (UID) | csr_read_comb word 0 returns IP_UID generic | CSR word 0 readback = 0x48495354 |
| F22 | Identity header (META mux) | csr_meta_sel selects VERSION/DATE/GIT/INSTANCE_ID | CSR word 1 readback cycles through 4 pages |
| F23 | UID immutability | Write to word 0 is ignored (read-only) | CSR word 0 unchanged after write |
| F24 | Phase-5 MuTRiG all-channel frame | 8 ports x 32-channel port offset into 256 bins | P04 in standalone tb: 256 accepted, 0 dropped, all bins match |
| F25 | Phase-5 FIFO depth gate | hit_fifo default depth 256 | FIFO absorbs one full 32-channel frame per 8 ports; over-depth burst is characterized, not hidden |
| F26 | V3 source select | csr_reg CONTROL[17:16], ingress mux | CSR readback, selected ready only |
| F27 | Type0 direct rate | `asi_type0_lane0..7` | 100 kHz per ASIC, one selected channel, nonzero bins, 5 ms RUNNING |
| F28 | Type1 direct rate | `asi_type1_up/down_data` | up/down bank selected independently, normal data filter before coalescing |
| F29 | Type1 direct delay | `asi_type1_up/down_ts` and GTS | delay-key histogram from true timestamp sideband |
| F30 | Bridge exclusion | system integration structural checks | any `histogram_ingress_bridge` drive into hist is a failing V3 condition |

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
| Simulator | QuestaOne 2026 (`/data1/questaone_sim/questasim/`) |
| License | Full Mentor floating (`8161@lic-mentor.ethz.ch`) |
| Capabilities | `rand`/`constraint`, `covergroup`/`cross`, DPI-C, full UVM 1.2 |
| Intel VHDL libs | Quartus `sim_lib` compiled into local `lpm` / `altera_mf` work libraries |

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

### 8.1a V3 Queue/FIFO Depth

The FEB V3 standalone resource profile uses `FIFO_ADDR_WIDTH=2` on every ingress
FIFO and `COAL_QUEUE_DEPTH=4`. Capacity pressure is intentionally visible through
`DROPPED_HITS`, `PORT_STATUS`, and `COAL_STATUS`; the resource profile is not
allowed to hide overload by silently restoring the legacy 256-deep queues.

The legacy Phase-5 all-channel `FIFO_ADDR_WIDTH=8` / `COAL_QUEUE_DEPTH=256`
cases remain useful historical stress tests, but they are not the V3 direct-input
signoff profile and must not be mixed into the less-than-2k-ALM resource claim.

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

### 8.7 Current V3 Direct-Input Evidence

`hist_v3_direct_input_test` is the current V3 direct-input focused test. It configures the
IP via CSR before RUNNING, uses a 1 ms ping-pong interval, runs the Type0 phase
for 5 ms, and then exercises Type1 up/down in both rate and delay modes.

Current standalone rerun status at `SEED=7`:

- `tb/REPORT/hist_v3_direct_input_20260517_seed7_post_makefix.log`: payload
  checks observe direct Type0 and Type1 hits, but the run fails due four
  `hist_pipeline_sva: measure_clear_pulse did not lead to flushing` assertion
  errors. This is tracked as BUG-010-H.
- The UVM report alone shows `0` UVM errors/fatals, but that is not sufficient
  closure evidence; BUG-009-H fixed the make target so simulator assertion
  errors now fail the run.

Key readback evidence:

- Type0 direct: `last_interval_total=1600`, `live_total=8`, `dropped=0`, `coal_status=0x00000100`.
- Type1 up rate: `delta_total=256`, `dropped=0`, `coal_status=0x00000100`.
- Type1 up delay: `delta_total=256`, `dropped=0`, `coal_status=0x00000100`.
- Type1 down rate: `delta_total=256`, `dropped=0`, `coal_status=0x00000100`.
- Type1 down delay: `delta_total=256`, `dropped=0`, `coal_status=0x00000100`.

Direct FEB V3 `tb_int` evidence:

- Command:
  `SEED=20260517 firmware_builds/systems/v3_pretest-260511-emutype0-dualport-260512/tb_int/hist_dualport/run_hist_dualport.sh matrix`.
- Harness:
  `tb_hist_direct_v3.sv` instantiates `histogram_statistics_v2` directly with
  Type0 lane0..7, Type1 up/down streams, and 48-bit Type1 up/down timestamp
  sidebands. It does not use `histogram_ingress_bridge`.
- Duration:
  `RUN_CYCLES=1250000`, `INTERVAL_CYCLES=125000`, covering 10 ms RUNNING with
  ten 1 ms ping-pong intervals.
- Result:
  `hist_direct_v3_matrix_20260517_021842_summary.csv` reports 30/30 PASS,
  zero simulator errors, zero drops, and `bin_sum == total` for every case.
- Mode/rate coverage:
  Type0 rate mode at 100 kHz, 500 kHz, and 1 MHz per ASIC; Type1 up/down rate
  and latency modes at the same rates; one-random-channel and all-channel
  patterns for each source/mode combination.
