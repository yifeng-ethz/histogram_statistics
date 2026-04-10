# Histogram Statistics Mu3E IP

Multi-port coalescing histogram with pipelined bin index and ping-pong rate readout.
Drop-in replacement for per-ASIC channel rate counter arrays in the Mu3e online data
acquisition system.

**Version:** 26.1.0.0410
**Module name:** `histogram_statistics_v2`
**Platform Designer group:** Mu3e Data Plane / Debug

---

## Use Case

The Mu3e front-end boards receive continuous hit streams from up to 8 MuTRiG ASICs.
Each hit carries a configurable key field (typically a timestamp delta or channel ID).
The histogram IP snoops these streams in real time, extracts the key, maps it to a bin
index, and maintains per-bin counters in dual-bank M10K SRAM.  A periodic timer swaps
the active bank so the frozen bank can be read out by the slow-control host without
disturbing the live accumulation.

Typical deployment:

- **Rate monitoring:** histogram of per-channel hit rates, read out once per second.
- **Timing diagnostics:** histogram of timestamp deltas to detect clock/data glitches.
- **Debug profiling:** route any 16-bit debug stream into the histogram via debug mode.

---

## Architecture

```
                     +-----------+
  hist_fill_in  ---->|           |    +----------+    +-----------+
  fill_in_1     ---->| hit_fifo  |--->|          |    |           |
  fill_in_2     ---->|  (x8)    |--->|rr_arbiter|--->|bin_divider|---+
  ...           ---->|           |--->|          |    |           |   |
  fill_in_7     ---->|           |    +----------+    +-----------+   |
                     +-----------+                                    |
                                    +----------------+                |
                                    |                |<---------------+
       host CSR  <--- avs_csr ----->| coalescing     |
       host bins <--- avs_hist_bin->| _queue         |
                                    |                |----> pingpong_sram
                                    +----------------+       (dual-bank M10K)
                                            |
                            interval_reset -+-> bank swap timer
```

### Pipeline Stages

| Stage | Component | Latency | Description |
|-------|-----------|---------|-------------|
| 1 | `ingress_comb` | 0 (comb) | Key extraction, filter evaluation, signed/unsigned conversion |
| 2 | `ingress_stage_reg` | 1 cycle | Registered ingress pipeline cut for timing closure |
| 3 | `hit_fifo` (x N_PORTS) | 1 cycle | Per-port elastic FIFO (depth 16) absorbs arbiter stalls |
| 4 | `rr_arbiter` | 1 cycle | Round-robin grant across all non-empty FIFOs |
| 5 | `divider_pipe` | 3 cycles | Key offset, port-channel offset, bin-width preparation |
| 6 | `bin_divider` | BIN_INDEX_WIDTH cycles | Pipelined restoring divider: `(key - left_bound) / bin_width` |
| 7 | `coalescing_queue` | 1 cycle | Merge repeated same-bin hits into (bin, count) pairs |
| 8 | `pingpong_sram` | 3 cycles | Read-modify-write to dual-bank M10K, with bank swap logic |

### Ping-Pong Rate Readout

When `ENABLE_PINGPONG = true`, the histogram maintains two SRAM banks:

1. **Active bank** -- receives live bin updates from the coalescing queue.
2. **Frozen bank** -- holds the previous interval's snapshot, readable by the host via `hist_bin`.

A countdown timer (CSR word 8, default 125 MHz = 1 s) triggers the bank swap.
On swap, the newly active bank is flushed to zero before accepting new updates.
The host reads bins from the frozen bank with zero contention.

When `ENABLE_PINGPONG = false`, a single bank is used.  The host reads from the
same bank that receives updates.  The `interval_reset` input can still trigger
a manual clear.

### Coalescing Queue

When multiple ports produce hits to the same bin within a short window, the
coalescing queue merges them into a single SRAM update carrying a batched count.
This prevents read-modify-write hazards in the `pingpong_sram` and reduces the
effective SRAM write bandwidth.

The queue tracks which bins are in-flight via a circular buffer.  A per-bin kick
counter (8-bit, saturating at 255) accumulates hits until the queue head drains.
Queue overflow is counted in CSR word 14 (`COAL_STATUS`).

---

## CSR Register Map

All registers are word-addressed through the `csr` Avalon-MM slave (4-bit address, read latency 1).

| Word | Name | Access | Description |
|------|------|--------|-------------|
| 0x00 | CONTROL | RW | `[0]` soft_reset, `[1]` apply_pending (RO), `[7:4]` mode, `[8]` key_unsigned, `[12]` filter_enable, `[13]` filter_reject, `[24]` error (RO), `[31:28]` error_info (RO) |
| 0x01 | LEFT_BOUND | RW | Signed left boundary of the histogram range |
| 0x02 | RIGHT_BOUND | RW | Signed right boundary (auto-computed from left + width * bins) |
| 0x03 | BIN_WIDTH | RW | `[15:0]` bin width in key-space units |
| 0x04 | KEY_LOC | RW | `[7:0]` update_key_lo, `[15:8]` update_key_hi, `[23:16]` filter_key_lo, `[31:24]` filter_key_hi |
| 0x05 | KEY_VALUE | RW | `[15:0]` update_key, `[31:16]` filter_key |
| 0x06 | UNDERFLOW_COUNT | RO | Keys below left_bound (saturating, reset on interval/clear) |
| 0x07 | OVERFLOW_COUNT | RO | Keys above right_bound (saturating, reset on interval/clear) |
| 0x08 | INTERVAL_CFG | RW | Ping-pong interval in clock cycles |
| 0x09 | BANK_STATUS | RO | `[0]` active_bank, `[1]` flushing, `[15:8]` flush_addr |
| 0x0A | PORT_STATUS | RO | `[7:0]` per-port FIFO empty, `[23:16]` max FIFO fill level |
| 0x0B | TOTAL_HITS | RO | Total accepted hits (saturating, reset on interval/clear) |
| 0x0C | DROPPED_HITS | RO | Dropped hits due to FIFO/queue overflow |
| 0x0D | VERSION | RO | `[31:24]` major, `[23:16]` minor, `[15:12]` patch, `[11:0]` build |
| 0x0E | COAL_STATUS | RO | `[7:0]` queue occupancy, `[15:8]` occupancy max, `[31:16]` overflow count |
| 0x0F | SCRATCH | RW | General-purpose scratch register |

### Runtime Configuration Workflow

1. Write `LEFT_BOUND`, `BIN_WIDTH`, key bit locations, and filter settings.
2. Write `CONTROL[0] = 1` to apply.  The IP copies shadow registers into the
   active configuration on the next interval boundary (or immediately if ping-pong
   is disabled).  `CONTROL[1]` reads back `1` while the apply is pending.
3. Read `TOTAL_HITS`, `DROPPED_HITS`, `UNDERFLOW_COUNT`, `OVERFLOW_COUNT` for diagnostics.
4. Read bins from `hist_bin` (supports burst reads up to N_BINS words).

### Measure Clear

Writing `0x00000000` to any address of the `hist_bin` slave triggers a
**measure-and-clear**: all bin counters and statistics are reset, and a new
accumulation interval begins.  The `interval_reset` input has the same effect.

### Debug Mode

Writing a negative value to `CONTROL[7:4]` (mode field) selects one of the
`debug_N` Avalon-ST sinks as the histogram input source instead of the normal
ingress ports:

| Mode | Source |
|------|--------|
| -1 (0xF) | `debug_1` (signed 16-bit) |
| -2 (0xE) | `debug_2` (unsigned 16-bit) |
| -3 (0xD) | `debug_3` |
| -4 (0xC) | `debug_4` |
| -5 (0xB) | `debug_5` |
| -6 (0xA) | `debug_6` |

---

## Platform Designer GUI

The `_hw.tcl` presents four tabs in Platform Designer:

### Configuration Tab

**Overview** -- Block-level description with data-path diagram and clocking summary.

**Histogram Sizing** -- The critical parameters that determine resource usage:

- `N_BINS` -- Number of histogram bins (1..2048).  Each true dual-port RAM pair
  hosts up to 512 bins.  The default 256 uses 2 M10K for single-bank or 4 M10K
  with ping-pong.
- `MAX_COUNT_BITS` -- Width of each bin counter (1..72).  Two M10K support up to
  40 bits; four M10K support up to 72 bits.  Counters saturate at their maximum
  value.
- `DEF_LEFT_BOUND` / `DEF_BIN_WIDTH` -- Power-on defaults for the histogram range.
  Overridable at runtime through CSR.

**Key Extraction** -- Bit-field locations for the update key and filter key within
the snooped data stream.  `SAR_TICK_WIDTH` controls the internal divider
resolution; `SAR_KEY_WIDTH` sets the maximum key width.  The constraint
`SAR_TICK_WIDTH >= SAR_KEY_WIDTH` is enforced by the validation callback.

**Ingress** -- Number of ports, channels per port, coalescing queue depth, and
AVST bus widths.  Ports beyond `N_PORTS` are automatically disabled.

**Ping-Pong / Interval** -- Enable/disable dual-bank mode, set the default
interval timer, snooping, and packet support.

**Resources** -- Live M10K and ALM estimates updated by the validation callback.

### Identity Tab

**Delivered Profile** -- Catalog revision and runtime visibility notes.

**Versioning** -- `VERSION_MAJOR`, `VERSION_MINOR`, `VERSION_PATCH`, `BUILD`
parameters that are packed into CSR word 13.

**Debug** -- `N_DEBUG_INTERFACE` (how many of the 6 debug sinks to enable) and
`DEBUG` level (0=off, 1=synthesizable, 2=simulation-only).

### Interfaces Tab

Clock/reset domain documentation, data-path interface descriptions (ingress sinks,
passthrough source), control-path interfaces (CSR, hist_bin, ctrl), and monitoring
interfaces (debug sinks).

### Register Map Tab

Interactive HTML table of the 16-word CSR window with address, name, access mode,
and bit-field descriptions.

---

## Presets

Three presets are shipped in `histogram_statistics_v2_presets.qprs`:

| Preset | N_BINS | Counter | Ports | Queue | Ping-Pong | Use Case |
|--------|--------|---------|-------|-------|-----------|----------|
| **Default** | 256 | 32-bit | 8 | 256 | On (1 s @ 125 MHz) | Standard per-ASIC rate monitoring |
| **Minimal** | 64 | 32-bit | 1 | 16 | Off | Single-channel, low-resource monitoring |
| **Large** | 1024 | 40-bit | 8 | 256 | On (1 s @ 125 MHz) | Fine-grained profiling, wide key space |

Select a preset from the Platform Designer component editor to load all
parameters at once, then adjust individual settings as needed.

---

## Directory Structure

```
histogram_statistics/
  histogram_statistics_v2_hw.tcl         -- Platform Designer component descriptor (v2)
  histogram_statistics_v2_presets.qprs   -- Presets (Default, Minimal, Large)
  histogram_statistics_hw.tcl            -- Legacy v1 component descriptor
  rtl/                                   -- All RTL source files
    histogram_statistics_v2.vhd          -- Top-level entity (v2)
    histogram_statistics_v2_pkg.vhd      -- Shared types and helper functions
    hit_fifo.vhd                         -- Per-port elastic FIFO
    rr_arbiter.vhd                       -- Round-robin port arbiter
    bin_divider.vhd                      -- Pipelined restoring divider
    coalescing_queue.vhd                 -- Same-bin update coalescer
    pingpong_sram.vhd                    -- Dual-bank histogram storage
    true_dual_port_ram_single_clock.vhd  -- Generic true dual-port RAM
    alt_dpram/alt_dpram_true.vhd         -- Altera dual-port RAM wrapper
    histogram_statistics.vhd             -- Legacy v1 top-level
    shift_reg_with_dsp.vhd              -- Shift register (v1 helper)
    b2o_encoder.v                        -- Binary-to-onehot encoder (v1)
  syn/quartus/                           -- Standalone synthesis signoff project
  tb/                                    -- Testbench (standalone + UVM)
    Makefile                             -- Quick standalone simulation
    tb_histogram_statistics_v2.sv        -- Standalone SV testbench
    uvm/                                 -- Full UVM environment
      Makefile                           -- UVM simulation flow
      tb_top.sv                          -- UVM top-level
      hist_env.sv                        -- UVM environment
      hist_if.sv                         -- Interface bundle
      tests/                             -- Test library (smoke, CSR, key, filter, ...)
      sva/                               -- SystemVerilog assertions
    DV_PLAN.md                           -- Verification plan
```

---

## Quick Start

### Standalone Simulation

```bash
cd tb/
make compile          # compile DUT + testbench
make run TEST=B01_smoke SEED=42
make run_all          # run all tests
```

### UVM Simulation

```bash
cd tb/uvm/
make compile          # compile DUT + UVM harness
make run TEST=hist_smoke_test
make run TEST=hist_burst_test SEED=random
```

### Standalone Synthesis Signoff

```bash
cd syn/quartus/
quartus_sh --flow compile histogram_statistics_v2_standalone
```

### Integration in Platform Designer

1. Add the `histogram_statistics/` directory to your Quartus IP search path.
2. In Platform Designer, search for **Histogram Statistics Mu3E IP** in the IP Catalog
   under **Mu3e Data Plane / Debug**.
3. Select a preset (Default, Minimal, or Large) and adjust parameters.
4. Connect `hist_fill_in` to your data stream, `csr` and `hist_bin` to the
   interconnect, `ctrl` to the run-control splitter, and `clock`/`reset` to
   your domain.
5. Optionally connect `fill_in_1..7` for multi-port operation and `debug_1..6`
   for auxiliary signal monitoring.

---

## Version History

| Version | Date | Change |
|---------|------|--------|
| 26.1.0.0410 | 2026-04-10 | Upgraded `_hw.tcl` to rich IP packaging format (tabbed GUI, presets, CSR register map, resource estimates) |
| 26.0.321 | 2026-03-21 | Restored v2 from `feb_system_v2` generated RTL |
| Rev 1.2 | 2026-04-09 | Decouple `build_key` from `filter_pass_v` gating to break timing path (-2.554 ns at 137.5 MHz) |
| Rev 1.1 | 2026-04-09 | Register `measure_clear_pulse` to break AVMM address decode timing path (-0.472 ns at 125 MHz) |
| Rev 1.0 | 2026-03-20 | Initial v2 top-level with multi-port, coalescing queue, ping-pong SRAM |
