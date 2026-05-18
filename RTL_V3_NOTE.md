# RTL_V3_NOTE.md - histogram_statistics_v2 explicit FEB V3 input contract

**Date:** 2026-05-17
**Scope:** `histogram_statistics_v2` RTL, Platform Designer packaging, standalone UVM, and FEB V3 system integration.

## 1. Problem Statement

The FEB V3 histogram path must not be validated through `histogram_ingress_bridge`.
That bridge can hide the actual integration bug by muxing or repacking traffic before
the histogram IP. The histogram IP must expose and verify the real V3 hit topology
directly:

- Type0 rate input from 8 MuTRiG/FEB lanes.
- Type1 input from 2 already-merged MTS processor banks, named up and down.
- Type1 delay mode using a separate 48-bit true-hit timestamp sideband.

Any `histogram_ingress_bridge_*.hist_out -> histogram_statistics_*.hist_*` path is
a failing integration condition for the V3 contract.

## 2. Required Histogram Interfaces

The histogram IP shall replace the generic `hist_fill_in` / `fill_in_1..7` public
contract with explicit V3 hit interfaces:

| Interface | Count | Data | Timestamp | Purpose |
|-----------|-------|------|-----------|---------|
| `type0_lane0_in` .. `type0_lane7_in` | 8 | normal Type0 hit word | none | Type0 rate/key histograms, all lanes active when selected |
| `type1_up_in` | 1 | normal Type1 hit word | separate `ts[47:0]` sideband | Type1 up-bank rate or delay histograms |
| `type1_down_in` | 1 | normal Type1 hit word | separate `ts[47:0]` sideband | Type1 down-bank rate or delay histograms |

Type0 and Type1 normal data fields remain the source for filtering. The Type1
timestamp sideband is used only for delay-key construction in delay mode.

## 3. CSR Source Selection

Add an explicit CSR field that selects which physical source feeds the existing
histogram update path:

| Value | Source |
|-------|--------|
| `0` | Type0, all 8 lanes merged through the existing per-port FIFO / arbiter / coalescing path |
| `1` | Type1 up bank |
| `2` | Type1 down bank |

Invalid source selections must be rejected by the existing apply/error mechanism.
The selected source is latched with the rest of the active configuration when
`CONTROL.apply` commits.

## 4. Mode And Filter Semantics

- Type0 supports rate/key mode.
- Type1 supports rate/key mode and delay mode.
- Delay mode subtracts the selected Type1 `ts[47:0]` sideband from the internal
  GTS counter and trims the result to `SAR_TICK_WIDTH`.
- Filtering always compares normal hit data fields, not the Type1 timestamp sideband.
- Filter polarity must remain configurable: pass matching traffic or reject matching
  traffic.
- Filtered-out hits must not enter the FIFO or coalescing queue.

## 5. Throughput Contract

The selected traffic source feeds the existing coalescing update path after
filtering:

- Type0 selected: all 8 Type0 lane streams are eligible every cycle.
- Type1 selected: the chosen merged bank can be as fast as 125 MHz, so filtering
  must happen before FIFO/coalescing admission.
- Backpressure, overflow, and drop counters must distinguish accepted ingress from
  filtered or capacity-dropped traffic well enough for DV and board debug.

## 6. MTS Processor Accommodation

The MTS processor Type1 outputs feeding histogram V3 must expose:

- normal Type1 data as an Avalon-ST stream,
- separate 48-bit true-hit timestamp sideband associated with each accepted Type1
  beat,
- up/down bank naming consistent with the histogram IP interfaces.

The timestamp sideband must not be packed into the normal Type1 data field just to
reach the histogram. Packing would make filtering ambiguous and reintroduce the
same class of hidden integration bug.

## 7. DV Closure Requirements

Standalone histogram DV is not closed until the following pass without bridge-based
stimulus:

- Structural check fails if a histogram input is driven by `histogram_ingress_bridge`.
- Type0 rate mode: 8 direct lanes, 100 kHz one randomly selected channel per ASIC,
  5 ms RUNNING, 1 ms ping-pong, CSR readback during and after RUNNING.
- Type1 up rate mode and delay mode: direct `type1_up_in` plus `ts[47:0]`, filtered
  and unfiltered cases.
- Type1 down rate mode and delay mode: direct `type1_down_in` plus `ts[47:0]`,
  filtered and unfiltered cases.
- Type1 delay latency plot evidence matches the FEB/SWB lifetime-style reference
  plot using the direct sideband timestamp path.
- Code coverage and functional coverage targets in `tb/DV_PLAN.md` are updated for
  the V3 source-select contract and reported honestly.

## 8. Synthesis Closure Requirements

Standalone synthesis must be rerun for the histogram IP after the V3 interface
change:

- use the existing standalone Quartus project under `syn/quartus/`,
- constrain at the tightened signoff clock defined by the synthesis plan,
- preserve enough input/output activity that the new Type0/Type1 ingress logic is
  not trimmed,
- report timing, resource deltas, and any residual warnings in `syn/SYN_REPORT.md`.

## 9. Coalescer Resource Reduction Target

The FEB V3 resource target is less than 2k ALMs for the standalone histogram IP.
The observed kick-count plots show the coalesced per-bin live count staying below
15 in the intended iid and bunched regimes, so `KICK_COUNT_WIDTH=4` is the correct
V3 default. Relative to the legacy 8-bit kick counter this saves:

- `256 bins * 4 bits = 1024` kick-RAM bits,
- roughly half of the kick-count increment, compare, and drain-count fanout width,
- the high four bits of update-width plumbing into the coalescer and SRAM update.

This is a useful reduction, but it is not sufficient by itself to guarantee the
less-than-2k-ALM target. Previous synthesis evidence points at the generated
per-bin coalescer update logic as the dominant resource risk: every incoming
update compares against many bin entries and fans out clear/update decisions. If
the standalone compile remains above target, the next RTL change must replace the
parallel per-bin coalescer bookkeeping with an indexed RAM-style coalescer or an
equivalent single-bin update micro-architecture. That would be a real
micro-architecture fix, not a workaround; synthesis must expose whether the
current V3 RTL still misses the area target.

## 10. Current Implementation Evidence

The current V3 implementation closes standalone timing and the direct
integration/DV path without the histogram ingress bridge. Resource closure is
still open:

- `MAX_COUNT_BITS=20`, `KICK_COUNT_WIDTH=4`, `FIFO_ADDR_WIDTH=2`,
  `COAL_QUEUE_DEPTH=4`, `SAR_TICK_WIDTH=21`, `POWER2_BIN_WIDTH_ONLY=true`.
- Standalone UVM direct-input run:
  `make -C histogram_statistics/tb/uvm run TEST=hist_v3_direct_input_test SEED=7`
  passed with zero UVM errors/fatals.
- Coverage-enabled rerun:
  `make -C histogram_statistics/tb/uvm run_cov TEST=hist_v3_direct_input_test SEED=7 UCDB=/tmp/hist_v3_direct_q4.ucdb`
  passed with zero UVM errors/fatals.
- Type0 evidence: 5 ms RUNNING, 1 ms ping-pong, 100 kHz one selected channel
  per ASIC, CSR readback `last_interval_total=1600`, `live_total=16`,
  `dropped=0`, `coal_status=0x00000400`.
- Type1 evidence: up/down rate and delay modes each report
  `delta_total=256`, `dropped=0`, and `coal_status=0x00000100`.
- Direct FEB V3 `tb_int` evidence:
  `SEED=20260517 firmware_builds/systems/v3_pretest-260511-emutype0-dualport-260512/tb_int/hist_dualport/run_hist_dualport.sh matrix`
  completed 30/30 PASS with zero simulator errors. The run uses
  `RUN_CYCLES=1250000` and `INTERVAL_CYCLES=125000`, so it covers 10 ms of
  RUNNING with a 1 ms ping-pong interval. Type0 rate mode passes 100 kHz,
  500 kHz, and 1 MHz per ASIC for one-random-channel and all-channel patterns.
  Type1 up/down rate and latency modes pass the same rate/pattern matrix with
  zero drops; latency bins are `[128,128]` in the timestamp-sideband test.
- Standalone Quartus command:
  `quartus_sh --flow compile histogram_statistics_v2_signoff -c histogram_statistics_v2_standalone`.
- Standalone synthesis result after the pending-bank/timing fixes:
  `2,745` ALMs, `2,310` registers, `10,240` block memory bits, `2` M10Ks,
  `0` DSPs. This misses the requested `<2k ALM` target.
- Worst setup corner: Slow 1100mV 85C setup slack `+0.809 ns`, TNS `0.000 ns`
  at the 7.273 ns standalone signoff period.
- Largest resource contributors in the fitted hierarchy are top self
  (`1,298.5` ALMs), `pingpong_sram` (`803.8` ALMs), and the 8 ingress FIFOs
  (about `45-47` ALMs each). The next resource fix should reduce the
  `pingpong_sram` bank-valid/pending/readback implementation and the top-level
  CSR/source-select/readback cone.

The current
`firmware_builds/systems/v3_pretest-260511-emutype0-dualport-260512/tb_int/hist_dualport`
direct harness instantiates `histogram_statistics_v2` directly and is valid
direct V3 hist evidence. The older bridge-oriented integration path remains
useful only for exposing the original bridge source-switch bug.
