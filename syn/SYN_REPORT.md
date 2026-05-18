# Synthesis Report: histogram_statistics_v2 V3 direct input profile

Date: 2026-05-17

## Result

PARTIAL. The standalone histogram IP meets the 1.1x timing signoff target with
direct Type0/Type1 inputs, but the current fitted resource is `2,745` ALMs and
therefore still misses the requested `<2k ALM` resource target. This report
intentionally does not use the older `histogram_ingress_bridge` harness as
evidence.

Current bridge-free functional evidence:

```sh
make -C histogram_statistics/tb run_all
```

Result:

```text
47 PASS, 0 FAIL
```

FEB v3 integration evidence:

```text
firmware_builds/systems/v3_pretest-260511/syn/feb_system_v3_qsys_generate_20260515_stream_debug_bridgefree_retry1_isolated.status
exit_code=0
error_count=0
```

The generated FEB v3 synthesis tree wires:

- `mts_preprocessor_0.hit_type1_out` as the 39-bit upper-bank main Type-1 path;
- `mts_preprocessor_1.hit_type1_out` as the 39-bit lower-bank main Type-1 path;
- `mts_preprocessor_0.hit_type1_extended_0` directly to `histogram_statistics_0.hit_type1_extended_0`;
- `mts_preprocessor_1.hit_type1_extended_1` directly to `histogram_statistics_0.hit_type1_extended_1`.

## Archived Previous Evidence

The archived 2026-05-15 bridge-plus-histogram standalone compile used:

```sh
cd histogram_statistics/syn/quartus
quartus_sh --flow compile histogram_statistics_v2_signoff -c histogram_statistics_v2_standalone
quartus_sta -t report_worst_paths.tcl
```

Latest evidence:

```text
histogram_statistics/syn/quartus/output_files/histogram_statistics_v2_standalone.flow.rpt
histogram_statistics/syn/quartus/output_files/histogram_statistics_v2_standalone.fit.summary
histogram_statistics/syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary
histogram_statistics/syn/quartus/output_files/worst_setup_paths.rpt
```

Tool:

```text
Quartus Prime 18.1.0 Build 625 Standard Edition
```

## Project Context

| Item | Value |
|---|---|
| Project | `syn/quartus/histogram_statistics_v2_signoff.qpf` |
| Revision | `histogram_statistics_v2_standalone` |
| Top | `histogram_statistics_v2` |
| Device | Arria V `5AGXBA7D4F31C5` |
| Nominal F_target | `125 MHz` |
| 1.1x signoff target | `137.5 MHz` |
| Period | `7.273 ns` |

V3 resource generics used by the standalone top defaults:

| Generic | Value |
|---|---:|
| `MAX_COUNT_BITS` | 20 |
| `SAR_TICK_WIDTH` | 21 |
| `FIFO_ADDR_WIDTH` | 2 |
| `COAL_QUEUE_DEPTH` | 4 |
| `KICK_COUNT_WIDTH` | 4 |
| `POWER2_BIN_WIDTH_ONLY` | true |
| `N_DEBUG_INTERFACE` | 0 |
| `ENABLE_DEBUG_INPUTS` | false |

## Timing Summary

From `syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary`:

| Corner | Setup Slack | Setup TNS | Hold Slack | MPW Slack |
|---|---:|---:|---:|---:|
| Slow 1100mV 85C | `+0.809 ns` | `0.000 ns` | `+0.271 ns` | `+2.698 ns` |
| Slow 1100mV 0C | `+0.909 ns` | `0.000 ns` | `+0.249 ns` | `+2.664 ns` |
| Fast 1100mV 85C | `+3.438 ns` | `0.000 ns` | `+0.165 ns` | `+2.847 ns` |
| Fast 1100mV 0C | `+3.762 ns` | `0.000 ns` | `+0.154 ns` | `+2.837 ns` |

Worst setup path from `output_files/worst_setup_paths.rpt`:
10 setup paths reported, 0 violated; worst slow-85C slack is `+0.809 ns`.

## Resource Summary

From `syn/quartus/output_files/histogram_statistics_v2_standalone.fit.summary`:

| Resource | Usage |
|---|---:|
| Logic utilization | `2,745 / 91,680 ALMs (3%)` |
| Registers | `2,310` |
| Virtual pins | `978` |
| Block memory bits | `10,240 / 13,987,840 (<1%)` |
| RAM blocks | `2 / 1,366 (<1%)` |
| DSP blocks | `0 / 800 (0%)` |

Largest hierarchy nodes from `output_files/histogram_statistics_v2_standalone.fit.rpt`:

| Node | ALMs | Registers | Memory |
|---|---:|---:|---:|
| top `histogram_statistics_v2` | `2,744.5` total, `1,298.5` self | `2,310` total, `1,076` self | `2` M10Ks |
| `pingpong_sram` | `803.8` total, `774.1` self | `789` total, `747` self | `2` M10Ks |
| 8x `hit_fifo` | about `45-47` each | `34` each | `160` ALM memory total |
| `rr_arbiter` | `101.5` | `33` | `0` |
| `coalescing_queue` | `91.1` | `97` | `0` |
| `bin_divider` | `77.2` | `43` | `0` |

The `<2k ALM` target is still open. The next useful reduction target is the
`pingpong_sram` valid/pending/read-arbitration logic plus the top-level
CSR/selection/readback logic; shrinking the coalescer kick counter to 4 bits is
already in the current profile but is not sufficient by itself.

## Validation

```sh
make -C histogram_statistics/tb/uvm run TEST=hist_v3_direct_input_test SEED=7
make -C histogram_statistics/tb/uvm run_cov TEST=hist_v3_direct_input_test SEED=7 UCDB=/tmp/hist_v3_direct_q4.ucdb
make -C histogram_statistics/tb/uvm run TEST=hist_smoke_test SEED=7
SEED=20260517 firmware_builds/systems/v3_pretest-260511-emutype0-dualport-260512/tb_int/hist_dualport/run_hist_dualport.sh matrix
```

Result:

- `hist_v3_direct_input_test`: FAIL as standalone DV closure evidence. The
  payload checks observe direct Type0 and Type1 hits, but the current rerun
  reports four `hist_pipeline_sva: measure_clear_pulse did not lead to
  flushing` assertion errors. Tracked as BUG-010-H.
- The UVM summary still reports zero UVM errors/fatals; this is not sufficient
  pass evidence after BUG-009-H because simulator assertion errors now fail the
  make target.
- Earlier coverage-enabled `hist_v3_direct_input_test` evidence in `/tmp` is
  retained only as historical payload evidence, not closure evidence for the
  current tree.
- `hist_smoke_test`: PASS, zero UVM errors/fatals.
- Direct FEB V3 `tb_int`: 30/30 PASS, zero simulator errors,
  `RUN_CYCLES=1250000`, `INTERVAL_CYCLES=125000`, report prefix
  `hist_direct_v3_matrix_20260517_021842`.
