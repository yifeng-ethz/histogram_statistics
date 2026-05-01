# Synthesis Report: histogram_statistics_v2 26.1.9.0501

Date: 2026-05-01

## Result

WARN. Standalone Quartus full compile completed with 0 errors at the 7.273 ns
over-constrained signoff period on Arria V `5AGXBA7D4F31C5`, but the slow
85C setup corner has a small -0.051 ns WNS. This is inside the project-level
Arria V bring-up tolerance for non-ambiguous debug paths, but it is not a
strict 1.1x standalone timing close.

Command:

```sh
quartus_sh --flow compile histogram_statistics_v2_signoff -c histogram_statistics_v2_standalone
```

Log:

```text
syn/quartus/histogram_statistics_v2_standalone_compile_26_1_9_delay_t_20260501.log
```

Tool:

```text
Quartus Prime 18.1.0 Build 625 Standard Edition
```

## Timing Summary

From
`syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary`:

| Corner | Setup Slack | Hold Slack | Min Pulse Width Slack | Setup TNS | Hold TNS |
|---|---:|---:|---:|---:|---:|
| Slow 1100mV 85C | -0.051 ns | +0.267 ns | +2.707 ns | -0.054 ns | 0.000 ns |
| Slow 1100mV 0C | +0.152 ns | +0.250 ns | +2.692 ns | 0.000 ns | 0.000 ns |
| Fast 1100mV 85C | +2.611 ns | +0.160 ns | +2.831 ns | 0.000 ns | 0.000 ns |
| Fast 1100mV 0C | +3.093 ns | +0.146 ns | +2.839 ns | 0.000 ns | 0.000 ns |

Fmax from
`syn/quartus/output_files/histogram_statistics_v2_standalone.sta.rpt`:

| Corner | Fmax |
|---|---:|
| Slow 1100mV 85C | 136.54 MHz |
| Slow 1100mV 0C | 140.43 MHz |

The design is fully constrained for hold and minimum pulse width. The worst
setup paths are archived in
`syn/quartus/output_files/histogram_statistics_v2_standalone.worst_setup_paths.rpt`;
the two violated paths are configuration/debug-mode decode paths ending at
`ingress_stage_write_req` / `ingress_stage_key`, not normal hit-data RAM
write paths.

## Resource Summary

From
`syn/quartus/output_files/histogram_statistics_v2_standalone.fit.summary` and
`syn/quartus/output_files/histogram_statistics_v2_standalone.fit.rpt`:

| Resource | Usage |
|---|---:|
| Logic utilization | 14,112 / 91,680 ALMs (15%) |
| Total LABs | 1,689 / 9,168 (18%) |
| Dedicated logic registers | 5,323 |
| Block memory bits | 16,384 / 13,987,840 (<1%) |
| RAM blocks | 2 / 1,366 (<1%) |
| MLAB memory bits | 69,632 |
| DSP blocks | 0 / 800 |
| Peak interconnect usage | 35.0% / 34.1% / 38.0% |

## Flow Runtime

From
`syn/quartus/output_files/histogram_statistics_v2_standalone.flow.rpt`:

| Stage | Elapsed | CPU Time |
|---|---:|---:|
| Analysis & Synthesis | 00:00:43 | 00:01:08 |
| Fitter | 00:04:34 | 00:10:35 |
| Assembler | 00:00:13 | 00:00:14 |
| Timing Analyzer | 00:00:24 + 00:00:09 | 00:00:54 + 00:00:12 |
| Total | 00:06:03 | 00:13:03 |

## Closure Changes

- `histogram_statistics_v2`: added positive `mode=+1` normal-hit delay
  histogramming. The key is
  `local_run_counter[12:0] - hit_type1.data[29:17]`, so ASIC/channel filtering
  stays on the normal ingress path and all eight ASIC inputs can be scanned.
- `histogram_statistics_v2`: resets the local delay counter on run-control
  `SYNC` (`asi_ctrl_data = "000000100"`), matching the clean IDLE to RUNNING
  sequence used by the FE SciFi toolkit.
- `histogram_statistics_v2_hw.tcl` and SVD: documented `mode=+1` and bumped
  the packaged identity to `26.1.9.0501`.
- `coalescing_queue`: added a one-cycle overflow event register so diagnostic
  overflow counting no longer places the hit-bin decode on a wide counter enable.
- `histogram_statistics_v2`: decodes negative debug mode once when the CSR
  configuration is applied, instead of feeding signed mode comparisons into the
  ingress write-request path.
- `histogram_statistics_v2`: registers the selected debug-source samples before
  ingress filtering, removing `cfg_debug_source` from the same-cycle hot path.
- `histogram_statistics_v2_hw.tcl`: allows `CHANNELS_PER_PORT=0` for already
  global stream keys such as Phase-6 `{ASIC, channel}` histogram paths.
- `pingpong_sram`: keeps deferred host reads on the frozen bank when
  `ENABLE_PINGPONG=true`, matching the immediate-read path and preventing live
  rate-bin dumps from mixing last-interval bins with active-interval bins.

The standalone project still emits expected virtual-pin warnings because it is
an IP signoff wrapper, not the final FEB top-level compile.
