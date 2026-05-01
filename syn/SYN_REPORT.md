# Synthesis Report: histogram_statistics_v2 26.1.7.0501

Date: 2026-05-01

## Result

PASS. Standalone Quartus full compile closed timing at the 7.273 ns signoff
period on Arria V `5AGXBA7D4F31C5`.

Command:

```sh
quartus_sh --flow compile histogram_statistics_v2_signoff -c histogram_statistics_v2_standalone
```

Log:

```text
syn/quartus/histogram_statistics_v2_standalone_compile_26_1_7_0501_final.log
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
| Slow 1100mV 85C | +0.210 ns | +0.270 ns | +2.710 ns | 0.000 ns | 0.000 ns |
| Slow 1100mV 0C | +0.405 ns | +0.251 ns | +2.694 ns | 0.000 ns | 0.000 ns |
| Fast 1100mV 85C | +2.816 ns | +0.160 ns | +2.821 ns | 0.000 ns | 0.000 ns |
| Fast 1100mV 0C | +3.300 ns | +0.149 ns | +2.833 ns | 0.000 ns | 0.000 ns |

Fmax from
`syn/quartus/output_files/histogram_statistics_v2_standalone.sta.rpt`:

| Corner | Fmax |
|---|---:|
| Slow 1100mV 85C | 141.58 MHz |
| Slow 1100mV 0C | 145.60 MHz |

The Timing Analyzer reports the design fully constrained for setup, hold, and
minimum pulse width.

## Resource Summary

From
`syn/quartus/output_files/histogram_statistics_v2_standalone.fit.summary` and
`syn/quartus/output_files/histogram_statistics_v2_standalone.fit.rpt`:

| Resource | Usage |
|---|---:|
| Logic utilization | 14,020 / 91,680 ALMs (15%) |
| Total LABs | 1,693 / 9,168 (18%) |
| Dedicated logic registers | 5,309 |
| Block memory bits | 16,384 / 13,987,840 (<1%) |
| RAM blocks | 2 / 1,366 (<1%) |
| MLAB memory bits | 69,632 |
| DSP blocks | 0 / 800 |
| Peak interconnect usage | 38.6% / 37.8% / 41.2% |

## Flow Runtime

From
`syn/quartus/output_files/histogram_statistics_v2_standalone.flow.rpt`:

| Stage | Elapsed | CPU Time |
|---|---:|---:|
| Analysis & Synthesis | 00:00:40 | 00:01:05 |
| Fitter | 00:04:18 | 00:09:55 |
| Assembler | 00:00:14 | 00:00:14 |
| Timing Analyzer | 00:00:24 | 00:00:53 |
| Total | 00:05:36 | 00:12:07 |

## Closure Changes

- `coalescing_queue`: added a one-cycle overflow event register so diagnostic
  overflow counting no longer places the hit-bin decode on a wide counter enable.
- `histogram_statistics_v2`: decodes negative debug mode once when the CSR
  configuration is applied, instead of feeding signed mode comparisons into the
  ingress write-request path.
- `histogram_statistics_v2`: registers the selected debug-source samples before
  ingress filtering, removing `cfg_debug_source` from the same-cycle hot path.
- `histogram_statistics_v2_hw.tcl`: allows `CHANNELS_PER_PORT=0` for already
  global stream keys such as Phase-6 `{ASIC, channel}` histogram paths.

The standalone project still emits expected virtual-pin warnings because it is
an IP signoff wrapper, not the final FEB top-level compile.
