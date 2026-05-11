# Synthesis Report: histogram_statistics_v2 standalone refresh

Date: 2026-05-11

## Result

FAIL. The current committed `histogram_statistics_v2` VERSION `26.1.6.0429`
does not meet standalone timing at the 1.1x signoff target.

Command:

```sh
quartus_sh --flow compile histogram_statistics_v2_signoff -c histogram_statistics_v2_standalone
```

Tool:

```text
Quartus Prime 18.1.0 Build 625 Standard Edition
```

Project context:

| Item | Value |
|---|---|
| Project | `syn/quartus/histogram_statistics_v2_signoff.qpf` |
| Revision | `histogram_statistics_v2_standalone` |
| Device | Arria V `5AGXBA7D4F31C5` |
| Nominal F_target | `125 MHz` |
| 1.1x signoff target | `137.5 MHz` |
| Period | `7.273 ns` |

The full compile completed successfully with `0 errors, 10022 warnings`; timing
signoff fails because one setup corner is negative.

## Timing Summary

From `syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary`
and `syn/quartus/output_files/histogram_statistics_v2_standalone.sta.rpt`:

| Corner | Setup Slack | Setup TNS | Hold Slack | MPW Slack | Fmax |
|---|---:|---:|---:|---:|---:|
| Slow 1100mV 85C | `-0.234 ns` | `-6.359 ns` | `0.274 ns` | `2.698 ns` | `133.21 MHz` |
| Slow 1100mV 0C | `0.036 ns` | `0.000 ns` | `0.255 ns` | `2.667 ns` | `138.18 MHz` |
| Fast 1100mV 85C | `2.459 ns` | `0.000 ns` | `0.162 ns` | `2.849 ns` | n/a |
| Fast 1100mV 0C | `2.950 ns` | `0.000 ns` | `0.148 ns` | `2.839 ns` | n/a |

Worst clock: `i_clk`. Worst setup slack: `-0.234 ns` at Slow 1100mV 85C.

## Failing Path

From `syn/quartus/output_files/histogram_statistics_v2_standalone.worst_setup_paths.rpt`:

| Corner | From | To | Data delay | Clock skew | Logic levels | Slack |
|---|---|---|---:|---:|---:|---:|
| Slow 1100mV 85C | `queue_hit_bin[3]` | `coalescing_queue:queue_inst|overflow_count_q[0]` | `7.429 ns` | `-0.078 ns` | `7` | `-0.234 ns` |

## Resource Summary

From `syn/quartus/output_files/histogram_statistics_v2_standalone.fit.summary`:

| Resource | Usage |
|---|---:|
| Logic utilization | `13,834 / 91,680 ALMs (15%)` |
| Registers | `5,248` |
| Virtual pins | `699` |
| Block memory bits | `16,384 / 13,987,840 (<1%)` |
| RAM blocks | `2 / 1,366 (<1%)` |
| DSP blocks | `0 / 800 (0%)` |

## Static Screen

Questa static screen for the current v2 standalone cone:

| Check | Result | Report |
|---|---|---|
| Lint errors | `0` | `tb/static_screen_20260511_v2cone/qverify_db/lint.rpt` |
| CDC violations | `0` | `tb/static_screen_20260511_v2cone/reports/cdc.rpt` |
| RDC violations | `0` | `tb/static_screen_20260511_v2cone/qverify_db/rdc.rpt` |

## Closure Note

Status: REGRESSION. Failed standalone signoff at corner Slow 1100mV 85C after
VERSION `26.1.6.0429`; integration must apply an SDC false_path only if the
path is architecturally false, or the IP must register-split the path.
