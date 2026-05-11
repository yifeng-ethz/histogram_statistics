# Synthesis Signoff: histogram_statistics_v2

Status: `FAIL / REGRESSION`

This signoff was refreshed on `2026-05-11` using the canonical standalone
Quartus project in [syn/quartus](syn/quartus). The current committed
`histogram_statistics_v2` VERSION `26.1.6.0429` fails the 1.1x standalone
target at Slow 1100mV 85C with `-0.234 ns` setup slack on `i_clk`.

The detailed evidence record is [doc/SIGNOFF.md](doc/SIGNOFF.md). The concise
synthesis report is [syn/SYN_REPORT.md](syn/SYN_REPORT.md).

## Build Context

| Item | Value |
|---|---|
| Project | `histogram_statistics_v2_signoff` |
| Revision | `histogram_statistics_v2_standalone` |
| Top-level entity | `histogram_statistics_v2` |
| Device family | Arria V |
| Device | `5AGXBA7D4F31C5` |
| Quartus version | `18.1.0 Build 625 09/12/2018 SJ Standard Edition` |
| Flow report timestamp | `Mon May 11 11:04:51 2026` |
| Flow status | `Successful - Mon May 11 10:59:18 2026` |
| Compile log end | `Mon May 11 10:59:44 2026` |
| Worst-path STA log end | `Mon May 11 11:04:51 2026` |
| Nominal F_target | `125 MHz` |
| 1.1x signoff target | `137.5 MHz` |
| Constrained period | `7.273 ns` |
| Constraint file | `syn/quartus/histogram_statistics_v2_standalone.sdc` |

## Evidence Paths

| Artifact | Path |
|---|---|
| Compile log | `syn/quartus/compile_histogram_statistics_v2_standalone_20260511.log` |
| Flow report | `syn/quartus/output_files/histogram_statistics_v2_standalone.flow.rpt` |
| Fit summary | `syn/quartus/output_files/histogram_statistics_v2_standalone.fit.summary` |
| STA summary | `syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary` |
| STA full report | `syn/quartus/output_files/histogram_statistics_v2_standalone.sta.rpt` |
| Worst setup report | `syn/quartus/output_files/histogram_statistics_v2_standalone.worst_setup_paths.rpt` |
| Static screen log | `tb/static_screen_20260511_v2cone/questa_static_screen.log` |

The full compile completed successfully with `0 errors, 10022 warnings`.

## Timing Closure

From
[syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary](syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary)
and
[syn/quartus/output_files/histogram_statistics_v2_standalone.sta.rpt](syn/quartus/output_files/histogram_statistics_v2_standalone.sta.rpt):

| Corner | Fmax | Setup Slack | Setup TNS | Hold Slack | Min Pulse Width Slack |
|---|---:|---:|---:|---:|---:|
| Slow 1100mV 85C | `133.21 MHz` | `-0.234 ns` | `-6.359 ns` | `0.274 ns` | `2.698 ns` |
| Slow 1100mV 0C | `138.18 MHz` | `0.036 ns` | `0.000 ns` | `0.255 ns` | `2.667 ns` |
| Fast 1100mV 85C | n/a | `2.459 ns` | `0.000 ns` | `0.162 ns` | `2.849 ns` |
| Fast 1100mV 0C | n/a | `2.950 ns` | `0.000 ns` | `0.148 ns` | `2.839 ns` |

Worst signoff corner:

- clock: `i_clk`
- worst setup slack: `-0.234 ns`
- worst setup TNS: `-6.359 ns`
- worst hold slack: `0.148 ns`
- worst minimum pulse-width slack: `2.667 ns`
- worst Fmax: `133.21 MHz`

Margin assessment:

- required signoff frequency: `137.5 MHz`
- achieved worst-case Fmax: `133.21 MHz`
- frequency headroom: `-4.29 MHz`

## Failing Path

From
`syn/quartus/output_files/histogram_statistics_v2_standalone.worst_setup_paths.rpt`:

| Corner | From | To | Data delay | Clock skew | Logic levels | Slack |
|---|---|---|---:|---:|---:|---:|
| Slow 1100mV 85C | `queue_hit_bin[3]` | `coalescing_queue:queue_inst|overflow_count_q[0]` | `7.429 ns` | `-0.078 ns` | `7` | `-0.234 ns` |

## Resource Summary

From
[syn/quartus/output_files/histogram_statistics_v2_standalone.fit.summary](syn/quartus/output_files/histogram_statistics_v2_standalone.fit.summary):

| Resource | Usage |
|---|---:|
| Logic utilization | `13,834 / 91,680 ALMs (15%)` |
| Total registers | `5,248` |
| Total pins | `0 / 426 (0%)` |
| Total virtual pins | `699` |
| Total block memory bits | `16,384 / 13,987,840 (<1%)` |
| Total RAM blocks | `2 / 1,366 (<1%)` |
| Total DSP blocks | `0 / 800 (0%)` |

## Static Screen

The current v2 standalone cone was screened with the Questa static wrapper using
`tb/histogram_statistics_v2_static.f`.

| Check | Result | Report |
|---|---|---|
| Lint errors | `0` | `tb/static_screen_20260511_v2cone/qverify_db/lint.rpt` |
| CDC violations | `0` | `tb/static_screen_20260511_v2cone/reports/cdc.rpt` |
| RDC violations | `0` | `tb/static_screen_20260511_v2cone/qverify_db/rdc.rpt` |

## Decision

Standalone IP signoff requires setup slack `>= 0 ns` at every available
standalone signoff corner. VERSION `26.1.6.0429` fails that rule at Slow
1100mV 85C. Newest version fails standalone signoff; integration must apply an
SDC false_path only if the arc is architecturally false, or the IP must
register-split the path.
