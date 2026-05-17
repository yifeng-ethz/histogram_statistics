# Synthesis Signoff: histogram_statistics_v2 V3 direct input profile

Status: `PARTIAL - TIMING PASS, RESOURCE TARGET OPEN`

This signoff was refreshed on `2026-05-17` using the canonical standalone
Quartus project in [syn/quartus](syn/quartus). The evidence is for the direct
FEB V3 histogram IP profile, not the older bridge-plus-histogram harness.

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
| Flow status | `Successful - Sun May 17 02:13:06 2026` |
| Nominal F_target | `125 MHz` |
| 1.1x signoff target | `137.5 MHz` |
| Constrained period | `7.273 ns` |
| Constraint file | `syn/quartus/histogram_statistics_v2_standalone.sdc` |

## Evidence Paths

| Artifact | Path |
|---|---|
| Compile log | `/tmp/hist_quartus_q4.log` |
| Flow report | `syn/quartus/output_files/histogram_statistics_v2_standalone.flow.rpt` |
| Fit summary | `syn/quartus/output_files/histogram_statistics_v2_standalone.fit.summary` |
| Fit full report | `syn/quartus/output_files/histogram_statistics_v2_standalone.fit.rpt` |
| STA summary | `syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary` |
| STA full report | `syn/quartus/output_files/histogram_statistics_v2_standalone.sta.rpt` |
| Worst setup report | `syn/quartus/output_files/worst_setup_paths.rpt` |
| Worst hold report | `syn/quartus/output_files/worst_hold_paths.rpt` |

## Timing Closure

From [syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary](syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary):

| Corner | Setup Slack | Setup TNS | Hold Slack | Min Pulse Width Slack |
|---|---:|---:|---:|---:|
| Slow 1100mV 85C | `+0.809 ns` | `0.000 ns` | `+0.271 ns` | `+2.698 ns` |
| Slow 1100mV 0C | `+0.909 ns` | `0.000 ns` | `+0.249 ns` | `+2.664 ns` |
| Fast 1100mV 85C | `+3.438 ns` | `0.000 ns` | `+0.165 ns` | `+2.847 ns` |
| Fast 1100mV 0C | `+3.762 ns` | `0.000 ns` | `+0.154 ns` | `+2.837 ns` |

Worst setup-path report after `quartus_sta -t report_worst_paths.tcl`: 10 setup
paths reported, 0 violated; worst slow-85C slack is `+0.809 ns`.

## Resource Summary

From [syn/quartus/output_files/histogram_statistics_v2_standalone.fit.summary](syn/quartus/output_files/histogram_statistics_v2_standalone.fit.summary):

| Resource | Usage |
|---|---:|
| Logic utilization | `2,745 / 91,680 ALMs (3%)` |
| Total registers | `2,310` |
| Total pins | `0 / 426 (0%)` |
| Total virtual pins | `978` |
| Total block memory bits | `10,240 / 13,987,840 (<1%)` |
| Total RAM blocks | `2 / 1,366 (<1%)` |
| Total DSP blocks | `0 / 800 (0%)` |

Largest hierarchy nodes from the fitter report:

| Node | ALMs | Registers | Memory |
|---|---:|---:|---:|
| top `histogram_statistics_v2` | `2,744.5` total, `1,298.5` self | `2,310` total, `1,076` self | `2` M10Ks |
| `pingpong_sram` | `803.8` total, `774.1` self | `789` total, `747` self | `2` M10Ks |
| 8x `hit_fifo` | about `45-47` ALMs each | `34` regs each | `160` ALM memory total |
| `rr_arbiter` | `101.5` ALMs | `33` regs | `0` |
| `coalescing_queue` | `91.1` ALMs | `97` regs | `0` |
| `bin_divider` | `77.2` ALMs | `43` regs | `0` |

## Resource Reduction Notes

The 4-bit coalesced kick counter saves `256 * 4 = 1024` stored kick-count bits
relative to the legacy 8-bit counter and halves the increment/drain count width.
The larger savings came from using the V3 resource profile explicitly:

- 20-bit bin/stat counters rather than 32-bit counters,
- 4 live coalescer cells rather than the legacy large live table,
- 4-entry ingress FIFOs for the direct V3 profile,
- shift-based power-of-two bin mapping instead of the arbitrary-width divider,
- removal of preserve attributes and redundant coalescer scrub logic.

## Decision

Standalone IP timing signoff is green for the V3 direct-input profile: all
available setup/hold corners close at the tightened 7.273 ns standalone period.
Resource signoff is still open because `2,745` ALMs exceeds the requested
`<2k ALM` target. The next reduction work should focus on `pingpong_sram`
bank-valid/pending/readback logic and the top-level CSR/source-select/readback
cone.
