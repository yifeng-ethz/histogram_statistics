# Synthesis Report: histogram_statistics_v2 GTS Clear Regression Fix

Date: 2026-05-19

## Result

PASS.  The standalone `histogram_statistics_v2` Quartus signoff project
compiled successfully at the 1.1x timing target for version `26.3.7.0519`.

```sh
cd histogram_statistics/syn/quartus
quartus_sh --flow compile histogram_statistics_v2_signoff -c histogram_statistics_v2_standalone
quartus_sta -t report_worst_paths.tcl
```

Tool:

```text
Quartus Prime 18.1.0 Build 625 Standard Edition
```

Output evidence:

```text
syn/quartus/output_files/histogram_statistics_v2_standalone.flow.rpt
syn/quartus/output_files/histogram_statistics_v2_standalone.fit.summary
syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary
syn/quartus/output_files/worst_setup_paths.rpt
syn/quartus/output_files/worst_hold_paths.rpt
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

## Timing Summary

From `syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary`:

| Corner | Setup Slack | Setup TNS | Hold Slack | MPW Slack |
|---|---:|---:|---:|---:|
| Slow 1100mV 85C | `+0.544 ns` | `0.000 ns` | `+0.256 ns` | `+2.707 ns` |
| Slow 1100mV 0C | `+0.608 ns` | `0.000 ns` | `+0.237 ns` | `+2.674 ns` |
| Fast 1100mV 85C | `+3.326 ns` | `0.000 ns` | `+0.160 ns` | `+2.854 ns` |
| Fast 1100mV 0C | `+3.685 ns` | `0.000 ns` | `+0.147 ns` | `+2.845 ns` |

The explicit `quartus_sta -t report_worst_paths.tcl` run reported 10 setup paths
and 3 hold paths, with 0 violations.  Worst slow-85C setup path:

```text
slack +0.544 ns
from  cfg_interval_cfg[28]
to    pingpong_sram:pingpong_inst|upd_ready_int
```

## Resource Summary

From `syn/quartus/output_files/histogram_statistics_v2_standalone.fit.summary`:

| Resource | Usage |
|---|---:|
| Logic utilization | `2,235 / 91,680 ALMs (2%)` |
| ALMs containing virtual pins | `578 / 91,680 (<1%)` |
| Registers | `1,887` |
| Virtual pins | `1,154` |
| Block memory bits | `10,240 / 13,987,840 (<1%)` |
| RAM blocks | `2 / 1,366 (<1%)` |
| DSP blocks | `0 / 800 (0%)` |

## Notes

- The compile completed with 0 errors and 4735 warnings.  The warning volume is
  dominated by existing virtual-pin and synthesis-report messages in this
  standalone signoff project.
- This report is synthesis/STA evidence only.  DV status for the same patch is
  recorded separately in `tb/DV_REPORT.md`.
