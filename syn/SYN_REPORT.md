# Synthesis Report: histogram_statistics_v2 Phase-5 Standalone

Date: 2026-04-27

## Result

PASS. Standalone Quartus full compile closed timing at the 7.273 ns signoff
period on Arria V `5AGXBA7D4F31C5`.

Command:

```sh
quartus_sh --flow compile histogram_statistics_v2_signoff -c histogram_statistics_v2_standalone
```

Tool:

```text
Quartus Prime 18.1.0 Build 625 Standard Edition
```

## Timing Summary

| Corner | Setup Slack | Hold Slack | Setup TNS | Hold TNS |
|---|---:|---:|---:|---:|
| Slow 1100mV 85C | +0.129 ns | +0.256 ns | 0.000 ns | 0.000 ns |
| Slow 1100mV 0C | +0.246 ns | +0.238 ns | 0.000 ns | 0.000 ns |
| Fast 1100mV 85C | +2.585 ns | +0.161 ns | 0.000 ns | 0.000 ns |
| Fast 1100mV 0C | +3.158 ns | +0.147 ns | 0.000 ns | 0.000 ns |

The Timing Analyzer reported the design fully constrained for setup and hold.

## Resource Summary

| Resource | Usage |
|---|---:|
| Logic utilization | 12,865 / 91,680 ALMs (14%) |
| Registers | 5,178 |
| Block memory bits | 16,384 / 13,987,840 (<1%) |
| RAM blocks | 2 / 1,366 (<1%) |
| MLAB memory bits | 69,632 |
| DSP blocks | 0 / 800 |

## Closure Changes

- `hit_fifo` keeps a registered FIFO head for the 256-entry Phase-5 FIFO.
- `coalescing_queue` updates peak occupancy from the registered queue level,
  removing the hit-bin decode path from `queue_level_max`.
- `histogram_statistics_v2` replicates active filter fields per ingress port
  so the hot match path is not driven by one high-fanout shared CSR source.

Quartus emits expected standalone virtual-pin warnings because this is an IP
signoff wrapper, not the final FEB top-level compile.
