# histogram_statistics coalescing queue model report

Date: 2026-04-25

## Scope

This report covers the first modeling gate for the `histogram_statistics_v2`
coalescing queue: analytical/TLM queue-depth modeling and cycle-level RTL
simulation comparison. DV expansion and synthesis trade-off work are deferred
until this TLM/RTL basis is stable.

## RTL Contract Modeled

Source RTL: [`../rtl/coalescing_queue.vhd`](../rtl/coalescing_queue.vhd)

- one post-divider hit can arrive per clock;
- one queued element can drain per clock when `i_drain_ready=1`;
- queue entries are bin tags;
- each tag has an 8-bit saturating kick counter;
- repeated hits to a queued tag increment `kick_count`;
- new tags allocate queue slots;
- overflow is either queue-full/new-tag or kick-counter saturation.

The TLM intentionally models the registered RTL observable behavior, including
`o_drain_count` reflecting the current head-bin kick value even when
`o_drain_valid=0`.

## Exact Analytical Depth Regions

Artifact: [`artifacts/queue_depth_exact_regions.csv`](artifacts/queue_depth_exact_regions.csv)

The exact table computes the distribution of distinct queued tags for iid
traffic during a bounded no-drain window. For each traffic class, stall length,
and hit probability per cycle, it reports:

- worst-case no-drop queue depth;
- minimum depth for queue-overflow probability <= 5%, 1%, 0.1%, and 1 ppm;
- default-depth-160 overflow probability;
- exact single-bin kick-saturation probability where applicable.

Key `stall_cycles=256`, `hit_probability_per_cycle=1.00` rows:

| traffic | worst-case no-drop depth | depth at <= 1 ppm queue overflow | depth-160 queue overflow probability | kick saturation probability |
|---|---:|---:|---:|---:|
| single-bin hotspot | 1 | 1 | 0 | 1 |
| local cluster-8 | 8 | 8 | 0 | n/a |
| one-port 32-bin uniform | 32 | 32 | 0 | n/a |
| uniform 256-bin iid | 256 | 185 | 0.619276021992 | n/a |

Interpretation:

- `COAL_QUEUE_DEPTH=160` is enough for local-cluster and one-port 32-bin stall
  windows at this stress point.
- `COAL_QUEUE_DEPTH=160` is not enough for iid all-256 traffic during a
  256-cycle no-drain window at one hit per cycle; the exact queue-overflow
  probability is about 61.9%.
- `COAL_QUEUE_DEPTH=256` is the exact no-drop queue-slot depth for adversarial
  all-bin traffic with 256 active bins.
- No queue depth can prevent single-bin kick saturation if more than 255 hits
  accumulate for that tag before it drains.

## TLM Sweep

Artifact: [`artifacts/queue_depth_sweep_tlm.csv`](artifacts/queue_depth_sweep_tlm.csv)

The executable TLM sweep covers:

- traffic profiles: single-bin hotspot, local cluster-8, 32-bin, 128-bin,
  uniform 256-bin, and sequential 256-bin scan;
- queue depths: 1 through 256, including 128, 144, 160, 192, 224, and 256;
- hit probabilities per cycle: 0.10, 0.25, 0.50, 0.75, 1.00;
- periodic drain stalls of 0, 64, 128, and 256 cycles per 1024-cycle window.

The sweep is intentionally short enough to keep the TLM/RTL gate interactive.
The exact-region CSV above is the signoff source for iid queue-depth
probabilities.

## RTL Simulation Match

Runner: [`scripts/run_queue_tlm_rtl.sh`](scripts/run_queue_tlm_rtl.sh)

RTL bench: [`rtl_sim/tb_coalescing_queue_trace.vhd`](rtl_sim/tb_coalescing_queue_trace.vhd)

Comparison artifacts:

- [`artifacts/queue_trace_stimulus.csv`](artifacts/queue_trace_stimulus.csv)
- [`artifacts/queue_trace_expected_tlm.csv`](artifacts/queue_trace_expected_tlm.csv)
- [`artifacts/queue_trace_observed_rtl.csv`](artifacts/queue_trace_observed_rtl.csv)
- [`artifacts/queue_trace_compare.csv`](artifacts/queue_trace_compare.csv)
- [`artifacts/queue_trace_rtl.log`](artifacts/queue_trace_rtl.log)

Result:

```text
QuestaSim-64 2026.1_1
vcom: Errors 0, Warnings 0
vsim: Errors 0, Warnings 0
TLM/RTL comparison PASS: 1190 cycles, 0 mismatches
```

The validation trace includes:

- repeated same-bin coalescing while drain is ready;
- queue-full/new-tag overflow at `QUEUE_DEPTH=160`;
- long same-bin no-drain burst that triggers kick saturation;
- full drain after each stress segment.

## Next Gate

The next step is to add DISLIN plots for the modeled variable space:

- 1D loss probability versus hit rate, with `Model/TLM` and `RTL SIM` traces;
- 2D loss probability versus hit rate and burstiness/active-bin count, using
  identical axes and color scale;
- later, a third `BOARD` legend entry once on-board data exists.
