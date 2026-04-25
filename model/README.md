# histogram_statistics model

This directory models the `coalescing_queue` sizing problem for
`histogram_statistics_v2` before using broader DV or synthesis evidence.

The first closure step is TLM versus RTL simulation:

```bash
bash histogram_statistics/model/scripts/run_queue_tlm_rtl.sh
```

Generated artifacts are written to `histogram_statistics/model/artifacts/`.

## Model Contract

The source of truth is [`../rtl/coalescing_queue.vhd`](../rtl/coalescing_queue.vhd):

- one post-divider hit can enter per clock;
- one queued element can drain per clock when `i_drain_ready=1`;
- each queue entry is a bin tag with an 8-bit saturating kick counter;
- a hit to an already queued tag increments that tag's kick counter;
- a hit to a new tag allocates one queue slot when room exists;
- overflow is caused by either a new tag while the queue is full, or by kick
  counter saturation at 255.

The analytical model reports exact queue-depth regions for iid traffic during a
bounded drain-stall window. The executable TLM then uses the same state update
order as the RTL and is compared cycle-by-cycle against a VHDL RTL simulation.

## Outputs

| Artifact | Meaning |
|---|---|
| `queue_depth_exact_regions.csv` | exact iid depth quantiles for no-drop, 5%, 1%, 0.1%, and 1 ppm queue-overflow probability |
| `queue_depth_sweep_tlm.csv` | executable TLM sweep over depths, traffic profiles, rates, and periodic drain stalls |
| `queue_trace_stimulus.csv` | deterministic validation trace driven into both TLM and RTL |
| `queue_trace_expected_tlm.csv` | cycle-level expected TLM outputs |
| `queue_trace_observed_rtl.csv` | cycle-level RTL outputs from `coalescing_queue.vhd` |
| `queue_trace_compare.csv` | cycle-level mismatch report; PASS requires zero mismatches |

Queue depth cannot fix kick-counter saturation. The exact-region CSV reports
queue-slot overflow separately from hotspot kick saturation risk where it can be
computed exactly.
