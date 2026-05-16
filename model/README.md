# histogram_statistics model

This directory models the `coalescing_queue` sizing problem for
`histogram_statistics_v2` before using broader DV or synthesis evidence.

The first closure step is TLM versus RTL simulation:

```bash
bash histogram_statistics/model/scripts/run_queue_tlm_rtl.sh
```

The DISLIN plot requested for the realistic 8-MuTRiG link cap is generated with:

```bash
bash histogram_statistics/model/scripts/render_queue_plots.sh
```

The bunched-injection coalescer comparison sweep is generated with:

```bash
bash histogram_statistics/model/scripts/render_bunched_coalescing.sh
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
| `queue_depth_quantiles_8mutrig.csv` | presentation CSV for required depth versus active channels at 8-MuTRiG saturation |
| `queue_depth_quantiles_8mutrig.png` | DISLIN PNG plot for analytical queue-depth regions |
| `queue_depth_quantiles_8mutrig.svg` | DISLIN SVG plot for analytical queue-depth regions |
| `queue_depth_sweep_tlm.csv` | executable TLM sweep over depths, traffic profiles, rates, and periodic drain stalls |
| `queue_loss_vs_depth_8mutrig.csv` | 8-MuTRiG, 256-channel loss fraction versus queue depth for iid, physical-hit, and injection traffic |
| `queue_loss_vs_depth_8mutrig.png` | DISLIN PNG plot of `queue_loss_vs_depth_8mutrig.csv` |
| `queue_loss_vs_depth_8mutrig.svg` | DISLIN SVG plot of `queue_loss_vs_depth_8mutrig.csv` |
| `queue_dislin_png.log` | DISLIN render log for the PNG output |
| `queue_dislin_svg.log` | DISLIN render log for the SVG output |
| `queue_trace_stimulus.csv` | deterministic validation trace driven into both TLM and RTL |
| `queue_trace_expected_tlm.csv` | cycle-level expected TLM outputs |
| `queue_trace_observed_rtl.csv` | cycle-level RTL outputs from `coalescing_queue.vhd` |
| `queue_trace_compare.csv` | cycle-level mismatch report; PASS requires zero mismatches |
| `queue_trace_tlm_rtl_match.png` | DISLIN PNG plot of TLM/RTL occupancy and overflow agreement |
| `queue_trace_tlm_rtl_match.svg` | DISLIN SVG plot of TLM/RTL occupancy and overflow agreement |
| `bunched_coalescing_rate_sweep.csv` | TLM rate sweep comparing per-bin and tag-addressable CB coalescers under all-bin bunched injection |
| `bunched_coalescing_rate_sweep.png` | DISLIN PNG shaded-band plot of pending-cell min/p50/max versus offered push rate |
| `bunched_coalescing_rate_sweep.svg` | DISLIN SVG shaded-band plot of pending-cell min/p50/max versus offered push rate |
| `bunched_coalescing_kick_sweep.png` | DISLIN PNG shaded-band plot of live-cell kick-count min/p50/max versus offered push rate |
| `bunched_coalescing_kick_sweep.svg` | DISLIN SVG shaded-band plot of live-cell kick-count min/p50/max versus offered push rate |
| `iid_poisson_coalescing_rate_sweep.csv` | TLM rate sweep for 256 independent Poisson bin sources feeding a source FIFO before the hist IP |
| `iid_poisson_coalescing_rate_sweep.png` | DISLIN PNG shaded-band plot of pending-cell min/p50/max for the iid-Poisson source-FIFO case |
| `iid_poisson_coalescing_rate_sweep.svg` | DISLIN SVG shaded-band plot of pending-cell min/p50/max for the iid-Poisson source-FIFO case |
| `iid_poisson_coalescing_kick_sweep.png` | DISLIN PNG shaded-band plot of live-cell kick-count min/p50/max for the iid-Poisson source-FIFO case |
| `iid_poisson_coalescing_kick_sweep.svg` | DISLIN SVG shaded-band plot of live-cell kick-count min/p50/max for the iid-Poisson source-FIFO case |
| `bunched_infinite_queue_rate_sweep.csv` | capacity-infinite deterministic-service reference for bunched update cells and latency |
| `bunched_infinite_queue_rate_sweep.png` | DISLIN PNG plot of the capacity-infinite bunched update-cell reference |
| `bunched_infinite_queue_rate_sweep.svg` | DISLIN SVG plot of the capacity-infinite bunched update-cell reference |
| `bunched_coalescing_dislin_png.log` | DISLIN render log for finite-coalescer PNG outputs |
| `bunched_coalescing_dislin_svg.log` | DISLIN render log for finite-coalescer SVG outputs |

Queue depth cannot fix kick-counter saturation. The exact-region CSV reports
queue-slot overflow separately from hotspot kick saturation risk where it can be
computed exactly.

## Bunched Coalescer Sweep Contract

The bunched comparison model is a deterministic TLM for comparing:

- the original fixed per-bin coalescer, where every bin owns one 8-bit
  saturating pending counter;
- a tag-addressable circular-buffer coalescer, where each live CB cell is
  `(bin_tag, kick_count)` and repeated hits to a live tag increment that cell.

The default sweep uses `N_BINS=256`, `125 MHz`, `2` pushed bin hits/cycle, and
an update engine service rate of `0.5` cell/cycle. Every injection pulse fires
all bins exactly once in bin order. The x-axis is aggregate offered push rate:
`push_rate_per_s = pulse_rate_hz * N_BINS`. The y-axis statistics are sampled
over steady-state cycles after warm-up:

- `min_cells`: minimum pending update cells;
- `p50_cells`: median pending update cells;
- `max_cells`: maximum pending update cells;
- `min_kick_count`, `p50_kick_count`, `max_kick_count`: live-cell kick-count
  statistics over nonzero pending cells.

At `100 kHz/bin`, the aggregate offered push rate is `25.6 M bin-updates/s`.
With concurrent push and drain, both modeled coalescers report
`min/p50/max = 0/0/192` pending cells. If the update engine were held off until
the whole 256-bin burst had been accepted, the single-burst peak would instead
be 256 cells.

The iid-Poisson sweep uses the same aggregate rate definition, but generates
256 independent Poisson bin sources. Their arrivals first enter an infinite
source FIFO. The hist IP ingress then consumes that FIFO at up to `2` hits/cycle,
while the update engine drains pending update cells at `0.5` cell/cycle. The
Poisson CSV therefore reports both hist-IP coalescer statistics and source-FIFO
columns: `source_arrivals`, `source_fifo_min_cells`, `source_fifo_p50_cells`,
`source_fifo_max_cells`, and `source_fifo_mean_cells`.
