# histogram_statistics coalescing queue model report

Date: 2026-05-16

This report is the inline presentation for the first
`histogram_statistics_v2` coalescing-queue modeling gate. The evidence stack is
analytical model, executable TLM, then RTL simulation. No board evidence is
claimed here.

Source RTL: [`../rtl/coalescing_queue.vhd`](../rtl/coalescing_queue.vhd)

Model contract:

- one post-divider hit can arrive per clock;
- one queued element can drain per clock when `i_drain_ready=1`;
- queue entries are histogram-bin tags;
- each queued tag has an 8-bit saturating kick counter;
- repeated hits to a queued tag coalesce by incrementing `kick_count`;
- a new tag allocates one queue slot;
- overflow is either queue-full/new-tag or kick-counter saturation.

## Set 1: Analytical Queue Depth Regions

![Analytical queue depth regions](artifacts/queue_depth_quantiles_8mutrig.png)

Data:
[`artifacts/queue_depth_quantiles_8mutrig.csv`](artifacts/queue_depth_quantiles_8mutrig.csv)

For iid traffic over `m` active channels, let `D_t` be the number of distinct
tags seen during a no-drain window after `t` cycles. With per-cycle hit
probability `p`, the exact occupancy distribution follows
$P_{t+1}(k)=P_t(k)(1-p+p k/m)+P_t(k-1)p(m-k+1)/m$. The queue-full
probability at depth `d` is $P_{\mathrm{full}}(d)=\Pr[D_T>d]$, so the depth for
a target tail probability is
$d_\epsilon=\min\{d:P_{\mathrm{full}}(d)\le\epsilon\}$.

Sanity check: this is the right abstraction for queue-slot sizing because
coalescing makes queue occupancy depend on distinct tags, not raw hit count.
For `T=256`, `p=1`, and `m=256`, the worst-case no-drop depth is 256, but the
1 ppm iid tail depth is 185. The former 160-entry queue is therefore not a
1 ppm-safe all-channel iid stall depth, although it covers localized 8-channel
physical hits. The Phase-5 build uses `COAL_QUEUE_DEPTH=256`, which covers the
adversarial all-channel injection case with no queue-full loss.

Key analytical rows at `T=256`, `p=1`:

| active channels | no-drop depth | depth at <= 1 ppm queue-full probability |
|---:|---:|---:|
| 1 | 1 | 1 |
| 8 | 8 | 8 |
| 32 | 32 | 32 |
| 128 | 128 | 124 |
| 256 | 256 | 185 |

## Set 2: Loss Fraction at the 8-MuTRiG Link Cap

![Queue loss versus depth](artifacts/queue_loss_vs_depth_8mutrig.png)

Data:
[`artifacts/queue_loss_vs_depth_8mutrig.csv`](artifacts/queue_loss_vs_depth_8mutrig.csv)

This plot uses the physical link cap requested for 8 MuTRiG inputs:
`8 * 25 Mhit/s = 200 Mhit/s` aggregate, 256 histogram channels, and a saturated
256-cycle no-drain window. The y-axis is lost offered-hit fraction, not just
the probability that the queue ever became full.

The iid curve is computed from the same distinct-tag recurrence while
accumulating expected lost hits. In compact form,
$L_{\mathrm{iid}}(d)=\mathbb{E}[\mathrm{lost\ hits\ during\ }T\mathrm{\ cycles}]/(Tp)$.
The deterministic traffic bounds are $L_{\mathrm{phys}}(d)=\max(0,(8-d)/8)$ for
an 8-channel physical cluster and $L_{\mathrm{inj}}(d)=\max(0,(256-d)/256)$ for
all-channel injection.

Sanity check: the iid queue-full probability and the iid lost-hit fraction are
different metrics. At depth 160, the all-256 iid queue-full probability during
the stall is about 61.9%, but the expected lost-hit fraction is about 1.27%,
because many later hits land on tags already in the coalescing queue. That
makes sense and should not be read as a contradiction. In contrast, all-channel
injection is adversarial and still loses 37.5% at depth 160, but loses 0 at the
Phase-5 depth of 256.

Selected values:

| queue depth | iid 256-channel | physical 8-channel hit | injection all-channel |
|---:|---:|---:|---:|
| 8 | 0.93805440291 | 0 | 0.96875 |
| 160 | 0.0127214992353 | 0 | 0.375 |
| 185 | 0.00000000573193 | 0 | 0.27734375 |
| 256 | 0 | 0 | 0 |

## Set 3: TLM and RTL Cycle Agreement

![TLM RTL trace agreement](artifacts/queue_trace_tlm_rtl_match.png)

Artifacts:

- [`artifacts/queue_trace_stimulus.csv`](artifacts/queue_trace_stimulus.csv)
- [`artifacts/queue_trace_expected_tlm.csv`](artifacts/queue_trace_expected_tlm.csv)
- [`artifacts/queue_trace_observed_rtl.csv`](artifacts/queue_trace_observed_rtl.csv)
- [`artifacts/queue_trace_compare.csv`](artifacts/queue_trace_compare.csv)

The executable TLM uses the RTL state update order. Queue occupancy evolves as
$q_{t+1}=q_t+I[\mathrm{new\ tag\ accepted}]-I[\mathrm{drain\ fire}]$, while a
repeated hit to a queued tag updates $kick_b(t+1)=\min(kick_b(t)+1,255)$.
Overflow increments when a new tag arrives with no queue slot available, or
when a queued tag is hit after its kick counter is already saturated.

The comparison metric is
$N_{\mathrm{mismatch}}=\sum_t I[y_{\mathrm{TLM}}(t)\ne y_{\mathrm{RTL}}(t)]$.

Result:

```text
QuestaSim-64 2026.1_1
vcom: Errors 0, Warnings 0
vsim: Errors 0, Warnings 0
TLM/RTL comparison PASS: 1190 cycles, 0 mismatches
```

Sanity check: this trace proves that the current TLM matches the RTL for the
targeted coalescing, queue-full, kick-saturation, and drain sequences in this
gate. It is not a full randomized DV closure claim.

## Set 4: Bunched and iid-Poisson Coalescer Sweeps

![Bunched coalescer rate sweep](artifacts/bunched_coalescing_rate_sweep.png)

Data:
[`artifacts/bunched_coalescing_rate_sweep.csv`](artifacts/bunched_coalescing_rate_sweep.csv)

![Bunched coalescer kick sweep](artifacts/bunched_coalescing_kick_sweep.png)

![iid-Poisson coalescer rate sweep](artifacts/iid_poisson_coalescing_rate_sweep.png)

Data:
[`artifacts/iid_poisson_coalescing_rate_sweep.csv`](artifacts/iid_poisson_coalescing_rate_sweep.csv)

![iid-Poisson coalescer kick sweep](artifacts/iid_poisson_coalescing_kick_sweep.png)

These sweeps compare two TLM coalescers:

- `per_bin_counter`: one fixed 8-bit saturating pending counter per bin;
- `tag_addressable_cb`: a circular buffer of `(bin_tag, kick_count)` cells,
  with tag-addressable coalescing against any live cell.

The default run uses `N_BINS=256`, `125 MHz`, `2` pushed bin hits/cycle into the
hist IP, and an update engine service rate of `0.5` update cell/cycle. The
x-axis is aggregate offered source rate, `pulse_rate_hz * N_BINS`, in million
bin-updates/s. The occupancy y-axis is pending update cells sampled over
steady-state cycles after warm-up. The kick-count y-axis is the live-cell
`kick_count` over nonzero pending cells. The cyan band is the per-bin
`min..max`; the red dashed bounds are the CB `min..max`; solid lines are p50
for each model.

The bunched source fires all 256 bins once per pulse and serializes that pulse
into the hist IP at `2` hits/cycle. The iid-Poisson source models 256
independent Poisson bin sources. Their arrivals first enter an infinite source
FIFO; that FIFO is then consumed by the hist IP at up to `2` hits/cycle, while
the update engine still drains coalesced update cells at `0.5` cell/cycle. The
iid CSV includes `source_arrivals` and source-FIFO min/p50/max/mean columns.

Selected rows:

| traffic | per-bin pulse rate | aggregate rate | model | cells min/p50/max | kick min/p50/max | source FIFO p50/max |
|---|---:|---:|---|---:|---:|---:|
| bunched | 100.0 kHz | 25.6 M/s | per-bin | 0 / 0 / 192 | 1 / 1 / 1 | - |
| bunched | 100.0 kHz | 25.6 M/s | CB | 0 / 0 / 192 | 1 / 1 / 1 | - |
| bunched | 244.1 kHz | 62.5 M/s | per-bin | 0 / 96 / 192 | 1 / 1 / 1 | - |
| bunched | 244.1 kHz | 62.5 M/s | CB | 0 / 96 / 192 | 1 / 1 / 1 | - |
| bunched | 950.0 kHz | 243.2 M/s | per-bin | 190 / 223 / 256 | 1 / 2 / 4 | - |
| bunched | 950.0 kHz | 243.2 M/s | CB | 190 / 223 / 256 | 1 / 2 / 4 | - |
| iid-Poisson | 100.0 kHz | 25.6 M/s | per-bin | 0 / 0 / 5 | 1 / 1 / 2 | 0 / 2 |
| iid-Poisson | 100.0 kHz | 25.6 M/s | CB | 0 / 0 / 5 | 1 / 1 / 2 | 0 / 2 |
| iid-Poisson | 244.1 kHz | 62.5 M/s | per-bin | 0 / 8 / 34 | 1 / 1 / 3 | 0 / 4 |
| iid-Poisson | 244.1 kHz | 62.5 M/s | CB | 0 / 8 / 34 | 1 / 1 / 4 | 0 / 4 |
| iid-Poisson | 950.0 kHz | 243.2 M/s | per-bin | 174 / 190 / 209 | 1 / 2 / 14 | 10 / 81 |
| iid-Poisson | 950.0 kHz | 243.2 M/s | CB | 170 / 190 / 213 | 1 / 2 / 12 | 10 / 81 |

Sanity check: at `100 kHz/bin`, the bunched offered aggregate rate is below the
`62.5 M cell/s` update-engine transaction capacity. Because this TLM lets push
and drain proceed in the same cycle, the peak pending-cell count is `192`, not
`256`; holding the update engine off until the full bunch is accepted would
produce a single-burst peak of `256`. The iid-Poisson case is much lower at
the same average rate because arrivals are spread in time before source-FIFO
serialization.

The finite coalescer curves do not exceed 256 pending cells because both
coalescers have a 256-bin tag universe and this sweep sets `cb_depth=256`.
That means the finite coalescers have no queue-full overflow in these sampled
cases, but it does not prove an unbounded transaction queue would remain small
if every update were modeled as a separate cell.

![Infinite bunched queue reference](artifacts/bunched_infinite_queue_rate_sweep.png)

Data:
[`artifacts/bunched_infinite_queue_rate_sweep.csv`](artifacts/bunched_infinite_queue_rate_sweep.csv)

The capacity-infinite reference is labeled `D/D/1 infinite K` in the CSV. It
uses deterministic all-bin bunch arrivals and deterministic hardware service,
not an exponential-service server, because the update engine cadence is fixed
in the hardware model. It is the check that removes finite-depth clipping from
the earlier queue-length question.

Selected infinite-reference rows:

| per-bin pulse rate | aggregate rate | min/p50/max queued cells | p50 latency | max latency |
|---:|---:|---:|---:|---:|
| 100.0 kHz | 25.6 M/s | 0 / 0 / 192 | 193 cycles / 1.544 us | 384 cycles / 3.072 us |
| 244.1 kHz | 62.5 M/s | 0 / 96 / 192 | 193 cycles / 1.544 us | 384 cycles / 3.072 us |
| 950.0 kHz | 243.2 M/s | 6088 / 18262 / 30436 | 9388 cycles / 75.104 us | 15642 cycles / 125.136 us |

## Figure Quality Gate

DISLIN renderers:
[`scripts/render_queue_plots.sh`](scripts/render_queue_plots.sh) and
[`scripts/render_bunched_coalescing.sh`](scripts/render_bunched_coalescing.sh)

All presentation PNGs are rendered at `1800 x 1112`, giving an aspect ratio of
`1.619`, within rounding of the golden ratio. SVG versions are generated from
the same renderer:

- [`artifacts/queue_depth_quantiles_8mutrig.svg`](artifacts/queue_depth_quantiles_8mutrig.svg)
- [`artifacts/queue_loss_vs_depth_8mutrig.svg`](artifacts/queue_loss_vs_depth_8mutrig.svg)
- [`artifacts/queue_trace_tlm_rtl_match.svg`](artifacts/queue_trace_tlm_rtl_match.svg)
- [`artifacts/bunched_coalescing_rate_sweep.svg`](artifacts/bunched_coalescing_rate_sweep.svg)
- [`artifacts/bunched_coalescing_kick_sweep.svg`](artifacts/bunched_coalescing_kick_sweep.svg)
- [`artifacts/iid_poisson_coalescing_rate_sweep.svg`](artifacts/iid_poisson_coalescing_rate_sweep.svg)
- [`artifacts/iid_poisson_coalescing_kick_sweep.svg`](artifacts/iid_poisson_coalescing_kick_sweep.svg)
- [`artifacts/bunched_infinite_queue_rate_sweep.svg`](artifacts/bunched_infinite_queue_rate_sweep.svg)

DISLIN 11.5.2 reported zero warnings for PNG and SVG renders. The final visual
pass checked that axes are readable, legends do not cover data, line styles are
visible when TLM and RTL traces overlap, and title/axis text is not clipped.
