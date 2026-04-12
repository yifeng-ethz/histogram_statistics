# Synthesis Signoff: histogram_statistics_v2

Status: `PASS WITH THIN TIMING MARGIN`

This signoff is based on a fresh standalone Quartus compile of the current RTL
tree in [syn/quartus](syn/quartus). The compile was rerun after the
`coalescing_queue` pointer-wrap fix. The shipped Platform Designer `Default`
preset now trims `COAL_QUEUE_DEPTH` to `160`, but this standalone compile still
uses the raw RTL default build shape with a `256 x 8` coalescing queue, so the
timing/resource result below is conservative relative to the packaged preset.

## Build Context

- Project: `histogram_statistics_v2_signoff`
- Revision: `histogram_statistics_v2_standalone`
- Top-level entity: `histogram_statistics_v2`
- Device family: Arria V
- Device: `5AGXBA7D4F31C5`
- Quartus version: `18.1.0 Build 625 09/12/2018 SJ Standard Edition`
- Flow report timestamp: `Sun Apr 12 11:13:37 2026`
- Host: `teferi.ethz.ch`
- Constraint file:
  [syn/quartus/histogram_statistics_v2_standalone.sdc](syn/quartus/histogram_statistics_v2_standalone.sdc)
- Clock target:
  - functional target: `125 MHz`
  - signoff target with 10% margin: `137.5 MHz`
  - constrained period: `7.273 ns`

## Verification Context

- Current standalone verification status is summarized in
  [VERIFICATION_SIGNOFF.md](VERIFICATION_SIGNOFF.md).
- The current verification evidence closes the project DV plan, but exact
  simulator-tier workflow closure is still blocked by the `questa_fe` runtime
  checkout issue on this host.
- That simulator-tier issue does not change the Quartus timing/utilization
  evidence summarized here.

## Flow Runtime

From
[syn/quartus/output_files/histogram_statistics_v2_standalone.flow.rpt](syn/quartus/output_files/histogram_statistics_v2_standalone.flow.rpt):

| Stage | Elapsed | CPU Time |
|-------|--------:|---------:|
| Analysis & Synthesis | `00:00:41` | `00:00:59` |
| Fitter | `00:03:25` | `00:07:41` |
| Assembler | `00:00:13` | `00:00:13` |
| Timing Analyzer | `00:00:16` | `00:00:30` |
| Total | `00:04:35` | `00:09:23` |

## Timing Closure

From
[syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary](syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary)
and
[syn/quartus/output_files/histogram_statistics_v2_standalone.sta.rpt](syn/quartus/output_files/histogram_statistics_v2_standalone.sta.rpt):

| Corner | Fmax | Setup Slack | Hold Slack | Min Pulse Width Slack |
|--------|-----:|------------:|-----------:|----------------------:|
| Slow 1100mV 85C | `139.12 MHz` | `0.085 ns` | `0.265 ns` | `2.704 ns` |
| Slow 1100mV 0C | `141.9 MHz` | `0.226 ns` | `0.250 ns` | `2.670 ns` |
| Fast 1100mV 85C | n/a in Fmax table | `2.742 ns` | `0.158 ns` | `2.849 ns` |
| Fast 1100mV 0C | n/a in Fmax table | `3.202 ns` | `0.145 ns` | `2.840 ns` |

Worst signoff corner:

- clock: `i_clk`
- worst-case setup slack: `0.085 ns`
- worst-case hold slack: `0.145 ns`
- worst-case minimum pulse-width slack: `2.670 ns`
- worst-case Fmax: `139.12 MHz`

Margin assessment:

- required signoff frequency: `137.49 MHz`
- achieved worst-case Fmax: `139.12 MHz`
- frequency headroom: `1.63 MHz`
- relative headroom: `1.19%`

This is a real pass, but not a loose one. The design meets the tightened
signoff target with only `0.085 ns` setup slack at the slow `85C` corner.

## Timing Hotspot

The timing analyzer again identifies the limiting arcs inside the inferred
coalescing-queue memories:

- `coalescing_queue:queue_inst|altdpram:queue_mem_rtl_0|...|lutrama*~CLKMUX_0 -> ...|MEMORYREGOUT`
- `coalescing_queue:queue_inst|altdpram:queue_mem_rtl_1|...|lutrama*~CLKMUX_0 -> ...|MEMORYREGOUT`

That matches the utilization picture below: `coalescing_queue` is still the
dominant logic block and remains the most likely place to lose timing margin
first.

## Top-Level Resource Summary

From
[syn/quartus/output_files/histogram_statistics_v2_standalone.fit.summary](syn/quartus/output_files/histogram_statistics_v2_standalone.fit.summary)
and
[syn/quartus/output_files/histogram_statistics_v2_standalone.fit.rpt](syn/quartus/output_files/histogram_statistics_v2_standalone.fit.rpt):

| Resource | Usage | Device Utilization |
|----------|-------|--------------------|
| ALMs | `10,354 / 91,680` | `11%` |
| Dedicated logic registers | `4,572 / 183,360` | `2%` |
| Total LABs used | `1,222 / 9,168` | `13%` |
| M10K blocks | `2 / 1,366` | `<1%` |
| Total block memory bits | `16,384 / 13,987,840` | `<1%` |
| Total MLAB memory bits | `8,192` | n/a |
| DSP blocks | `0 / 800` | `0%` |
| Peak interconnect usage | `35.0% / 33.7% / 39.3%` | total / H / V |
| Maximum fan-out | `4958` | n/a |

## Block-Level Utilization

The table below combines the hierarchy fitter view with the mapped RAM
inventory. `hit_fifo x8` is aggregated across all eight identical instances.

| Block | Synth ALUTs | Fit ALMs Needed | Regs | ALM Share | Register Share | Dedicated RAM | Notes |
|-------|------------:|----------------:|-----:|----------:|---------------:|---------------|-------|
| `coalescing_queue` | `8210` | `5256.9` | `2397` | `50.77%` | `52.43%` | `2 x 2048-bit MLAB` | Dominant logic block and current timing limiter |
| `hit_fifo x8` | `344` | `360.1` | `192` | `3.48%` | `4.20%` | `8 x 512-bit MLAB` | Small per instance, linear in port count |
| `bin_divider` | `510` | `316.0` | `503` | `3.05%` | `11.00%` | none | Register-heavy arithmetic pipeline |
| `pingpong_sram` | `461` | `274.5` | `363` | `2.65%` | `7.94%` | `2 x 8192-bit M10K` | Owns all block RAM in the design |
| `rr_arbiter` | `153` | `114.7` | `39` | `1.11%` | `0.85%` | none | Small and not timing-dominant |

Top-level reference totals:

- synthesized combinational ALUTs: `14,949`
- fitted ALMs needed: `10,354`
- fitted registers: `4,572`
- block RAM bits: `16,384`
- M10Ks: `2`

## RAM Inventory

From
[syn/quartus/output_files/histogram_statistics_v2_standalone.fit.rpt](syn/quartus/output_files/histogram_statistics_v2_standalone.fit.rpt):

| Owner | Resource | Type | Geometry | Bits |
|-------|----------|------|----------|-----:|
| `coalescing_queue` | `queue_mem_rtl_0` | MLAB | `256 x 8`, dual-port | `2048` |
| `coalescing_queue` | `queue_mem_rtl_1` | MLAB | `256 x 8`, dual-port | `2048` |
| `hit_fifo x8` | `mem_rtl_0` | MLAB | `16 x 32`, dual-port | `8 x 512` |
| `pingpong_sram.bank_a` | `ram_rtl_0` | M10K | `256 x 32`, dual-port | `8192` |
| `pingpong_sram.bank_b` | `ram_rtl_0` | M10K | `256 x 32`, dual-port | `8192` |

Memory interpretation:

- all M10K usage belongs to `pingpong_sram`
- all MLAB usage belongs to `coalescing_queue` and the replicated `hit_fifo`
  banks
- there is no DSP usage anywhere in the standalone build
- the standalone build is still using the conservative raw `256`-deep queue
  shape, not the packaged `160`-deep preset

## Synthesis Interpretation by Block

`coalescing_queue`

- This is the dominant cost center in both logic and registers.
- It also owns half of the total MLAB memory bits.
- The current timing endpoints still land in its inferred queue memories.

`pingpong_sram`

- This block is modest in ALM cost but owns both M10Ks.
- Current memory footprint is efficient for the `256`-bin, `32`-bit dual-bank
  organization.
- Timing is not primarily limited here in the current compile.

`bin_divider`

- Register count is high relative to ALM count, which is expected for the
  pipelined divider.
- It is not the dominant timing bottleneck in this compile, but it remains the
  second most register-heavy direct child after `coalescing_queue`.

`hit_fifo x8`

- Each FIFO is small, but replication makes the aggregate cost noticeable.
- The total replicated MLAB footprint is `4096` bits.
- This block is not the main timing risk, but parameter growth in port count
  scales it linearly.

`rr_arbiter`

- Resource cost is negligible relative to the rest of the design.
- This block is not a closure concern in the current snapshot.

## Signoff Conclusion

Synthesis signoff passes for the current standalone RTL configuration:

- the design meets the `137.5 MHz` tightened signoff target on `i_clk`
- worst-case setup slack is positive at `0.085 ns`
- resource usage remains comfortably inside the Arria V device envelope:
  - `11%` ALMs
  - `2%` registers
  - `2` M10Ks
  - `0` DSPs

The main caveat is margin quality, not closure correctness:

- timing margin is thin at the slow `85C` corner
- `coalescing_queue` is both the dominant area block and the likely timing
  limiter
- future growth in queue complexity, bin count, or data-path fan-out should be
  re-signed off rather than assumed safe from this compile alone
