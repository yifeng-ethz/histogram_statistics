# histogram_statistics_v2 DV -- Performance and Stress Cases

**Parent:** DV_PLAN.md
**ID Range:** P001-P148
**Total:** 148 cases
**Method:** R (constrained-random), S (sweep), K (soak)

These tests characterize throughput limits, stress conditions, and long-running behaviors of the histogram_statistics_v2 IP core. Each test maps to a specific system-level performance question about the multi-port ingress path, coalescing queue, ping-pong SRAM, and readout pipeline. Every case explains what throughput limit it probes or what long-running behavior it validates.

**General methodology:** Each test runs a parameterised sequence with LCG-seeded stimulus (Questa FSE: no `rand`/`constraint`). Cycle-accurate throughput, drop rates, and bin counts are measured by the scoreboard and counter-based coverage. Results are compared against analytical bounds derived from pipeline structure.

**Key architectural constants:**
- N_BINS=256, N_PORTS=8, FIFO depth=16 (2^4), COAL_QUEUE_DEPTH=256, KICK_WIDTH=8 (max coalesced=255)
- Pipeline latency: ingress(1) + arb_pipe(1) + key_pipe(1) + divider_in(1) + bin_divider(8) + queue_hit_pipe(1) = 13 cycles from FIFO read to queue entry
- Arbiter: round-robin, 1 grant/cycle, skips port that was just output (back-to-back same-port suppression)
- Pingpong update pipeline: 4 stages (issue/read/add+sum/write) with RAW forwarding
- Bank clear: 256 cycles (N_BINS), upd_ready=0 during clear

---

## 1. Single-Port Throughput (SPT) -- 12 cases

Sweep injection rate on a single port (port 0) to characterize the maximum sustainable ingress rate before the FIFO-depth=16 buffer overflows and drops begin. The arbiter sees only one non-empty FIFO so it grants every cycle, making this a pure FIFO-depth test.

**Why this section exists:** The per-port FIFO is only 16 deep. If ingress delivers a burst longer than 16 keys before the arbiter drains one, drops occur. This section quantifies exactly where that threshold lies for a single port, which is the baseline for all multi-port analysis.

**Output per test:** `{injected_hits, accepted_hits, dropped_hits, fifo_level_max, bin_accuracy_pct}`.

| ID | Method | Scenario | Duration/Iter | Stimulus | Checker |
|----|--------|----------|---------------|----------|---------|
| P001 | S | 1% injection rate: 1 hit per 100 cycles | 10k hits | Port 0 only, uniform bin dist | Zero drops; fifo_level_max <= 2; all bins correct |
| P002 | S | 10% injection rate: 1 hit per 10 cycles | 10k hits | Port 0, uniform bins | Zero drops; fifo_level_max < 4 |
| P003 | S | 25% injection rate: 1 hit per 4 cycles | 10k hits | Port 0, uniform bins | Zero drops; fifo_level_max < 6 |
| P004 | S | 50% injection rate: 1 hit per 2 cycles | 10k hits | Port 0, uniform bins | Zero drops; fifo_level_max < 10 |
| P005 | S | 75% injection rate: 3 hits per 4 cycles | 10k hits | Port 0, uniform bins | Zero drops; arbiter drains at 1/cycle so FIFO stays bounded |
| P006 | S | 100% injection rate: back-to-back hits | 10k hits | Port 0, uniform bins | Possible drops once FIFO fills; measure drop onset latency |
| P007 | S | Burst-16: 16 back-to-back then 16-cycle gap | 5k bursts | Port 0, uniform bins | Zero drops: burst exactly matches FIFO depth |
| P008 | S | Burst-17: 17 back-to-back then 15-cycle gap | 5k bursts | Port 0, uniform bins | Exactly 1 drop per burst (FIFO overflow on 17th); verify drop counter |
| P009 | S | Burst-32: 32 back-to-back then 32-cycle gap | 2k bursts | Port 0, uniform bins | 16 drops per burst; verify FIFO stays full for 16 cycles |
| P010 | R | Random injection rate 50-100% with LCG gaps | 20k hits | Port 0, LCG bin dist | Drop rate matches analytical model; bins correct for accepted hits |
| P011 | S | Ramp 0%->100%->0% over 20k cycles | 20k cycles | Port 0, linear ramp | No drops during ramp-up phase until FIFO fills; recovery after ramp-down |
| P012 | S | Square-wave: 100% for 32 cycles, 0% for 32 cycles | 5k waves | Port 0, uniform bins | Drops only during high phase when burst > 16; FIFO drains completely during low phase |

---

## 2. Multi-Port Throughput (MPT) -- 12 cases

All 8 ports active with varying per-port rates. The round-robin arbiter services at most 1 port per cycle, so aggregate throughput is bounded at 1 hit/cycle. With 8 ports each at >12.5% rate, contention causes FIFO buildup. This section quantifies arbiter fairness and aggregate drop rate.

**Why this section exists:** In the real system all 8 SciFi FEB links deliver hits simultaneously. The arbiter must fairly share bandwidth without starving any port. These tests verify that the round-robin policy distributes service evenly and that per-port FIFO depth is sufficient for the expected per-link hit rates.

**Output per test:** `{per_port_accepted[8], per_port_dropped[8], per_port_fifo_max[8], aggregate_throughput}`.

| ID | Method | Scenario | Duration/Iter | Stimulus | Checker |
|----|--------|----------|---------------|----------|---------|
| P013 | S | All 8 ports at 10% (aggregate 80%) | 10k hits/port | Uniform bins per port | Zero drops on all ports; throughput ~0.8 hits/cycle |
| P014 | S | All 8 ports at 12.5% (aggregate 100%) | 10k hits/port | Uniform bins | Zero drops; arbiter saturated but FIFOs never exceed 1 |
| P015 | S | All 8 ports at 15% (aggregate 120%) | 10k hits/port | Uniform bins | Drops begin; per-port drop rate roughly equal (fairness) |
| P016 | S | All 8 ports at 25% (aggregate 200%) | 5k hits/port | Uniform bins | Significant drops; per-port drop rate within +/-10% of mean |
| P017 | S | All 8 ports at 50% (aggregate 400%) | 2k hits/port | Uniform bins | Heavy drops; verify no port is starved (min accepted > 0) |
| P018 | S | All 8 ports at 100% (aggregate 800%) | 2k hits/port | Uniform bins | Maximum contention; verify arbiter round-robin fairness: max-min accepted < 5% |
| P019 | R | Random per-port rates (5-50% each, LCG) | 10k hits total | LCG rates, LCG bins | Aggregate drop rate matches sum of per-port analytical models |
| P020 | S | Staggered start: port i starts at cycle i*100 | 10k hits/port | All ports at 25% | Early ports get more service initially; all converge to equal after startup |
| P021 | S | Port 0 at 100%, ports 1-7 at 5% each | 10k hits/port 0 | Mixed rates | Port 0 gets ~1/8 bandwidth like others due to round-robin; measure fairness |
| P022 | R | Burst-synchronized: all 8 ports burst 16 hits simultaneously | 1k bursts | Synchronized bursts, uniform bins | Only 1 port drained per cycle; 7 ports overflow; measure total drops |
| P023 | S | Alternating pairs: ports 0-3 active for 1000 cycles, then ports 4-7 | 5k alternations | 25% per active port | Zero drops during each phase; clean handoff measured |
| P024 | R | Per-port random burst length (1-16) with LCG timing | 20k hits total | LCG burst lengths, LCG gaps | Drop rate correlates with burst length; no port deadlock |

---

## 3. Coalescing Efficiency (COE) -- 14 cases

The coalescing queue (depth=256, kick_width=8) merges repeated hits to the same bin into a single (bin, count) update. Coalescing efficiency determines how much downstream bandwidth is saved. When all 256 bins are uniformly hit, the queue quickly fills (each bin gets a separate entry). When hits cluster on few bins, the queue stays shallow but kick counters can saturate at 255.

**Why this section exists:** The coalescing queue is the critical path between the bin_divider (1 hit/cycle input) and the pingpong_sram (1 update per 4 cycles throughput). Without coalescing, the SRAM update pipeline would bottleneck at 0.25 hits/cycle. Coalescing collapses N hits to the same bin into 1 update, so the effective throughput depends entirely on how well the workload coalesces. These tests map the coalescing efficiency curve.

**Output per test:** `{queue_occupancy_max, queue_overflow_count, drain_count_avg, drain_count_max, coalescing_ratio}`.

| ID | Method | Scenario | Duration/Iter | Stimulus | Checker |
|----|--------|----------|---------------|----------|---------|
| P025 | S | Uniform distribution: all 256 bins equally likely | 10k hits | Port 0, 50% rate | Queue fills to ~256 after 256 distinct bins seen; coalescing_ratio ~ N_hits/256 |
| P026 | S | Single-bin: all hits to bin 0 | 10k hits | Port 0, 50% rate | Queue occupancy stays at 1; kick counter saturates at 255 every 255 hits; overflow_count = floor(10000/255)-1 |
| P027 | S | Two-bin: alternating bin 0 and bin 1 | 10k hits | Port 0, 50% rate | Queue occupancy stays at 2; excellent coalescing |
| P028 | S | 4-bin cluster: bins 0-3 uniformly | 10k hits | Port 0, 50% rate | Queue occupancy 4; drain_count_avg ~ 2500/drain_events |
| P029 | S | 16-bin cluster | 10k hits | Port 0, 50% rate | Queue occupancy 16; good coalescing |
| P030 | S | 64-bin uniform | 10k hits | Port 0, 50% rate | Queue occupancy 64; moderate coalescing |
| P031 | S | 128-bin uniform | 10k hits | Port 0, 50% rate | Queue occupancy 128; coalescing still helps |
| P032 | S | 256-bin uniform (worst case for queue) | 10k hits | Port 0, 50% rate | Queue at max capacity; drain_count_avg ~1; minimal coalescing |
| P033 | R | Zipf distribution: bin 0 gets 50% of hits, rest distributed | 10k hits | Port 0, LCG | Queue occupancy < 256; bin 0 dominates; kick counter for bin 0 saturates repeatedly |
| P034 | S | Kick saturation stress: 256 consecutive hits to same bin | 512 hits x 4 | Port 0, 100% rate | Kick counter saturates at 255; overflow_count increments on 256th hit; verify no data corruption |
| P035 | R | Random bin distribution with 8 ports active | 20k hits total | All ports, LCG bins | Queue occupancy higher due to port offset (key = raw + port*32); measure effective coalescing |
| P036 | S | Sequential scan: bins 0,1,2,...,255,0,1,... | 10k hits | Port 0, 100% rate | Worst-case queue fill: each bin enqueued before first drain; queue full after 256 entries |
| P037 | S | Reverse scan: bins 255,254,...,0,255,... | 10k hits | Port 0, 100% rate | Same queue behavior as P036; verify no ordering-dependent bugs |
| P038 | R | Bimodal: 50% hits to bin 0, 50% to bin 255 | 10k hits | Port 0, LCG timing | Queue occupancy 2; both bins coalesce well; verify both bin counts correct |

---

## 4. Ping-Pong Stress (PPS) -- 14 cases

The ping-pong SRAM swaps active/frozen bank on each interval timer expiry. During swap, the new active bank is cleared (256 cycles, upd_ready=0), blocking the coalescing queue drain. With short intervals, the clear phase consumes a large fraction of the interval, reducing effective update bandwidth. With continuous hits during swap, the coalescing queue must buffer all pending updates.

**Why this section exists:** In production, the interval is 125M cycles (1 second at 125 MHz), so the 256-cycle clear is negligible (0.0002%). But during testing or with short measurement windows, the clear fraction is large. These tests verify that (a) no updates are lost during the clear phase, (b) the frozen bank contains exactly the correct counts from the previous interval, (c) the timer fires at the correct cycle count, and (d) back-to-back intervals do not corrupt state.

**Output per test:** `{interval_count, clears_observed, hits_during_clear, bin_sum_per_interval, expected_sum_per_interval}`.

| ID | Method | Scenario | Duration/Iter | Stimulus | Checker |
|----|--------|----------|---------------|----------|---------|
| P039 | S | Interval=100 cycles, continuous hits at 50% | 50 intervals | Port 0, uniform bins | ~50 hits/interval minus ~128 lost to clear phase; frozen bank correct per interval |
| P040 | S | Interval=256 cycles (=clear time) | 50 intervals | Port 0, 50% rate | Clear phase consumes entire interval; upd_ready=0 for 100% of time; zero updates reach SRAM |
| P041 | S | Interval=257 cycles (1 cycle of update window) | 50 intervals | Port 0, 50% rate | Only 1 cycle of effective update; at most 1 bin updated per interval |
| P042 | S | Interval=512 cycles (50% effective) | 50 intervals | Port 0, 50% rate | ~128 effective cycles; queue must buffer 128 pending updates during clear |
| P043 | S | Interval=1000 cycles | 50 intervals | Port 0, 50% rate | ~744 effective cycles; verify sum of frozen bins matches accepted hits per interval |
| P044 | S | Interval=10000 cycles | 20 intervals | Port 0, 50% rate | Clear fraction ~2.5%; near-full bandwidth; verify bin accuracy |
| P045 | S | Interval=100, all 8 ports at 12.5% | 50 intervals | All ports, uniform bins | Aggregate rate = 100%; massive queue pressure during clear; measure overflow |
| P046 | S | Interval=500, hits during bank swap boundary | 100 intervals | Port 0, 100% for 10 cycles centered on swap | Hits arrive exactly as clear_active asserts; verify none are lost and none corrupt new bank |
| P047 | R | Random interval length (100-10000, LCG) per run | 20 intervals | Port 0, 50% rate | Variable interval; frozen bank always matches its interval's hits |
| P048 | S | Back-to-back interval pulses: interval=256 (clear fills entire interval) | 100 intervals | Port 0, 25% rate | upd_ready never asserts; queue drains zero; all hits remain in queue; no hang |
| P049 | S | Read frozen bank during active writes | 20 intervals | Port 0, 50% rate; burst read every interval | Burst read returns frozen bank data while active bank is being updated; no port-A/port-B conflict |
| P050 | S | Interval=100, Zipf bin distribution | 50 intervals | Port 0, 50% | Coalescing helps during clear phase; fewer distinct queue entries than uniform; lower overflow |
| P051 | K | 10k intervals at interval=1000 cycles | 10M cycles | Port 0, 25% rate | Long soak of interval cycling; no timer drift; interval_pulse count == expected |
| P052 | S | Interval change mid-run: start at 10000, switch to 500 via CSR | 100 intervals | Port 0, 50% rate | Timer reloads with new value after CSR apply; no partial-interval corruption |

---

## 5. Backpressure Characterization (BPC) -- 12 cases

Each ingress port has a 16-deep FIFO. When the FIFO is full, new hits are dropped (drop_pulse asserts). The FIFO drains at 1 entry/cycle (arbiter grant) but fills at up to 1 entry/cycle (port ingress). With multiple ports, the drain rate per port is 1/N_active_ports. This section maps the exact FIFO fill dynamics and drop thresholds.

**Why this section exists:** The system must quantify the maximum sustained per-port injection rate that avoids drops, which depends on the number of active ports. With 1 active port, the FIFO drains at 1/cycle so 100% injection rate is sustainable (net fill rate = 0). With 8 active ports, each port gets 1/8 drain rate, so maximum sustainable rate is 12.5%. These tests validate this analytical model and expose any off-by-one errors in FIFO full detection.

**Output per test:** `{per_port_fifo_max[8], per_port_drop_count[8], cycles_until_first_drop, steady_state_drop_rate}`.

| ID | Method | Scenario | Duration/Iter | Stimulus | Checker |
|----|--------|----------|---------------|----------|---------|
| P053 | S | 1 port at 100%: net drain rate = 0 (fill=1, drain=1) | 10k hits | Port 0 only, back-to-back | Zero drops; fifo_level oscillates near 1; steady state achieved |
| P054 | S | 2 ports at 50% each (aggregate 100%) | 10k hits/port | Ports 0-1, alternating | Zero drops; each port drained every 2 cycles on average |
| P055 | S | 4 ports at 25% each (aggregate 100%) | 10k hits/port | Ports 0-3 | Zero drops; each port drained every 4 cycles on average |
| P056 | S | 8 ports at 12.5% each (aggregate 100%) | 10k hits/port | All ports | Zero drops; each port drained every 8 cycles on average |
| P057 | S | 8 ports at 13% each (aggregate 104%) | 10k hits/port | All ports | Drops begin slowly; measure cycles until first drop; verify drop rate ~4% |
| P058 | S | 8 ports at 15% each (aggregate 120%) | 5k hits/port | All ports | Steady-state drops; fifo_level_max approaches 16; drop rate ~20% |
| P059 | S | 8 ports at 25% each (aggregate 200%) | 2k hits/port | All ports | Heavy drops; FIFO permanently full; drop rate ~50% |
| P060 | S | FIFO full → empty transition: burst 16 then stop | 100 bursts | Port 0 only | FIFO hits full (level=16), then drains to 0 in exactly 16 cycles; no off-by-one |
| P061 | S | FIFO write during full: verify drop counter accuracy | 100 bursts of 20 | Port 0 only | Exactly 4 drops per burst (20-16); csr_dropped_hits == 400 total |
| P062 | R | Random multi-port injection with LCG timing | 50k cycles | All ports, LCG rates 5-30% | Per-port drop count matches analytical model within 5% |
| P063 | S | Arbiter stall: sink_ready hardwired to 1, but queue drain_ready goes low | 10k hits | Port 0, 50% rate | FIFO fills because arbiter output is always consumed but queue back-pressures; verify arbiter itself never stalls (sink_ready=1 always) |
| P064 | S | Clear during FIFO full: issue measure_clear while all FIFOs full | 10 iterations | All ports at 100% rate | FIFOs reset to empty; drop counters reset; no residual data after clear |

---

## 6. Long Soak (SOK) -- 12 cases

Long-running tests that exercise the full pipeline for extended durations to catch counter overflow, state machine hangs, timer drift, and rare-event bugs that only manifest after thousands of operations.

**Why this section exists:** The histogram core runs continuously for hours in production. Short tests can miss: (a) 32-bit counter saturation in stats registers, (b) timer drift from rounding errors, (c) queue state machine hangs under specific occupancy patterns, (d) SRAM corruption from repeated read-modify-write cycles, and (e) coalescing queue pointer wraparound bugs at the 256-entry boundary.

**Output per test:** Counter snapshots every 10k hits; final `{total_hits, dropped_hits, underflow_count, overflow_count, queue_occupancy, queue_overflow_count, all_bin_sum}`.

| ID | Method | Scenario | Duration/Iter | Stimulus | Checker |
|----|--------|----------|---------------|----------|---------|
| P065 | K | 100k hits, single port, 50% rate, uniform bins | 200k cycles | Port 0, LCG bins | csr_total_hits == 100k; bin_sum == accepted; no hang; fifo_level_max stable |
| P066 | K | 100k hits, all 8 ports at 10%, uniform bins | 125k cycles | All ports, LCG bins | Per-port accepted roughly equal; total_hits == 100k; bin accuracy 100% |
| P067 | K | 500k hits, single port, 25% rate | 2M cycles | Port 0, LCG bins | No counter saturation; no timer drift; all bins correct |
| P068 | K | 100k hits with 100 interval cycles (1000 intervals) | 100k cycles | Port 0, 50%, interval=100 | 1000 bank swaps; no state corruption; each interval's bin sum correct |
| P069 | K | Counter saturation test: drive total_hits to 2^32-1 | Until sat | Port 0, 100% rate | csr_total_hits saturates at 0xFFFFFFFF; no wrap-around; stays at max |
| P070 | K | Queue pointer wraparound: >256 distinct bins cycled through queue | 100k hits | Port 0, sequential scan, 50% rate | Queue rd_ptr and wr_ptr wrap multiple times; no pointer corruption |
| P071 | K | Coalescing queue occupancy cycle: fill to 256, drain to 0, repeat 1000x | 256k hits | Port 0, 100% rate, sequential bins | Each fill/drain cycle produces correct bin counts; occupancy_max == 256 |
| P072 | K | Multi-interval accumulation: 50 intervals, verify each independently | 50 intervals x 2k hits | Port 0, 50% rate, interval=4k | Read frozen bank after each swap; sum == accepted hits in that interval |
| P073 | K | Continuous hits with periodic CSR reads every 1k cycles | 200k cycles | Port 0, 25% rate | CSR reads do not disturb hit processing; no stall or data corruption |
| P074 | K | 100k hits with random clear pulses every 5k-15k cycles | 200k cycles | Port 0, 50% rate, LCG clear timing | Each segment's counters reset correctly; no residual state after clear |
| P075 | K | 8 ports, 100k total hits, Zipf bin dist, 20 intervals | 20 intervals x 5k hits | All ports, interval=25k | Every interval's frozen bank correct; no inter-interval leakage |
| P076 | K | Stress soak: 8 ports at 20%, 50k hits, interval=500 | 500 intervals | All ports, uniform bins | Combined stress of high contention + frequent swaps; no hang; no drift |

---

## 7. Burst Read Performance (BRP) -- 12 cases

The host reads histogram bins via the Avalon-MM hist_bin interface. Burst reads are deferred if the update pipeline is active (non-pingpong mode) or if the pipeline has in-flight updates (pingpong mode). The read path uses port A of the dual-port RAM while the update path uses port B, but the burst read must latch the correct bank and arbitrate RAM port A access with update issue reads.

**Why this section exists:** In production, the host performs burst reads to fetch all 256 bins between interval swaps. If a burst read stalls too long due to update pipeline activity, the host may timeout. If the burst read occurs on the wrong bank, stale data is returned. These tests verify burst read latency, correctness, and interaction with active updates.

**Output per test:** `{burst_latency_cycles, readdata_valid_count, bin_data_correctness, read_during_update_stalls}`.

| ID | Method | Scenario | Duration/Iter | Stimulus | Checker |
|----|--------|----------|---------------|----------|---------|
| P077 | S | Full 256-bin burst read, no active updates | 10 reads | Quiescent DUT | All 256 readdatavalid in 256 consecutive cycles; data matches SRAM contents |
| P078 | S | Full 256-bin burst read during continuous updates | 10 reads | Port 0 at 50% | Burst deferred until pipeline drains; measure deferral latency |
| P079 | S | Single-bin reads (burstcount=1) interleaved with updates | 256 reads | Port 0 at 25% | Each read returns correct bin value; read latency measured per bin |
| P080 | S | Burst-16 reads (16 bins at a time) sweeping all 256 bins | 16 reads | Port 0 at 25% | All 256 bins read correctly in 16 bursts; total latency measured |
| P081 | S | Burst read immediately after bank swap | 20 intervals | Port 0 at 50%, interval=10k | Read frozen bank in first 256 cycles of new interval; verify data from completed interval |
| P082 | S | Burst read during bank clear phase | 10 intervals | Port 0 at 50%, interval=1k | Read targets frozen bank (not being cleared); verify data correct |
| P083 | S | Back-to-back burst reads with no gap | 5 consecutive reads | Quiescent DUT | Second burst starts immediately after first completes; no stall between |
| P084 | S | Burst read while all 8 ports active at 12.5% | 10 reads | All ports active | Read deferred by pipeline activity; measure worst-case deferral |
| P085 | R | Random burst sizes (1-256) at random start addresses | 100 reads | Port 0 at 25%, LCG | All reads return correct data; no address overflow |
| P086 | S | Read during queue drain burst: queue drains 50 entries consecutively | 10 reads | Port 0, clustered bins | Read deferred by update pipeline staying active; measure max deferral |
| P087 | S | Pingpong disabled: burst read while updates active | 10 reads | Port 0 at 50%, pingpong off | Read deferred until both pipeline empty AND no new upd_valid; verify timing |
| P088 | S | Concurrent burst read + CSR read | 50 iterations | Port 0 at 25% | Both AVMM interfaces return correct data; no bus conflict |

---

## 8. Config Change Under Load (CCL) -- 10 cases

Configuration changes (left_bound, bin_width, filter settings) go through a two-phase commit: CSR write sets shadow registers, then a commit (CSR0 bit 0) raises cfg_apply_request. The actual apply waits for all ingress pipeline stages to drain (cfg_apply_pending=1 blocks port_ready, starving ingress). During apply, hits in the arbiter/divider/queue pipeline continue to use the OLD config. New config takes effect only after ingress_stage_valid is all-zero.

**Why this section exists:** In production, the operator may change histogram bounds between measurement runs without resetting the core. If the transition is not clean, hits computed under the old config may be applied with new bin boundaries, producing incorrect bin indices. These tests verify that the config transition is atomic and that the pipeline drains completely before the new config is applied.

**Output per test:** `{apply_latency_cycles, hits_under_old_config, hits_under_new_config, bin_correctness}`.

| ID | Method | Scenario | Duration/Iter | Stimulus | Checker |
|----|--------|----------|---------------|----------|---------|
| P089 | S | Change left_bound while port 0 at 50% | 10 changes | Port 0 active | Apply latency measured; no mixed-config hits; bins correct before and after |
| P090 | S | Change bin_width from 16 to 32 while port 0 at 25% | 5 changes | Port 0 active | Bin indices recomputed correctly after apply; old-config hits drained |
| P091 | S | Change filter_enable from 0 to 1 under load | 5 changes | Port 0 at 50% | Filter takes effect atomically; hits before apply unfiltered; hits after apply filtered |
| P092 | S | Rapid commit: apply 3 configs in sequence with 100-cycle gap | 3 applies | Port 0 at 25% | Each apply waits for previous to complete; no config corruption |
| P093 | S | Config apply while all 8 ports active at 12.5% | 5 changes | All ports active | Apply waits for all 8 ingress stages to drain; measure worst-case apply latency |
| P094 | S | Config change during bank swap (interval boundary) | 5 changes | Port 0 at 50%, interval=5k | Apply and bank swap can overlap; verify both complete correctly |
| P095 | S | Invalid config (bin_width=0, right_bound <= left_bound): commit rejected | 5 attempts | Port 0 at 25% | csr_error asserts; cfg_apply_request never fires; current config unchanged |
| P096 | S | Config change + immediate burst read | 5 changes | Port 0 at 25% | Burst read uses old config's data (already in SRAM); new config affects future hits only |
| P097 | R | Random config changes (LCG bounds/widths) every 10k hits | 50k hits | Port 0, 50% rate | Each config segment produces correct bin counts; no inter-config leakage |
| P098 | S | Config apply with zero hits in flight (quiescent) | 10 changes | All ports idle | Apply latency == 1 cycle (ingress already empty); instantaneous config switch |

---

## 9. Seed Sweep (SED) -- 8 cases

Rerun critical tests with multiple LCG seeds to expose seed-dependent bugs. The LCG PRNG (Questa FSE: no `rand`) determines bin distributions, injection timing, and burst patterns. A bug that appears with one seed but not another indicates a state-dependent corner case that a single seed would miss.

**Why this section exists:** Constrained-random verification depends on stimulus diversity. The LCG sequence with seed S produces a fixed, deterministic sequence. If a bug requires a specific bin collision pattern (e.g., two hits to the same bin exactly 13 cycles apart -- the full pipeline latency), only certain seeds will generate that pattern. This section ensures 8+ independent seed values are tested for each critical scenario.

**Output per test:** `{per_seed_pass_fail, per_seed_drop_count, per_seed_overflow_count}`.

| ID | Method | Scenario | Duration/Iter | Stimulus | Checker |
|----|--------|----------|---------------|----------|---------|
| P099 | R | MPT P015 (8 ports at 15%) x 8 seeds | 10k hits/port x 8 | Seeds 0x1234_0001..0008 | All 8 seeds pass; per-seed drop rate within expected bounds |
| P100 | R | COE P025 (uniform 256-bin) x 8 seeds | 10k hits x 8 | Seeds 0x5678_0001..0008 | Queue occupancy_max consistent across seeds (should be 256 for all) |
| P101 | R | PPS P039 (interval=100) x 8 seeds | 50 intervals x 8 | Seeds 0x9ABC_0001..0008 | Per-interval bin sum consistent across seeds within statistical bounds |
| P102 | R | SOK P065 (100k soak) x 4 seeds | 100k hits x 4 | Seeds 0xDEF0_0001..0004 | No hang in any seed; total_hits correct for all |
| P103 | R | BPC P062 (random multi-port) x 8 seeds | 50k cycles x 8 | Seeds 0x1111_0001..0008 | Drop rate variance across seeds < 5% of mean |
| P104 | R | COE P033 (Zipf dist) x 8 seeds | 10k hits x 8 | Seeds 0x2222_0001..0008 | Queue overflow behavior consistent; bin accuracy 100% for all seeds |
| P105 | R | CCL P097 (random config changes) x 4 seeds | 50k hits x 4 | Seeds 0x3333_0001..0004 | All config transitions clean for all seeds |
| P106 | R | Full pipeline stress (8 ports, 20%, interval=500) x 8 seeds | 50k cycles x 8 | Seeds 0x4444_0001..0008 | No seed-dependent hang or corruption |

---

## 10. Multi-Interval Accumulation (MIA) -- 10 cases

Each interval produces an independent histogram snapshot in the frozen bank. The active bank is cleared at swap, so each interval starts from zero. These tests verify that (a) the frozen bank accurately reflects the previous interval's hits and nothing more, (b) clearing the active bank takes exactly 256 cycles, (c) there is no inter-interval leakage (hits from interval N appearing in interval N+1's frozen bank), and (d) the interval pulse fires at the correct cycle.

**Why this section exists:** The entire purpose of ping-pong mode is to provide non-intrusive readout of a stable snapshot while accumulation continues. If even one hit leaks between intervals, the physical measurement (e.g., time-over-threshold distribution) is corrupted. These tests validate the isolation guarantee.

**Output per test:** `{per_interval_bin_sum, expected_per_interval_hits, interval_pulse_timing, frozen_bank_correctness}`.

| ID | Method | Scenario | Duration/Iter | Stimulus | Checker |
|----|--------|----------|---------------|----------|---------|
| P107 | S | 10 intervals, interval=10k, port 0 at 25% | 100k cycles | Uniform bins | Each frozen bank bin sum == accepted_hits_in_interval; zero leakage |
| P108 | S | 10 intervals, interval=10k, all 8 ports at 10% | 100k cycles | Uniform bins | Per-interval sum matches aggregate accepted across all ports |
| P109 | S | 100 intervals, interval=1000, port 0 at 25% | 100k cycles | Uniform bins | All 100 frozen snapshots correct; no drift in interval timing |
| P110 | S | 10 intervals with different injection rates per interval | 10 intervals | Port 0: rates 10/20/30/.../100% | Each frozen bank sum proportional to its interval's injection rate |
| P111 | S | Interval boundary: hit arrives on exact cycle of swap | 100 intervals | Port 0, timed hit | Hit goes to old interval (already in pipeline) or new interval (not yet in pipeline); verify deterministic placement |
| P112 | S | Read frozen bank, verify bin-by-bin correctness | 10 intervals | Port 0, 25%, known bin pattern | After each swap, burst-read frozen bank; compare every bin against reference model |
| P113 | S | Non-pingpong mode: single bank, manual clear + read | 10 cycles | Port 0, 25% | Write hist_bin with 0x00000000 to trigger clear; read back; verify counts correct before clear |
| P114 | S | Interval=1 (minimum possible): constant swap | 1000 intervals | Port 0, 100% rate | upd_ready never asserts (clear takes 256 cycles >> 1 cycle interval); zero updates; no hang |
| P115 | S | Interval=512 with burst read after every swap | 50 intervals | Port 0, 50% rate | Burst read completes within 256 cycles (during next interval's update window); data correct |
| P116 | R | Random hit timing relative to interval boundary (LCG) | 50 intervals | Port 0, 50%, LCG timing | Statistical verification: no bias toward hits appearing in wrong interval |

---

## 11. Port Imbalance (PIB) -- 10 cases

The round-robin arbiter gives each port equal priority regardless of FIFO fill level. When one port is at 100% and others are at 1%, the hot port receives only 1/N_active service slots, causing its FIFO to fill and eventually drop. These tests verify that (a) no port is completely starved (livelock-free), (b) the hot port's drop rate matches the analytical model, and (c) cold ports experience zero drops.

**Why this section exists:** In the real detector, one SciFi layer may have much higher hit rates than others (e.g., layers closer to the beam). The histogram must not lose data from low-rate layers just because a high-rate layer is overwhelming the arbiter. These tests verify that round-robin fairness prevents starvation.

**Output per test:** `{per_port_accepted[8], per_port_dropped[8], per_port_service_count[8], starvation_detected}`.

| ID | Method | Scenario | Duration/Iter | Stimulus | Checker |
|----|--------|----------|---------------|----------|---------|
| P117 | S | Port 0 at 100%, ports 1-7 at 1% | 10k hits on port 0 | Port 0 back-to-back; others sparse | Port 0 drop rate ~87.5% (gets 1/8 service); ports 1-7 zero drops |
| P118 | S | Port 0 at 100%, ports 1-7 at 0% (inactive) | 10k hits | Port 0 only | Port 0 zero drops; arbiter always grants port 0 |
| P119 | S | Port 0 at 100%, port 1 at 100%, ports 2-7 at 0% | 10k hits each | Two hot ports | Each hot port gets ~50% service; ~50% drop rate each; zero starvation |
| P120 | S | Ports 0-3 at 50%, ports 4-7 at 1% | 10k hits/port | 4 hot, 4 cold | Hot ports ~75% drop rate (get 1/8 each, need 4/8 total); cold ports zero drops |
| P121 | S | Port 7 at 100%, ports 0-6 at 1% | 10k hits on port 7 | Port 7 hot | Verify no positional bias in round-robin; port 7 drop rate same as port 0 in P117 |
| P122 | S | Rotating hot port: port i at 100% for 1000 cycles, then port i+1 | 8k cycles | Rotating | Each port's hot phase produces same drop rate; no residual state from previous hot port |
| P123 | R | Random per-port rates (LCG): one port always >= 80% | 20k hits total | LCG rates with constraint | Hot port drops, cold ports lossless; aggregate throughput ~1 hit/cycle |
| P124 | S | All ports at 12.5% except port 0 at 0% | 10k hits/port | 7 active ports | 7-port round-robin: each gets 1/7 service (~14.3%); zero drops at 12.5% per port |
| P125 | S | Gradual imbalance: port 0 ramps from 10% to 100% over 10k cycles | 10k cycles | Port 0 ramp; others at 10% | Drop onset detected; measure exact rate at which drops begin |
| P126 | S | Extreme imbalance: port 0 at 100%, ports 1-7 at 12.5% each | 5k hits/port 0 | 8 active ports | Aggregate 187.5%; all ports drop; port 0 drops most; verify fairness within 10% |

---

## 12. Queue Saturation (QST) -- 14 cases

The coalescing queue has COAL_QUEUE_DEPTH=256 entries. When all 256 entries are occupied (256 distinct bins in-flight) and a new distinct bin arrives, either the queue must drain an entry first (if drain_fire happens simultaneously) or the hit is counted as overflow. The kick counter is 8 bits: a bin already in the queue can absorb up to 255 hits before the counter saturates.

**Why this section exists:** The queue is the most complex resource in the design. Its overflow counter is the primary diagnostic for understanding whether the histogram is losing data. These tests verify that (a) the overflow counter accurately counts every lost hit, (b) the queue correctly handles simultaneous enqueue and drain, (c) kick saturation at 255 is handled without corruption, (d) the queue clears correctly after overflow, and (e) the overflow counter itself does not wrap (16-bit saturating).

**Output per test:** `{queue_overflow_count, queue_occupancy_max, expected_overflow, kick_saturation_events, bin_accuracy}`.

| ID | Method | Scenario | Duration/Iter | Stimulus | Checker |
|----|--------|----------|---------------|----------|---------|
| P127 | S | Drive exactly 256 distinct bins (0-255) sequentially | 256 hits | Port 0, 100% rate | Queue reaches 256; no overflow (room available for each bin); overflow_count == 0 |
| P128 | S | Drive 257 distinct bins (impossible: only 256 exist) via same-bin wrapping | 512 hits | Port 0, 100%, sequential scan x2 | Second pass coalesces with first; queue stays at 256; zero overflow |
| P129 | S | Fill queue to 256, then send 100 hits to new bins (all already queued) | 356 hits | Port 0, sequential then repeat | All 100 extra hits coalesce; overflow_count == 0 |
| P130 | S | Fill queue to 256, then send hits while drain_ready = 0 | 300 hits | Port 0, 100%, queue stalled | Queue cannot drain; hits to already-queued bins still coalesce; hits to new bins impossible (all 256 bins queued); kick saturation causes overflow |
| P131 | S | Kick saturation: 256 consecutive hits to bin 0 | 256 hits | Port 0, 100%, single bin | kick_count reaches 255 on 255th hit; 256th hit: kick already at max, overflow_count += 1 |
| P132 | S | Kick saturation across all bins: 256 hits each to all 256 bins | 65536 hits | Port 0, 50% rate | Each bin's kick saturates once at 255; drain and refill; total overflow == 256 (one per bin) |
| P133 | S | Queue drain race: hit and drain to same bin on same cycle | 1k hits | Port 0, 50%, single-bin focus | Simultaneous drain_fire and hit_valid for same bin; verify clean handoff (queued_effective = 0) |
| P134 | S | Queue occupancy_max accuracy: fill to 200, drain to 0, fill to 256 | 1k hits | Port 0, controlled pattern | occupancy_max == 256 (tracks true peak); never resets until clear |
| P135 | S | Overflow counter saturation: drive 65536+ overflow events | Until overflow saturates | Port 0, 100%, single bin | overflow_count saturates at 0xFFFF (16-bit); no wrap to 0 |
| P136 | R | Random bin distribution with queue near-full (240-256 occupancy) | 50k hits | Port 0, 50%, LCG bins | Overflow only when 256 distinct bins queued AND new distinct bin arrives; counter accurate |
| P137 | S | Clear during queue overflow: issue measure_clear_pulse at queue_level=256 | 10 iterations | Port 0, sequential bins | Queue clears in 256 cycles; overflow_count resets; queue resumes clean |
| P138 | S | Queue drain with pingpong clear: upd_ready=0 blocks drain | 10 intervals | Port 0, 50%, interval=300 | Drain stalls during 256-cycle clear; queue accumulates; verify no lost entries |
| P139 | K | 100k hits, 256-bin sequential scan, queue always near-full | 100k hits | Port 0, 25% rate, sequential bins | Queue pointer wraps many times; no pointer corruption; occupancy_max == 256 |
| P140 | S | Simultaneous queue overflow + bank swap | 20 intervals | Port 0, 100%, interval=500, sequential bins | Queue overflows while clear_active blocks drain; verify overflow counted, not lost; frozen bank correct |

---

## 13. Long-Run Coverage Harvest and Plateau Campaigns -- 8 cases

These are promoted closure runs, not generic benchmarking tests. Each case is a
parameterised long-run transaction mix with incremental hit-count snapshots so
signoff can judge coverage gained per wall-clock second and stop when the
incremental gain plateaus.

**Why this section exists:** once the directed and medium-length PERF suite is
in place, extra runtime only matters if it expands observable DUT state space.
These cases are the curated long-run farm used to answer that question.

**Output per test:** `{txn_count, wall_s, stmt_pct, branch_pct, cond_pct,
expr_pct, toggle_pct, covergroups_pct, delta_total_pct_per_s,
delta_toggle_pct, plateau_assessment}`.

| ID | Method | Scenario | Duration/Iter | Stimulus | Checker |
|----|--------|----------|---------------|----------|---------|
| P141 | R | Real-hit triad harvest | 256/512/1024/2048 hits per phase | `hist_real_hit_rand_test` profile=`triad` | Baseline real-hit mix; measure when general-purpose toggle/branch growth saturates |
| P142 | R | Boundary-focused harvest | 256/512/1024/2048 hits | `hist_real_hit_rand_test` profile=`boundary` | Signed ET values biased to underflow/overflow and exact-boundary crossings; verifies whether boundary logic still buys new structure |
| P143 | R | Queue/coalescing harvest | 256/512/1024/2048 hits | `hist_real_hit_rand_test` profile=`queue` | Multi-port 256-bin sweep with repeated same-bin bursts; targets queue occupancy, coalescing, and overflow-adjacent logic |
| P144 | R | Interval/apply harvest | 128/256/512/1024 hits | `hist_real_hit_rand_test` profile=`interval` | Short-interval config changes plus swaps and reads; targets timer/swap/apply interaction |
| P145 | K | Baseline + P141 promotion sweep | Whole-suite merge | Deterministic baseline followed by P141 final point | Promote only if merged gain-per-second stays above plateau threshold |
| P146 | K | Baseline + P142 promotion sweep | Whole-suite merge | Deterministic baseline followed by P142 final point | Keep only if boundary-focused long run adds merged structural information beyond P141 |
| P147 | K | Baseline + P143 promotion sweep | Whole-suite merge | Deterministic baseline followed by P143 final point | Keep only if queue/coalescing profile adds merged toggle/branch coverage after baseline |
| P148 | K | Plateau signoff campaign | Whole-suite merge | Deterministic baseline plus all promoted P141-P144 cases | Final signoff uses cumulative gain-per-second to justify stopping or adding a new stimulus idea |

---

## Summary

| Section | Cases | ID Range | Focus |
|---------|-------|----------|-------|
| Single-Port Throughput (SPT) | 12 | P001-P012 | Per-port FIFO depth, single-port drop onset |
| Multi-Port Throughput (MPT) | 12 | P013-P024 | Arbiter fairness, aggregate bandwidth |
| Coalescing Efficiency (COE) | 14 | P025-P038 | Queue occupancy vs bin distribution |
| Ping-Pong Stress (PPS) | 14 | P039-P052 | Short intervals, bank swap correctness |
| Backpressure Characterization (BPC) | 12 | P053-P064 | FIFO fill dynamics, drop thresholds |
| Long Soak (SOK) | 12 | P065-P076 | Counter overflow, timer drift, hangs |
| Burst Read Performance (BRP) | 12 | P077-P088 | Host readout latency, read/write interaction |
| Config Change Under Load (CCL) | 10 | P089-P098 | Atomic config transition, pipeline drain |
| Seed Sweep (SED) | 8 | P099-P106 | Seed-dependent bug exposure |
| Multi-Interval Accumulation (MIA) | 10 | P107-P116 | Inter-interval isolation, frozen bank accuracy |
| Port Imbalance (PIB) | 10 | P117-P126 | Round-robin starvation, hot/cold fairness |
| Queue Saturation (QST) | 14 | P127-P140 | Overflow counter, kick saturation, pointer wrap |
| Long-Run Coverage Harvest (LHC) | 8 | P141-P148 | Coverage-vs-wall-clock closure and plateau signoff |
| **Total** | **148** | P001-P148 | |
