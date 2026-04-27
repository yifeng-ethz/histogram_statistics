# histogram_statistics_v2 DV — Crossing Coverage

**Parent:** [DV_PLAN.md](DV_PLAN.md)
**Companion docs:** [DV_PLAN.md](DV_PLAN.md), [DV_HARNESS.md](DV_HARNESS.md), [DV_REPORT.md](DV_REPORT.md), [DV_COV.md](DV_COV.md)
**DUT:** `histogram_statistics_v2` (Rev 1.2)
**Date:** 2026-04-09
**Status:** Planning

---

## 0. Conventions

- **Transaction**: One complete stimulus sequence: configure DUT (optional), inject
  hits, optionally read back bins/CSRs. The DUT is NOT reset between transactions.
- **Sampling point**: Coverage is sampled when the scoreboard marks a transaction as
  complete (all hits injected and pipeline drained, or readback complete).
- **Bin notation**: `bins X = {values}` uses SystemVerilog covergroup syntax.
- **Priority**: P1 = must-have (blocks tapeout), P2 = important (regression quality
  gate), P3 = nice-to-have (extra confidence).
- **Target**: Default 95% per cross unless noted otherwise.

---

## 1. Coverpoint Definitions

### 1.1 Config Space Coverpoints

```systemverilog
covergroup cg_config @(posedge txn_complete);

    // Operating mode: 0 = normal, -1..-6 = debug, positive = reserved
    cp_mode : coverpoint cfg_mode {
        bins normal      = {4'h0};
        bins debug_1     = {4'hF};  // -1 in 4-bit signed
        bins debug_2     = {4'hE};  // -2
        bins debug_3     = {4'hD};  // -3
        bins debug_4     = {4'hC};  // -4
        bins debug_5     = {4'hB};  // -5
        bins debug_6     = {4'hA};  // -6
        bins reserved    = {[4'h1:4'h9]};
    }

    // Key representation
    cp_key_unsigned : coverpoint cfg_key_unsigned {
        bins unsigned_key = {1'b1};
        bins signed_key   = {1'b0};
    }

    // Filter enable
    cp_filter_enable : coverpoint cfg_filter_enable {
        bins disabled = {1'b0};
        bins enabled  = {1'b1};
    }

    // Filter reject (accept-match vs reject-match)
    cp_filter_reject : coverpoint cfg_filter_reject {
        bins accept_mode = {1'b0};
        bins reject_mode = {1'b1};
    }

    // Bin width values (0 = direct bounds mode, powers of 2, non-power-of-2)
    cp_bin_width : coverpoint cfg_bin_width {
        bins zero      = {16'd0};
        bins one       = {16'd1};
        bins small     = {[16'd2:16'd15]};
        bins default16 = {16'd16};
        bins medium    = {[16'd17:16'd127]};
        bins pow2_128  = {16'd128};
        bins pow2_256  = {16'd256};
        bins large     = {[16'd257:16'd1023]};
        bins pow2_1024 = {16'd1024};
        bins huge      = {[16'd1025:16'hFFFF]};
    }

    // Left bound (signed)
    cp_left_bound : coverpoint cfg_left_bound_signed {
        bins large_neg = {[$:(-32768)]};
        bins small_neg = {[(-32767):(-1)]};
        bins zero      = {0};
        bins small_pos = {[1:32767]};
        bins large_pos = {[32768:$]};
    }

    // Key extraction bit range: width of key field
    cp_key_field_width : coverpoint (cfg_update_key_hi - cfg_update_key_lo + 1) {
        bins narrow   = {[1:4]};
        bins medium   = {[5:12]};
        bins default13= {13};            // default UPDATE_KEY_BIT_HI(29)-LO(17)+1
        bins wide     = {[14:16]};
        bins max      = {[17:39]};       // up to AVST_DATA_WIDTH
    }

    // Filter key match value
    cp_filter_key_lo_range : coverpoint cfg_filter_key_lo {
        bins low    = {[0:9]};
        bins mid    = {[10:29]};
        bins high   = {[30:38]};
    }

    cp_filter_key_hi_range : coverpoint cfg_filter_key_hi {
        bins low    = {[0:9]};
        bins mid    = {[10:29]};
        bins high   = {[30:38]};
    }

endgroup
```

### 1.2 Stimulus Space Coverpoints

```systemverilog
covergroup cg_stimulus @(posedge txn_complete);

    // Port index (which of the 8 ports received hits)
    cp_port_index : coverpoint txn_port_index {
        bins port[] = {[0:7]};
    }

    // Number of active ports in this transaction
    cp_active_ports : coverpoint txn_active_port_count {
        bins single   = {1};
        bins dual     = {2};
        bins quad     = {[3:4]};
        bins many     = {[5:7]};
        bins all      = {8};
    }

    // Key value relative to histogram bounds
    cp_key_region : coverpoint txn_key_region {
        bins underflow   = {REGION_UNDERFLOW};     // key < left_bound
        bins bin0_exact  = {REGION_BIN0};          // key == left_bound
        bins low_bins    = {REGION_LOW};            // bin_index in [1:63]
        bins mid_bins    = {REGION_MID};            // bin_index in [64:191]
        bins high_bins   = {REGION_HIGH};           // bin_index in [192:253]
        bins last_bin    = {REGION_LAST_BIN};       // bin_index == 254
        bins bin255      = {REGION_BIN255};         // bin_index == 255
        bins overflow    = {REGION_OVERFLOW};       // key >= right_bound
    }

    // Total hit count per transaction
    cp_hit_count : coverpoint txn_hit_count {
        bins single    = {1};
        bins few       = {[2:7]};
        bins byte_ish  = {[8:63]};
        bins moderate  = {[64:255]};
        bins batch     = {[256:999]};
        bins stress    = {[1000:$]};
    }

    // Injection rate (hits per clock cycle, averaged over the transaction)
    cp_injection_rate : coverpoint txn_injection_rate_class {
        bins idle       = {RATE_IDLE};             // 0 hits (config-only txn)
        bins low        = {RATE_LOW};              // < 1 hit per 16 clocks
        bins medium     = {RATE_MEDIUM};           // 1 hit per 4-16 clocks
        bins high       = {RATE_HIGH};             // 1 hit per 1-4 clocks
        bins saturating = {RATE_SATURATING};       // 1 hit per clock sustained
    }

    // Per-port injection distribution in multi-port transactions
    cp_port_skew : coverpoint txn_port_skew_class {
        bins uniform    = {SKEW_UNIFORM};          // all ports +/- 10%
        bins biased     = {SKEW_BIASED};           // one port > 50% of hits
        bins single     = {SKEW_SINGLE};           // only one port active
    }

    // Key distribution within the valid range
    cp_key_distribution : coverpoint txn_key_dist_class {
        bins uniform    = {DIST_UNIFORM};          // spread across all bins
        bins clustered  = {DIST_CLUSTERED};        // >50% of hits in 4 bins
        bins single_bin = {DIST_SINGLE_BIN};       // all hits to one bin
        bins boundary   = {DIST_BOUNDARY};         // hits near left/right bound
    }

endgroup
```

### 1.3 Pipeline State Coverpoints

```systemverilog
covergroup cg_pipeline_state @(posedge sample_pipeline);

    // Per-port FIFO occupancy (sampled at hit injection time)
    // FIFO_ADDR_WIDTH=8, so depth=256, level 0..256
    cp_fifo_occ_port0 : coverpoint fifo_level[0] {
        bins empty     = {0};
        bins low       = {[1:64]};
        bins mid       = {[65:160]};
        bins high      = {[161:255]};
        bins full      = {256};
    }
    // (Repeat cp_fifo_occ_port1..port7 with same bins)

    // Aggregate: maximum FIFO occupancy across all ports
    cp_fifo_max_any : coverpoint fifo_max_level {
        bins empty     = {0};
        bins low       = {[1:4]};
        bins mid       = {[5:10]};
        bins high      = {[11:15]};
        bins full      = {16};
    }

    // Coalescing queue occupancy
    // QUEUE_DEPTH=256, occupancy 0..256
    cp_queue_occ : coverpoint queue_occupancy {
        bins empty      = {0};
        bins low        = {[1:32]};
        bins mid        = {[33:128]};
        bins high       = {[129:248]};
        bins near_full  = {[249:255]};
        bins full       = {256};
    }

    // Queue overflow event (kick counter saturation or queue full)
    cp_queue_overflow : coverpoint queue_overflow_seen {
        bins no_overflow  = {1'b0};
        bins overflow     = {1'b1};
    }

    // Active bank in pingpong
    cp_active_bank : coverpoint active_bank {
        bins bank0 = {1'b0};
        bins bank1 = {1'b1};
    }

    // Flushing state (bank clear in progress)
    cp_flushing : coverpoint flushing {
        bins idle     = {1'b0};
        bins clearing = {1'b1};
    }

    // Config apply pending (ingress blocked)
    cp_apply_pending : coverpoint cfg_apply_pending {
        bins clear   = {1'b0};
        bins pending = {1'b1};
    }

endgroup
```

### 1.4 Result Space Coverpoints

```systemverilog
covergroup cg_result @(posedge txn_complete);

    // Bin index of the hit (per divider output)
    cp_bin_index : coverpoint result_bin_index {
        bins first     = {8'd0};
        bins second    = {8'd1};
        bins low       = {[8'd2:8'd63]};
        bins mid       = {[8'd64:8'd191]};
        bins high      = {[8'd192:8'd253]};
        bins penult    = {8'd254};
        bins last      = {8'd255};
    }

    // Underflow observed in this transaction
    cp_underflow : coverpoint txn_had_underflow {
        bins no  = {1'b0};
        bins yes = {1'b1};
    }

    // Overflow observed in this transaction
    cp_overflow : coverpoint txn_had_overflow {
        bins no  = {1'b0};
        bins yes = {1'b1};
    }

    // Drops observed in this transaction (FIFO full)
    cp_dropped : coverpoint txn_had_drops {
        bins no  = {1'b0};
        bins yes = {1'b1};
    }

    // Coalesced (same bin hit while already queued)
    cp_coalesced : coverpoint txn_had_coalescing {
        bins no  = {1'b0};
        bins yes = {1'b1};
    }

    // Kick counter saturation observed
    cp_kick_saturated : coverpoint txn_had_kick_saturation {
        bins no  = {1'b0};
        bins yes = {1'b1};
    }

    // Bin count correctness result (from scoreboard compare)
    cp_compare_result : coverpoint txn_compare_passed {
        bins pass = {1'b1};
        bins fail = {1'b0};
    }

endgroup
```

### 1.5 Timing Coverpoints

```systemverilog
covergroup cg_timing @(posedge txn_complete);

    // Hits injected during a bank swap (interval_pulse active)
    cp_hits_during_swap : coverpoint txn_hits_during_bank_swap {
        bins none    = {1'b0};
        bins present = {1'b1};
    }

    // Hits injected during bank clear (flushing=1)
    cp_hits_during_clear : coverpoint txn_hits_during_clear {
        bins none    = {1'b0};
        bins present = {1'b1};
    }

    // Hits injected while cfg_apply_pending=1
    cp_hits_during_apply : coverpoint txn_hits_during_apply {
        bins none    = {1'b0};
        bins present = {1'b1};
    }

    // Burst read issued while update pipeline is active
    cp_burst_during_update : coverpoint txn_burst_during_update {
        bins none    = {1'b0};
        bins present = {1'b1};
    }

    // Measure clear issued while hits are in the pipeline
    cp_clear_during_pipeline : coverpoint txn_clear_during_pipeline_active {
        bins none    = {1'b0};
        bins present = {1'b1};
    }

    // Interval pulse fires during burst read
    cp_swap_during_burst : coverpoint txn_swap_during_burst_read {
        bins none    = {1'b0};
        bins present = {1'b1};
    }

endgroup
```

---

## 2. Intra-Transaction Cross Definitions

Each cross below captures interactions between coverpoints within a single
transaction. The scoreboard samples all coverpoints at transaction completion.

### 2.1 X01: Mode x Key Unsigned (P1)

```systemverilog
x_mode_key : cross cp_mode, cp_key_unsigned;
```

| Field | Detail |
|-------|--------|
| **Why** | Debug modes (-1..-6) bypass the normal key path and use a fixed signed interpretation. Normal mode uses the configurable `key_unsigned` flag. Crossing these catches bugs where the key_unsigned flag leaks into debug mode or debug mode inadvertently forces signed interpretation in normal mode. |
| **Bins** | 8 modes x 2 = 16 |
| **Exclusions** | None. All combinations are legal (debug modes ignore key_unsigned, but the cross verifies they truly ignore it). |
| **Priority** | P1 |
| **Target** | 95% |

### 2.2 X02: Filter Enable x Filter Reject x Mode (P1)

```systemverilog
x_filter_mode : cross cp_filter_enable, cp_filter_reject, cp_mode {
    // Debug modes bypass the filter entirely; accept all combinations to verify
    // the filter is truly bypassed.
}
```

| Field | Detail |
|-------|--------|
| **Why** | The filter has four states: disabled, enabled-accept, enabled-reject, disabled-reject. In debug modes the filter is bypassed. Crossing mode catches bugs where the filter is still active in debug mode or where reject polarity is wrong in normal mode. |
| **Bins** | 2 x 2 x 8 = 32 |
| **Exclusions** | None (filter_reject when filter_enable=0 is harmless but must be verified as no-op). |
| **Priority** | P1 |
| **Target** | 95% |

### 2.3 X03: Bin Width x Left Bound x Key Region (P1)

```systemverilog
x_binning : cross cp_bin_width, cp_left_bound, cp_key_region;
```

| Field | Detail |
|-------|--------|
| **Why** | The bin_divider computes `(key - left_bound) / bin_width` with a restoring divider. Boundary conditions depend on all three: bin_width=0 uses direct bounds, bin_width=1 makes every unit a bin, large bin_width collapses many values into one bin. Negative left_bound with unsigned overflow in subtraction is a known risk. This cross catches off-by-one errors, wrong sign extension, and division-by-zero handling. |
| **Bins** | 10 x 5 x 8 = 400 |
| **Exclusions** | `bin_width=0` with `key_region in {bin0_exact, low_bins, mid_bins, high_bins, last_bin, bin255}` are unlikely (bin_width=0 means direct right_bound, so only underflow/overflow typically), but NOT excluded since they exercise the non-division fallback path. |
| **Priority** | P1 |
| **Target** | 90% (some corner combinations hard to hit) |

### 2.4 X04: Port Index x Key Region x Active Ports (P2)

```systemverilog
x_port_key : cross cp_port_index, cp_key_region, cp_active_ports;
```

| Field | Detail |
|-------|--------|
| **Why** | Port offset (`port_index * CHANNELS_PER_PORT = port * 32`) shifts keys before the divider. A hit on port 7 with key=0 gets shifted by 224, which could push it from bin0 into overflow. This cross catches port-offset arithmetic bugs, especially when multiple ports are active and the round-robin arbiter interleaves them. |
| **Bins** | 8 x 8 x 5 = 320 |
| **Exclusions** | Some (port_index, active_ports) pairs are illegal: e.g. `port_index=5` with `active_ports=single` unless that single port is port 5. Exclude `port_index >= active_ports` when `active_ports < 8` and ports are numbered 0..active_ports-1. |
| **Priority** | P2 |
| **Target** | 90% |

### 2.5 X05: Hit Count x Injection Rate x Queue Occupancy (P1)

```systemverilog
x_throughput : cross cp_hit_count, cp_injection_rate, cp_queue_occ;
```

| Field | Detail |
|-------|--------|
| **Why** | The coalescing queue (depth 256) is the primary backpressure point. High hit counts at saturating rates stress queue occupancy. Low rates may never fill the queue. This cross catches queue management bugs: pointer wrap, occupancy miscounting, drain stalls. |
| **Bins** | 6 x 5 x 6 = 180 |
| **Exclusions** | `hit_count=single` with `queue_occ=full` is impossible (1 hit cannot fill a 256-deep queue). `injection_rate=idle` with any `hit_count` except 0 is impossible. Exclude these. |
| **Priority** | P1 |
| **Target** | 85% (queue full + high rate is hard to sustain predictably) |

### 2.6 X06: Queue Occupancy x Queue Overflow x Dropped (P1)

```systemverilog
x_backpressure : cross cp_queue_occ, cp_queue_overflow, cp_dropped;
```

| Field | Detail |
|-------|--------|
| **Why** | Data loss has two independent paths: FIFO drops (ingress) and queue overflow (post-divider). This cross verifies all combinations are observable and correctly reported. Particularly important: queue overflow without FIFO drops (queue bottleneck), FIFO drops without queue overflow (ingress burst faster than drain), both simultaneously (sustained overload). |
| **Bins** | 6 x 2 x 2 = 24 |
| **Exclusions** | None. |
| **Priority** | P1 |
| **Target** | 95% |

### 2.7 X07: Active Bank x Flushing x Hits During Swap (P1)

```systemverilog
x_bank_swap : cross cp_active_bank, cp_flushing, cp_hits_during_swap;
```

| Field | Detail |
|-------|--------|
| **Why** | Bank swap is the critical integrity boundary. During swap, the newly active bank is cleared (flushing=1). Hits arriving during this window must be queued (coalescing_queue accepts them) but cannot be written to SRAM until flushing completes. This cross catches: lost hits during swap, data written to wrong bank, stale data from cleared bank leaking into results. |
| **Bins** | 2 x 2 x 2 = 8 |
| **Exclusions** | `flushing=0` with `hits_during_swap=present` is unlikely in a single sample but possible if the swap completes within the sampling window. Not excluded. |
| **Priority** | P1 |
| **Target** | 100% (all 8 bins must be hit) |

### 2.8 X08: FIFO Occupancy x Hit Count x Active Ports (P2)

```systemverilog
x_fifo_stress : cross cp_fifo_max_any, cp_hit_count, cp_active_ports;
```

| Field | Detail |
|-------|--------|
| **Why** | Each port has its own depth-16 FIFO. With 8 ports injecting simultaneously, the arbiter can only drain one port per cycle. This cross verifies the FIFO fill level correlates correctly with injection patterns. Catches arbiter starvation (one port always full while others empty) and FIFO pointer bugs under sustained load. |
| **Bins** | 5 x 6 x 5 = 150 |
| **Exclusions** | `fifo_max=full` with `hit_count=single` and `active_ports=single` is impossible. |
| **Priority** | P2 |
| **Target** | 85% |

### 2.9 X09: Bin Width x Bin Index (P1)

```systemverilog
x_div_result : cross cp_bin_width, cp_bin_index;
```

| Field | Detail |
|-------|--------|
| **Why** | The restoring divider has 8 pipeline stages (BIN_INDEX_WIDTH=8). Different bin widths exercise different quotient bit patterns. bin_width=1 means the quotient matches the delta directly; bin_width=256 means only bin 0 is ever used (for a 256-bin histogram with default bounds). Large bin_width with high bin indices tests whether the divider correctly produces the full 8-bit quotient. |
| **Bins** | 10 x 7 = 70 |
| **Exclusions** | `bin_width=huge` with `bin_index=last` may be unreachable if `N_BINS * bin_width` exceeds the key range. Mark as expected-miss. |
| **Priority** | P1 |
| **Target** | 85% |

### 2.10 X10: Key Unsigned x Key Region x Left Bound (P1)

```systemverilog
x_key_sign : cross cp_key_unsigned, cp_key_region, cp_left_bound;
```

| Field | Detail |
|-------|--------|
| **Why** | Signed vs unsigned key extraction uses different paths in `build_key()`. Unsigned keys are zero-extended; signed keys are sign-extended. A negative left_bound with an unsigned key can create surprising underflow/overflow results. This cross catches sign-extension bugs and incorrect boundary comparisons when mixing signed bounds with unsigned keys. |
| **Bins** | 2 x 8 x 5 = 80 |
| **Exclusions** | `key_unsigned=1` with `key_region=underflow` and `left_bound=large_neg` is very unlikely (unsigned key is always >= 0, so underflow only happens if left_bound > 0). Not excluded since it verifies the boundary check correctly handles this case. |
| **Priority** | P1 |
| **Target** | 90% |

### 2.11 X11: Filter Enable x Key Region x Hit Count (P2)

```systemverilog
x_filter_throughput : cross cp_filter_enable, cp_key_region, cp_hit_count;
```

| Field | Detail |
|-------|--------|
| **Why** | When the filter is enabled, some hits are rejected before reaching the FIFO. The total_hits counter counts all accepted hits (including filtered), but the bin counts only reflect passing hits. This cross verifies that high-volume filtered traffic does not cause pipeline stalls, counter overflows, or hits leaking through the filter. |
| **Bins** | 2 x 8 x 6 = 96 |
| **Exclusions** | None. |
| **Priority** | P2 |
| **Target** | 90% |

### 2.12 X12: Burst During Update x Active Bank x Flushing (P2)

```systemverilog
x_burst_conflict : cross cp_burst_during_update, cp_active_bank, cp_flushing;
```

| Field | Detail |
|-------|--------|
| **Why** | In non-pingpong mode, burst reads are deferred when the update pipeline is active (`upd_ready_int` gating). In pingpong mode, reads target the frozen bank. This cross catches: read from wrong bank during/after swap, stale data served during clearing, burst read deferred indefinitely (livelock). |
| **Bins** | 2 x 2 x 2 = 8 |
| **Exclusions** | None. |
| **Priority** | P2 |
| **Target** | 100% (all 8 bins) |

### 2.13 X13: Apply Pending x Hits During Apply x Hit Count (P1)

```systemverilog
x_apply_block : cross cp_apply_pending, cp_hits_during_apply, cp_hit_count;
```

| Field | Detail |
|-------|--------|
| **Why** | When `cfg_apply_pending=1`, ingress is blocked (`ingress_accept` forced to 0). Hits arriving during this window should be backpressured (via ready=0), not dropped or silently lost. This cross catches: hits accepted during apply, ready signal not deasserted, config applied with stale pipeline data. |
| **Bins** | 2 x 2 x 6 = 24 |
| **Exclusions** | `apply_pending=0` with `hits_during_apply=present` is impossible. Exclude. |
| **Priority** | P1 |
| **Target** | 90% |

### 2.14 X14: Clear During Pipeline x Queue Occupancy x FIFO Occupancy (P2)

```systemverilog
x_clear_mid_flight : cross cp_clear_during_pipeline, cp_queue_occ, cp_fifo_max_any;
```

| Field | Detail |
|-------|--------|
| **Why** | Measure clear (`measure_clear_pulse`) resets the entire pipeline: FIFOs, arbiter, divider, queue, pingpong. If the pipeline has in-flight data (FIFO non-empty, queue non-empty), the clear must discard it cleanly without corrupting RAM state. This cross catches: partial clear leaving stale queue entries, clear race with drain producing a spurious write, FIFO pointer corruption after mid-flight clear. |
| **Bins** | 2 x 6 x 5 = 60 |
| **Exclusions** | `clear_during_pipeline=0` with `queue_occ=full` is legal (clear happened after queue drained). Not excluded. |
| **Priority** | P2 |
| **Target** | 85% |

### 2.15 X15: Mode x Port Index x Key Distribution (P2)

```systemverilog
x_mode_port : cross cp_mode, cp_port_index, cp_key_distribution {
    // In debug mode (mode < 0), only port 0 is active.
    ignore_bins debug_non_port0 =
        binsof(cp_mode) intersect {[4'hA:4'hF]} &&
        binsof(cp_port_index) intersect {[1:7]};
}
```

| Field | Detail |
|-------|--------|
| **Why** | Debug modes route a single debug interface through port 0 only. Normal mode uses all 8 ports. This cross verifies port isolation: debug data appears only in bin counts reachable from port 0 offset, and normal-mode port data does not contaminate debug results. |
| **Bins** | 8 x 8 x 4 = 256, minus 6x7x4=168 ignored = 88 effective |
| **Exclusions** | Debug mode with port > 0 is impossible (debug only uses port 0). Marked `ignore_bins`. |
| **Priority** | P2 |
| **Target** | 90% |

### 2.16 X16: Swap During Burst x Active Bank (P1)

```systemverilog
x_swap_burst_race : cross cp_swap_during_burst, cp_active_bank;
```

| Field | Detail |
|-------|--------|
| **Why** | If an interval pulse fires while a burst read is in progress, the `read_bank_latched` signal freezes the read bank for the burst. This cross verifies the burst reads the correct (frozen) bank even when the active bank changes mid-burst. A bug here would cause the burst to read a mix of old and new bank data. |
| **Bins** | 2 x 2 = 4 |
| **Exclusions** | None. |
| **Priority** | P1 |
| **Target** | 100% (all 4 bins) |

### 2.17 X17: Coalesced x Queue Occupancy x Key Distribution (P2)

```systemverilog
x_coalescing : cross cp_coalesced, cp_queue_occ, cp_key_distribution;
```

| Field | Detail |
|-------|--------|
| **Why** | Coalescing effectiveness depends on the key distribution. Clustered or single-bin patterns should coalesce heavily (queue stays low despite many hits). Uniform distribution across 256 bins fills the queue. This cross catches: coalescing not working (queue fills despite repeated same-bin hits), kick counter not accumulating correctly, drain-and-hit-same-cycle race in `gen_kick_bins`. |
| **Bins** | 2 x 6 x 4 = 48 |
| **Exclusions** | None. |
| **Priority** | P2 |
| **Target** | 85% |

### 2.18 X18: Kick Saturated x Hit Count x Coalesced (P2)

```systemverilog
x_kick_sat : cross cp_kick_saturated, cp_hit_count, cp_coalesced;
```

| Field | Detail |
|-------|--------|
| **Why** | The kick counter is 8-bit (KICK_WIDTH=8), saturating at 255. If more than 255 hits arrive for the same bin before it drains, the counter saturates and the overflow is counted. This cross catches: saturation arithmetic errors, overflow counter not incrementing, hits lost when kick counter is at max. |
| **Bins** | 2 x 6 x 2 = 24 |
| **Exclusions** | `kick_saturated=yes` with `hit_count=single` is impossible (need 256+ same-bin hits before drain). `kick_saturated=yes` with `coalesced=no` is impossible (saturation requires coalescing). |
| **Priority** | P2 |
| **Target** | 90% |

### 2.19 X19: Key Field Width x Key Unsigned x Bin Width (P2)

```systemverilog
x_key_extract : cross cp_key_field_width, cp_key_unsigned, cp_bin_width;
```

| Field | Detail |
|-------|--------|
| **Why** | The `build_key()` function extracts bits [key_hi:key_lo] from the data word and either zero-extends or sign-extends to SAR_TICK_WIDTH (32 bits). Narrow fields with signed interpretation can produce small negative keys. Wide fields can exceed the histogram range. Combined with bin_width, this exercises the full extraction + division pipeline. |
| **Bins** | 5 x 2 x 10 = 100 |
| **Exclusions** | None. |
| **Priority** | P2 |
| **Target** | 85% |

### 2.20 X20: Underflow x Overflow x Bin Index (P1)

```systemverilog
x_boundary_result : cross cp_underflow, cp_overflow, cp_bin_index;
```

| Field | Detail |
|-------|--------|
| **Why** | Underflow and overflow are mutually exclusive per hit, but both can occur in the same transaction across different hits. This cross verifies the divider correctly classifies each hit and that the bin_index output is valid only when neither flag is set. Catches: bin_index non-zero during underflow/overflow (stale pipeline data), both flags set simultaneously (logic bug). |
| **Bins** | 2 x 2 x 7 = 28 |
| **Exclusions** | `underflow=yes AND overflow=yes` with any `bin_index` at the per-hit level is illegal (mutually exclusive). At the transaction level both may be `yes` (different hits). No exclusion at the transaction level. |
| **Priority** | P1 |
| **Target** | 95% |

---

## 3. Transaction-Level Crosses (Consecutive Transactions)

These crosses capture the DUT state across transaction boundaries. The scoreboard
stores the previous transaction's properties and crosses them with the current
transaction. This catches bugs that manifest only when state left by transaction N
affects the result of transaction N+1.

### 3.1 T01: Config Change Cross (P1)

```systemverilog
covergroup cg_txn_config_change @(posedge txn_complete);

    // Previous transaction's config
    cp_prev_bin_width_class : coverpoint prev_cfg.bin_width_class {
        bins zero    = {BW_ZERO};
        bins small   = {BW_SMALL};
        bins default = {BW_DEFAULT};
        bins large   = {BW_LARGE};
    }

    // Current transaction's config
    cp_curr_bin_width_class : coverpoint curr_cfg.bin_width_class {
        bins zero    = {BW_ZERO};
        bins small   = {BW_SMALL};
        bins default = {BW_DEFAULT};
        bins large   = {BW_LARGE};
    }

    cp_prev_mode : coverpoint prev_cfg.mode {
        bins normal = {4'h0};
        bins debug  = {[4'hA:4'hF]};
    }

    cp_curr_mode : coverpoint curr_cfg.mode {
        bins normal = {4'h0};
        bins debug  = {[4'hA:4'hF]};
    }

    cp_prev_filter : coverpoint prev_cfg.filter_enable {
        bins off = {1'b0};
        bins on  = {1'b1};
    }

    cp_curr_filter : coverpoint curr_cfg.filter_enable {
        bins off = {1'b0};
        bins on  = {1'b1};
    }

    cp_config_changed : coverpoint config_changed {
        bins no_change = {1'b0};
        bins changed   = {1'b1};
    }

    // Cross: what changed from previous to current
    x_bin_width_transition : cross cp_prev_bin_width_class, cp_curr_bin_width_class;
    x_mode_transition      : cross cp_prev_mode, cp_curr_mode;
    x_filter_transition    : cross cp_prev_filter, cp_curr_filter;

    // Combined: mode transition AND whether config was explicitly changed
    x_mode_change : cross cp_prev_mode, cp_curr_mode, cp_config_changed;

endgroup
```

| Field | Detail |
|-------|--------|
| **Why** | Config applies are deferred (cfg_apply_pending mechanism). Changing config between transactions while the pipeline still holds data from the previous config is a rich source of bugs: stale config in divider, partial apply, wrong bounds on in-flight hits. The `config_changed=no` case with `mode_transition` verifies that identical re-apply is harmless. |
| **Bins** | bin_width: 4x4=16, mode: 2x2=4, filter: 2x2=4, combined: 2x2x2=8. Total: 32 unique cross bins. |
| **Priority** | P1 |
| **Target** | 95% |

### 3.2 T02: Bank Swap Data Integrity Cross (P1)

```systemverilog
covergroup cg_txn_bank_swap @(posedge txn_complete);

    // Did a bank swap occur during or between the previous and current transaction?
    cp_swap_occurred : coverpoint swap_between_txns {
        bins no_swap = {1'b0};
        bins swapped = {1'b1};
    }

    // Hits in the previous transaction's final phase (last 10% of hits)
    cp_prev_late_hits : coverpoint prev_txn_late_hit_count_class {
        bins none    = {LATE_NONE};
        bins few     = {LATE_FEW};        // 1-10 hits
        bins many    = {LATE_MANY};       // 10+ hits
    }

    // Hits in the current transaction's initial phase (first 10% of hits)
    cp_curr_early_hits : coverpoint curr_txn_early_hit_count_class {
        bins none    = {EARLY_NONE};
        bins few     = {EARLY_FEW};
        bins many    = {EARLY_MANY};
    }

    // Previous transaction's bin count total (before swap)
    cp_prev_bin_total_class : coverpoint prev_txn_bin_total_class {
        bins zero     = {TOTAL_ZERO};
        bins low      = {TOTAL_LOW};      // 1-100
        bins high     = {TOTAL_HIGH};     // 100+
    }

    x_swap_integrity : cross cp_swap_occurred, cp_prev_late_hits,
                             cp_curr_early_hits, cp_prev_bin_total_class;

endgroup
```

| Field | Detail |
|-------|--------|
| **Why** | When a bank swap occurs, the active bank switches and the new bank is cleared. Hits that were in-flight during the swap must land in the correct bank. Late hits from the previous transaction might get queued in the coalescing queue and drain into the NEW bank after the swap (incorrect) or be correctly attributed to the old bank. Early hits from the new transaction must go to the cleared bank. This cross catches: off-by-one in bank assignment during swap, hits from old interval appearing in new bank, incomplete clear of new bank. |
| **Bins** | 2 x 3 x 3 x 3 = 54 |
| **Exclusions** | `swap_occurred=no` with `prev_bin_total=zero` AND `prev_late_hits=many` is impossible. |
| **Priority** | P1 |
| **Target** | 85% |

### 3.3 T03: Clear Between Transactions Cross (P1)

```systemverilog
covergroup cg_txn_clear @(posedge txn_complete);

    // Was measure_clear issued between transactions?
    cp_clear_issued : coverpoint clear_between_txns {
        bins no_clear  = {1'b0};
        bins cleared   = {1'b1};
    }

    // Previous transaction's residual state
    cp_prev_queue_occ : coverpoint prev_txn_final_queue_occ_class {
        bins empty = {OCC_EMPTY};
        bins low   = {OCC_LOW};
        bins high  = {OCC_HIGH};
    }

    cp_prev_fifo_occ : coverpoint prev_txn_final_fifo_occ_class {
        bins empty = {OCC_EMPTY};
        bins low   = {OCC_LOW};
        bins high  = {OCC_HIGH};
    }

    // Current transaction's first readback result
    cp_curr_bins_zero : coverpoint curr_txn_initial_bins_all_zero {
        bins not_zero = {1'b0};
        bins all_zero = {1'b1};
    }

    // Current transaction's stats counters
    cp_curr_stats_zero : coverpoint curr_txn_initial_stats_zero {
        bins not_zero = {1'b0};
        bins all_zero = {1'b1};
    }

    x_clear_state : cross cp_clear_issued, cp_prev_queue_occ, cp_prev_fifo_occ;
    x_clear_effect : cross cp_clear_issued, cp_curr_bins_zero, cp_curr_stats_zero;

endgroup
```

| Field | Detail |
|-------|--------|
| **Why** | Measure clear resets both banks, all FIFOs, the arbiter, the divider pipeline, and the coalescing queue. If the previous transaction left residual data in the pipeline (non-empty queue, non-empty FIFOs), the clear must discard all of it. The `x_clear_effect` cross verifies that after a clear, readback returns all zeros and stats are reset. Catches: incomplete queue clear (clear_active finishes before all 256 bins are zeroed), FIFO pointer not reset, stale divider pipeline producing a hit after clear. |
| **Bins** | state: 2x3x3=18, effect: 2x2x2=8. Total: 26. |
| **Priority** | P1 |
| **Target** | 95% |

### 3.4 T04: Residual Queue State Cross (P2)

```systemverilog
covergroup cg_txn_residual @(posedge txn_complete);

    // Queue occupancy at start of current transaction (left over from previous)
    cp_initial_queue_occ : coverpoint initial_queue_occ_class {
        bins empty    = {OCC_EMPTY};
        bins low      = {OCC_LOW};
        bins mid      = {OCC_MID};
        bins high     = {OCC_HIGH};
    }

    // Current transaction's injection rate
    cp_curr_rate : coverpoint txn_injection_rate_class {
        bins low        = {RATE_LOW};
        bins medium     = {RATE_MEDIUM};
        bins high       = {RATE_HIGH};
        bins saturating = {RATE_SATURATING};
    }

    // Did the current transaction experience queue overflow?
    cp_curr_overflow : coverpoint txn_had_queue_overflow {
        bins no  = {1'b0};
        bins yes = {1'b1};
    }

    x_residual_queue : cross cp_initial_queue_occ, cp_curr_rate, cp_curr_overflow;

endgroup
```

| Field | Detail |
|-------|--------|
| **Why** | Without reset between transactions, queue state persists. A previous transaction that left 200 entries in the queue (drained slowly) combined with a new transaction at saturating rate would overflow the queue almost immediately. This cross catches: queue overflow counting errors when starting non-empty, drain interleaving bugs between old and new transaction hits, occupancy_max tracking across transaction boundaries. |
| **Bins** | 4 x 4 x 2 = 32 |
| **Exclusions** | `initial_queue=empty` with `curr_rate=low` and `overflow=yes` is very unlikely. Not excluded (could happen with bin_width=0 corner). |
| **Priority** | P2 |
| **Target** | 85% |

### 3.5 T05: Config Transition x Pipeline Drain Cross (P1)

```systemverilog
covergroup cg_txn_config_drain @(posedge txn_complete);

    // Did a config apply happen between transactions?
    cp_config_applied : coverpoint config_applied_between_txns {
        bins no  = {1'b0};
        bins yes = {1'b1};
    }

    // Were there in-flight hits when apply was requested?
    cp_pipeline_active_at_apply : coverpoint pipeline_had_inflight_at_apply {
        bins empty  = {1'b0};
        bins active = {1'b1};
    }

    // How long did cfg_apply_pending stay high? (cycles)
    cp_apply_duration : coverpoint apply_pending_duration_class {
        bins zero    = {DUR_ZERO};        // 0 cycles (no apply)
        bins short   = {DUR_SHORT};       // 1-10 cycles
        bins medium  = {DUR_MEDIUM};      // 11-100 cycles
        bins long    = {DUR_LONG};        // 100+ cycles
    }

    x_config_drain : cross cp_config_applied, cp_pipeline_active_at_apply,
                           cp_apply_duration;

endgroup
```

| Field | Detail |
|-------|--------|
| **Why** | The apply mechanism waits for `ingress_stage_valid` to be all-zero before copying shadow config to active config. If the pipeline has in-flight hits (FIFO non-empty, arbiter holding data, divider pipeline active), the apply may stall for many cycles. This cross catches: apply fires while pipeline is non-empty (ingress_empty_v logic bug), config partially applied, apply hangs indefinitely (livelock when FIFO keeps refilling). |
| **Bins** | 2 x 2 x 4 = 16 |
| **Exclusions** | `config_applied=no` with `apply_duration != zero` is impossible. |
| **Priority** | P1 |
| **Target** | 95% |

### 3.6 T06: Interval Boundary Cross (P2)

```systemverilog
covergroup cg_txn_interval @(posedge txn_complete);

    // Number of interval pulses during this transaction
    cp_interval_count : coverpoint txn_interval_pulse_count {
        bins zero  = {0};
        bins one   = {1};
        bins multi = {[2:$]};
    }

    // Transaction hit count
    cp_hit_count_class : coverpoint txn_hit_count_class {
        bins zero  = {HC_ZERO};
        bins low   = {HC_LOW};
        bins high  = {HC_HIGH};
    }

    // Were stats counters read back during this transaction?
    cp_stats_read : coverpoint txn_stats_read_back {
        bins no  = {1'b0};
        bins yes = {1'b1};
    }

    x_interval_txn : cross cp_interval_count, cp_hit_count_class, cp_stats_read;

endgroup
```

| Field | Detail |
|-------|--------|
| **Why** | Stats counters reset on each interval pulse. If a transaction spans multiple intervals, the counters reset mid-transaction and readback values depend on timing. This cross catches: stats read returning mid-reset value, total_hits showing only partial count (from current interval), multiple resets accumulating an incorrect "net" count. |
| **Bins** | 3 x 3 x 2 = 18 |
| **Exclusions** | None. |
| **Priority** | P2 |
| **Target** | 90% |

---

## 4. Coverage Targets and Exclusion Summary

### 4.1 Default Target

All crosses default to **95%** coverage unless specifically overridden below.

### 4.2 Per-Cross Target Overrides

| Cross | Target | Rationale |
|-------|--------|-----------|
| X03 (binning) | 90% | Large cross product (400 bins). Some bin_width x left_bound x key_region corners require very specific stimulus. |
| X04 (port x key) | 90% | Port exclusions reduce effective bins; remaining are reachable. |
| X05 (throughput) | 85% | Queue-full + saturating-rate needs precise timing control. |
| X07 (bank swap) | 100% | Only 8 bins, all safety-critical. |
| X08 (FIFO stress) | 85% | FIFO-full under stress is timing-dependent. |
| X09 (div result) | 85% | Some bin_width x bin_index combinations unreachable. |
| X12 (burst conflict) | 100% | Only 8 bins, all required for read correctness. |
| X16 (swap burst) | 100% | Only 4 bins, critical for data integrity. |
| T02 (bank swap data) | 85% | 54-bin cross with timing-dependent late/early hits. |
| T04 (residual queue) | 85% | Residual state depends on previous transaction drain time. |

### 4.3 Illegal/Impossible Bins

| Cross | Illegal Combination | Reason |
|-------|---------------------|--------|
| X05 | `injection_rate=idle` x any `hit_count > 0` | Idle means 0 hits by definition |
| X05 | `hit_count=single` x `queue_occ=full` | 1 hit cannot fill 256-deep queue |
| X13 | `apply_pending=0` x `hits_during_apply=present` | Cannot have hits during apply if not pending |
| X15 | `mode=debug` x `port_index > 0` | Debug routes through port 0 only |
| X18 | `kick_saturated=yes` x `hit_count=single` | Need 256+ same-bin hits for saturation |
| X18 | `kick_saturated=yes` x `coalesced=no` | Saturation requires coalescing by definition |
| X20 | `underflow=yes` x `overflow=yes` (per hit) | Mutually exclusive per divider output |
| T05 | `config_applied=no` x `apply_duration != zero` | No apply means zero pending time |

---

## 5. Long-Running Test Strategy

### 5.1 Test: `hist_coverage_soak_test`

A single UVM test that runs 100K+ transactions without DUT reset. Each transaction
randomly selects a configuration and stimulus profile, injects hits, and optionally
reads back results for scoreboard comparison.

### 5.2 Transaction Generator Architecture

```
┌──────────────────────────────────────────────────────┐
│              hist_coverage_vseq (virtual sequence)     │
│                                                        │
│  loop (100_000+ iterations):                           │
│    1. Randomly select transaction_type:                │
│       - CONFIG_ONLY    (10% weight)                    │
│       - SINGLE_PORT    (25% weight)                    │
│       - MULTI_PORT     (25% weight)                    │
│       - STRESS         (10% weight)                    │
│       - READBACK       (10% weight)                    │
│       - CLEAR          (5% weight)                     │
│       - CONFIG_AND_HIT (10% weight)                    │
│       - DEBUG_MODE     (5% weight)                     │
│                                                        │
│    2. Generate transaction parameters:                 │
│       - Config: randomize mode, bounds, bin_width,     │
│         filter settings, key extraction bits           │
│       - Stimulus: randomize port(s), hit count,        │
│         injection rate, key distribution               │
│       - Readback: optionally issue burst read and/or   │
│         CSR reads                                      │
│                                                        │
│    3. Execute transaction:                             │
│       a. If config changed: write CSRs, apply config   │
│       b. Wait for apply_pending to clear               │
│       c. Inject hits (parallel on active ports)        │
│       d. Wait for pipeline drain                       │
│       e. Optionally issue burst read + CSR reads       │
│       f. Signal txn_complete to coverage sampler       │
│                                                        │
│    4. Record transaction properties for T0x crosses    │
│       (store as prev_txn for next iteration)           │
└──────────────────────────────────────────────────────┘
```

### 5.3 Transaction Types

| Type | Description | Coverage Goals |
|------|-------------|---------------|
| CONFIG_ONLY | Write new CSR config, apply, no hits | T01, T05 (config transition), X01, X02, X10 |
| SINGLE_PORT | Hits on one port only | X04, X09, X10, X11, X15 |
| MULTI_PORT | Hits on 2-8 ports simultaneously | X04, X08, X05 |
| STRESS | Maximum rate hits on all 8 ports, 1000+ hits | X05, X06, X08, X17, X18 |
| READBACK | Burst read + CSR stat reads (no new hits) | X12, X16, T06 |
| CLEAR | Issue measure_clear between hit phases | T03, X14 |
| CONFIG_AND_HIT | Change config then immediately inject hits | T01, T05, X03, X13 |
| DEBUG_MODE | Set negative mode, inject via debug interface | X01, X15, X02 |

### 5.4 Weighted Randomization Strategy

The virtual sequence uses coverage-driven feedback to bias randomization toward
uncovered bins. After every 1000 transactions, the sequence queries the coverage
database and adjusts weights:

```systemverilog
// Pseudocode for coverage-driven weight adjustment
task adjust_weights();
    real cov;
    foreach (cross_list[i]) begin
        cov = cross_list[i].get_coverage();
        if (cov < 50.0)
            cross_list[i].weight *= 2;     // Double weight for undercovered crosses
        else if (cov > 90.0)
            cross_list[i].weight /= 2;     // Halve weight for well-covered crosses
    end
endtask
```

### 5.5 Pipeline Drain Protocol

Between transactions, the test must drain the pipeline to ensure the scoreboard
comparison is against a quiescent state. The drain sequence:

1. Stop all hit injection (deassert all port valid signals).
2. Wait for all `fifo_empty` signals to assert (max ~16 cycles per port, serialized
   by arbiter = up to 8*16 = 128 cycles).
3. Wait for `divider_valid` to pulse then go idle (bin_divider pipeline latency is
   BIN_INDEX_WIDTH + 3 = 11 cycles, plus queue_hit_pipe = 1 cycle = 12 cycles).
4. Wait for `queue_occupancy == 0` (drain all coalesced hits to pingpong_sram).
5. Wait for `upd_write_valid` to go idle (pingpong_sram write pipeline = 4 stages).
6. Total worst-case drain: 128 + 12 + 256 + 4 = 400 cycles.

The test adds a 50-cycle guard margin, waiting 450 cycles total after the last
hit injection before considering the transaction complete.

### 5.6 Scoreboard Integration

For each transaction, the scoreboard:

1. **On config apply**: Updates shadow config, resets per-interval reference model.
2. **On hit injection**: Computes expected key, applies filter, computes bin index
   using reference divider, increments `ref_bins[bin]`. Tracks expected drops
   (FIFO full model) and expected underflow/overflow counts.
3. **On burst read**: Compares each word against `ref_bins[]`. Reports mismatch
   as UVM_ERROR.
4. **On CSR read**: Compares stat counters against reference counters.
5. **On measure clear**: Resets all reference state to zero.
6. **On interval pulse**: Resets per-interval counters. In pingpong mode, the
   reference model swaps banks (marks old reference as frozen for read comparison).

### 5.7 Run Time Estimation

At 125 MHz simulation clock, assuming average 50 cycles per transaction:

- 100K transactions x 50 cycles = 5M cycles = 40 ms simulation time
- Questa FSE performance: ~1M cycles/sec with +acc => ~5 seconds wall time
- With coverage collection: ~10 seconds wall time
- With full UVM overhead: ~30-60 seconds wall time

Target: complete 100K-transaction soak in under 5 minutes wall time.

### 5.8 Seed Management

Run the soak test with multiple seeds to improve coverage:

```makefile
SEEDS = 1 42 12345 99999 314159 271828 161803 577215 141421 173205

soak_all: $(addprefix soak_seed_,$(SEEDS))

soak_seed_%: compile
    $(VSIM) -c -work $(WORK) -t ps -voptargs=+acc \
      -suppress 19 -suppress 3009 -suppress 3473 \
      -coverage \
      +UVM_TESTNAME=hist_coverage_soak_test -sv_seed $* \
      +NUM_TXNS=100000 \
      -do "coverage save -onexit soak_$*.ucdb; run -all; quit -f" $(TB_TOP)
```

After running all seeds, merge coverage:

```bash
vcover merge soak_merged.ucdb soak_*.ucdb
vcover report -html -output soak_cov_html soak_merged.ucdb
```

### 5.9 Coverage Closure Checklist

| Step | Action |
|------|--------|
| 1 | Run initial 100K-txn soak with 10 seeds |
| 2 | Merge coverage, identify holes |
| 3 | Write targeted sequences for uncovered bins (e.g., bin_width=0 + large_neg left_bound + overflow key) |
| 4 | Add targeted sequences to the soak test weight table |
| 5 | Re-run with additional 5 seeds |
| 6 | Repeat steps 2-5 until all crosses meet their targets |
| 7 | Final report: list any waived bins with justification |

---

## 6. Appendix: Summary of All Crosses

| ID | Name | Coverpoints | Bins | Priority | Target |
|----|------|-------------|------|----------|--------|
| X01 | Mode x Key Unsigned | cp_mode, cp_key_unsigned | 16 | P1 | 95% |
| X02 | Filter x Mode | cp_filter_enable, cp_filter_reject, cp_mode | 32 | P1 | 95% |
| X03 | Binning | cp_bin_width, cp_left_bound, cp_key_region | 400 | P1 | 90% |
| X04 | Port x Key x Active Ports | cp_port_index, cp_key_region, cp_active_ports | 320 | P2 | 90% |
| X05 | Throughput | cp_hit_count, cp_injection_rate, cp_queue_occ | 180 | P1 | 85% |
| X06 | Backpressure | cp_queue_occ, cp_queue_overflow, cp_dropped | 24 | P1 | 95% |
| X07 | Bank Swap | cp_active_bank, cp_flushing, cp_hits_during_swap | 8 | P1 | 100% |
| X08 | FIFO Stress | cp_fifo_max_any, cp_hit_count, cp_active_ports | 150 | P2 | 85% |
| X09 | Divider Result | cp_bin_width, cp_bin_index | 70 | P1 | 85% |
| X10 | Key Sign | cp_key_unsigned, cp_key_region, cp_left_bound | 80 | P1 | 90% |
| X11 | Filter Throughput | cp_filter_enable, cp_key_region, cp_hit_count | 96 | P2 | 90% |
| X12 | Burst Conflict | cp_burst_during_update, cp_active_bank, cp_flushing | 8 | P2 | 100% |
| X13 | Apply Block | cp_apply_pending, cp_hits_during_apply, cp_hit_count | 24 | P1 | 90% |
| X14 | Clear Mid-Flight | cp_clear_during_pipeline, cp_queue_occ, cp_fifo_max_any | 60 | P2 | 85% |
| X15 | Mode x Port | cp_mode, cp_port_index, cp_key_distribution | 88* | P2 | 90% |
| X16 | Swap Burst Race | cp_swap_during_burst, cp_active_bank | 4 | P1 | 100% |
| X17 | Coalescing | cp_coalesced, cp_queue_occ, cp_key_distribution | 48 | P2 | 85% |
| X18 | Kick Saturation | cp_kick_saturated, cp_hit_count, cp_coalesced | 24 | P2 | 90% |
| X19 | Key Extraction | cp_key_field_width, cp_key_unsigned, cp_bin_width | 100 | P2 | 85% |
| X20 | Boundary Result | cp_underflow, cp_overflow, cp_bin_index | 28 | P1 | 95% |
| T01 | Config Change | prev/curr bin_width, mode, filter | 32 | P1 | 95% |
| T02 | Bank Swap Data | swap_occurred, late/early hits, bin total | 54 | P1 | 85% |
| T03 | Clear Between Txns | clear_issued, prev queue/FIFO occ, result check | 26 | P1 | 95% |
| T04 | Residual Queue | initial_queue_occ, injection rate, overflow | 32 | P2 | 85% |
| T05 | Config Drain | config_applied, pipeline_active, duration | 16 | P1 | 95% |
| T06 | Interval Boundary | interval_count, hit_count, stats_read | 18 | P2 | 90% |

\* After removing ignore_bins for debug mode x non-port0.

**Total estimated cross bins: ~1,934**
**P1 crosses: 14 (X01-X03, X05-X07, X09-X10, X13, X16, X20, T01-T03, T05)**
**P2 crosses: 12 (X04, X08, X11-X12, X14-X15, X17-X19, T04, T06)**
