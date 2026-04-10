# histogram_statistics_v2 DV -- Edge Cases

**Parent:** DV_PLAN.md
**ID Range:** E001-E999
**Total:** 172 cases
**Method:** All directed (D)

These tests exercise near-boundary conditions across the full histogram_statistics_v2 data path: bin divider arithmetic limits, FIFO depth fill, coalescing queue saturation, ping-pong SRAM bank-swap timing, config-apply interlocks, statistics counter saturation, CSR access ordering, interval/clear race conditions, and filter field extraction at bit-width extremes. Each case is deliberately close to a failure boundary and must pass without hang, data corruption, or silent miscounting.

---

## 1. Bin Divider Arithmetic Boundaries (BDA) -- 18 cases

The `bin_divider` computes `bin_index = (key - left_bound) / bin_width` via unsigned subtraction of signed values followed by a BIN_INDEX_WIDTH=8 stage restoring divider. These cases probe the arithmetic boundaries of this pipeline.

| ID | Scenario | Stimulus | Checker | RTL Ref |
|----|----------|----------|---------|---------|
| E001 | Key exactly at left_bound (delta=0, bin_index must be 0) | `i_key = left_bound`, any valid `bin_width >= 1` | `o_bin_index == 0`, `o_underflow == 0`, `o_overflow == 0` | bin_divider.vhd:102,109 |
| E002 | Key at left_bound + bin_width - 1 (last key that maps to bin 0) | `i_key = left_bound + bin_width - 1` | `o_bin_index == 0`; would be 1 if off-by-one in the restoring compare | :134 |
| E003 | Key at left_bound + bin_width (first key of bin 1) | `i_key = left_bound + bin_width` | `o_bin_index == 1` exactly; catches `>=` vs `>` bug in quotient stage | :134 |
| E004 | Key at right_bound - 1 (last in-range key, bin N_BINS-1 or below) | `i_key = right_bound - 1` with `right_bound = left_bound + bin_width * N_BINS` | `o_overflow == 0`; `o_bin_index == N_BINS - 1` | :104 |
| E005 | Key at exactly right_bound (first overflow key) | `i_key = right_bound` | `o_overflow == 1`; the range check uses `>=` so this must overflow | :104 |
| E006 | Key at left_bound - 1 (first underflow key) | `i_key = left_bound - 1` | `o_underflow == 1` | :102 |
| E007 | bin_width = 1 (every delta maps to its own bin) | `left_bound=0, bin_width=1, key=0..255` sweep | `o_bin_index == key - left_bound` for each; division-by-1 must produce identity | :131 |
| E008 | bin_width = N_BINS (single bin covers entire range) | `left_bound=0, bin_width=256, key=0..255` | `o_bin_index == 0` for all 256 keys; quotient always 0 with remainder < 256 | :131,134 |
| E009 | bin_width = 0 (degenerate: division by zero) | `bin_width=0`, valid key in `[left_bound, right_bound)` | `o_overflow == 1` (special-cased overflow); no hang or undefined state | :106-107 |
| E010 | Remainder exactly 0 (delta is exact multiple of bin_width) | `left_bound=0, bin_width=16, key=48` => delta=48, quotient=3, remainder=0 | `o_bin_index == 3`; verifies no off-by-one when remainder vanishes at every stage | :134-136 |
| E011 | Remainder = bin_width - 1 (largest possible remainder) | `left_bound=0, bin_width=16, key=31` => delta=31, quotient=1, remainder=15 | `o_bin_index == 1`; remainder must not round up to next bin | :134 |
| E012 | Maximum bin_index = 255 (quotient all-ones) | `left_bound=0, bin_width=1, key=255` | `o_bin_index == 255` (0xFF); all 8 quotient bits set; restoring loop must set bit_idx 7 down to 0 | :129,136 |
| E013 | delta_v unsigned subtraction when key and left_bound have different signs | `left_bound = -100 (signed), key = +100 (signed)` => unsigned subtraction of two's complement values yields delta=200 | `o_bin_index == 200 / bin_width`; catches unsigned(signed) subtraction bug | :109 |
| E014 | Both key and left_bound at signed maximum (+2^31-1) | `key = 0x7FFFFFFF, left_bound = 0x7FFFFFFF` => delta=0 | `o_bin_index == 0`; verifies no overflow in unsigned cast | :109 |
| E015 | Both at signed minimum (-2^31) | `key = 0x80000000, left_bound = 0x80000000` => delta=0 | `o_bin_index == 0`; unsigned subtraction of identical values yields 0 | :109 |
| E016 | Key at signed minimum, left_bound at signed maximum (max negative delta) | `key = -2^31, left_bound = 2^31-1` | `o_underflow == 1` because key < left_bound in signed comparison | :102 |
| E017 | Key at signed maximum, left_bound at signed minimum (max positive delta) | `key = 2^31-1, left_bound = -2^31` => unsigned delta = 0xFFFFFFFF | `o_bin_index == 0xFFFFFFFF / bin_width`, clipped to 8 bits; if in range, correct index; if exceeds N_BINS, overflow | :109,134 |
| E018 | Pipeline flush: i_clear during active divider stages | Inject 3 valid keys then assert `i_clear` while stages 2-4 are active | All pipeline registers clear to 0; `o_valid` never asserts for in-flight entries after clear | :84-91 |

---

## 2. Signed/Unsigned Key Extraction Boundaries (SKE) -- 14 cases

The `build_key` function extracts a bit field from the AVST data word and interprets it as signed or unsigned, then resizes to SAR_TICK_WIDTH=32 bits. Boundaries probe MSB/LSB of the data word, sign-extension, and zero-crossing.

| ID | Scenario | Stimulus | Checker | RTL Ref |
|----|----------|----------|---------|---------|
| E019 | Unsigned key extraction: key field at data word bits [38:35] (FILTER_KEY default) | Data word with bits 38:35 = 0xF, all other bits = 0 | Extracted unsigned key = 15; no sign extension into upper bits | histogram_statistics_v2.vhd:362-367 |
| E020 | Unsigned key extraction: key field at data word bits [0:0] (single LSB) | `cfg_update_key_low=0, cfg_update_key_high=0`, data[0]=1 | Key = 1 (zero-extended to 32 bits); verifies single-bit extraction | :362-367 |
| E021 | Unsigned key extraction: key field at data word MSB [38:38] | `cfg_update_key_low=38, cfg_update_key_high=38`, data[38]=1 | Key = 1; verifies extraction at top-of-word boundary | :362-367 |
| E022 | Signed key extraction: max positive (key field all zeros except sign=0) | `cfg_key_unsigned=0`, SAR_KEY_WIDTH=16 bit field = 0x7FFF | Signed result = +32767, resized to +32767 in 32-bit signed | :369-374 |
| E023 | Signed key extraction: max negative (sign bit = 1, rest zeros) | Signed field = 0x8000 (16-bit) | Result = -32768, sign-extended to 32-bit = 0xFFFF8000 | :369-374 |
| E024 | Signed key extraction: -1 (all ones) | Field = 0xFFFF | Result = -1, sign-extended to 0xFFFFFFFF | :369-374 |
| E025 | Signed key extraction: zero crossing (field = 0x0000) | Signed field = 0 | Result = 0; no spurious sign extension | :369-374 |
| E026 | Unsigned key: field spans bit_lo=0 to bit_hi=31 (full 32-bit extraction) | `key_lo=0, key_hi=31`, data = 0xDEADBEEF | Key = 0xDEADBEEF zero-extended; verifies full-width extraction | :362-367 |
| E027 | Key field where bit_hi > AVST_DATA_WIDTH-1 (out-of-range high bit) | `cfg_update_key_high=40` on 39-bit data word | Bits above data'high read as 0; extract_unsigned pads zeros for out-of-range | histogram_statistics_v2_pkg.vhd:106 |
| E028 | Key field where bit_lo = bit_hi (single-bit key) | `key_lo=17, key_hi=17`, data[17]=1 | Key = 1; verifies (bit_hi - bit_lo) == 0 loop terminates correctly | :104-110 |
| E029 | Port offset addition: port=7 with CHANNELS_PER_PORT=32 adds 224 | Inject key=10 on port 7 | `divider_in_key == 10 + 7*32 = 234`; catches port_offset truncation | histogram_statistics_v2.vhd:697-698 |
| E030 | Port offset wrapping: key near SAR_TICK_WIDTH max + port offset | key = 0x7FFFFFF0 on port 7 => key + 224 = 0x800000D0 (signed overflow to negative) | `divider_in_key` is the arithmetically correct signed value; bin_divider handles the sign | :697-698 |
| E031 | Debug mode key: 16-bit signed data = 0x8000 (-32768) with mode=-1 | `cfg_mode = -1`, `debug_data = 0x8000` | Key = sign-extended to -32768 in 32 bits | :386-388 |
| E032 | Debug mode key: 16-bit unsigned data = 0xFFFF with mode=0 (non-debug path) | `cfg_mode = 0`, regular port data | Normal build_key path used, not build_debug_key; cfg_mode >= 0 means normal | :507-528 |

---

## 3. FIFO Depth Boundaries (FDB) -- 14 cases

Each per-port `hit_fifo` has depth 2^FIFO_ADDR_WIDTH = 16 entries. The FIFO uses pointer comparison for level tracking. These cases probe fill/empty boundaries and pointer wrap.

| ID | Scenario | Stimulus | Checker | RTL Ref |
|----|----------|----------|---------|---------|
| E033 | FIFO exactly full: 16 writes with no reads | Inject 16 valid hits on one port, stall downstream | `o_full == 1`, `o_level == 16`, `o_empty == 0` | hit_fifo.vhd:95,112 |
| E034 | FIFO at 15 (one below full) | 15 writes, no reads | `o_full == 0`, `o_level == 15`; one more write still accepted | :95 |
| E035 | FIFO overflow attempt: 17th write when full | 16 writes then 1 more while full | 17th write silently dropped (write_v=false because level_v >= FIFO_DEPTH); `o_level` stays 16 | :95 |
| E036 | FIFO empty read (underflow guard) | Issue `i_read=1` when `o_empty=1` | read_v=false (level_reg /= 0 fails); rd_ptr unchanged; no garbage output | :94 |
| E037 | Simultaneous write and read at exactly full (16) | At level=16, assert both `i_write=1` and `i_read=1` | Read proceeds (level > 0), write re-accepted (level drops to 15 then back to 16); net level stays 16 | :94-106 |
| E038 | Simultaneous write and read at exactly empty (0) | At level=0, assert both `i_write=1` and `i_read=1` | Only write proceeds (read blocked by level=0); level becomes 1 | :94-95 |
| E039 | Pointer wrap: write 16 entries, read 16, write 16 more | Two full fill-drain cycles | wr_ptr wraps from 15->0; rd_ptr wraps from 15->0; all 32 entries correct | :98,103 |
| E040 | Single-entry oscillation: write 1, read 1, repeat 100x | 100 push-pop cycles | No pointer drift; level returns to 0 each time; o_level_max == 1 | :113-114 |
| E041 | Peak level tracking: fill to 10, drain to 5, fill to 12, drain all | Non-monotonic fill pattern | `o_level_max == 12` (tracks peak across entire lifetime) | :113-114 |
| E042 | Clear while partially full: level=8, assert i_clear | 8 entries in FIFO, then clear | `o_level == 0, o_empty == 1, o_full == 0, o_level_max == 0`; pointers reset | :83-88 |
| E043 | Write during clear cycle (same clock edge) | Assert `i_clear=1` and `i_write=1` on same cycle | Reset takes priority (rst/clear branch); write is ignored | :82-83 |
| E044 | FIFO read data is combinational (showahead style) | Write one entry, check `o_read_data` on the next cycle (after pointer update) | Data appears at `mem(rd_ptr)` combinationally; rd_ptr must have been updated first | :68 |
| E045 | All 8 FIFOs full simultaneously | Drive all 8 ports with 16 hits each, stall arbiter | All 8 `fifo_full` signals asserted; `drop_pulse` fires for any further hits on each port | histogram_statistics_v2.vhd:601-602 |
| E046 | Drop pulse fires exactly once per rejected hit when FIFO full | Fill FIFO to 16, inject 3 more hits | `drop_pulse` asserts for exactly 3 cycles; `csr_dropped_hits` increments by 3 | :601-602,857 |

---

## 4. Coalescing Queue Boundaries (CQB) -- 18 cases

The `coalescing_queue` maps N_BINS=256 bins into a circular queue of QUEUE_DEPTH=256 entries with per-bin kick counters (KICK_WIDTH=8, KICK_MAX=255). These cases probe queue-full, kick saturation, and simultaneous drain+enqueue races.

| ID | Scenario | Stimulus | Checker | RTL Ref |
|----|----------|----------|---------|---------|
| E047 | Queue at exactly QUEUE_DEPTH (256 distinct bins) | 256 hits to 256 different bins | `queue_level == 256`, `queue_room_c == 0` (no drain active) | coalescing_queue.vhd:101 |
| E048 | Queue at QUEUE_DEPTH with drain_fire active (room freed by drain) | Queue at 256, drain_ready=1, head drains => queue_room_c = 1 because `(drain_fire_c = '1')` term | New hit accepted on same cycle as drain; level stays 256; no overflow counted | :101 |
| E049 | Queue at QUEUE_DEPTH, no drain, new distinct bin hit (overflow) | 257th distinct bin hit when queue full and drain_fire_c=0 | `overflow_count_q` increments by 1; hit is lost | :246-247 |
| E050 | Simultaneous drain and hit to same bin (queued_effective path) | Drain head = bin X, simultaneously hit for bin X | `queued_effective = 0` (drain clears it); bin X re-enqueued as new entry with kick=1 | :124-128,130-138 |
| E051 | Hit to already-queued bin: kick counter increments from 0 to 1 | Bin X already queued with kick=1, second hit to bin X | `kick_ram(X) == 2`; queued_effective=1 so it coalesces | :131-133 |
| E052 | Kick counter at 254, one more hit (254->255) | Bin X with kick=254, one more hit | `kick_ram(X) == 255`; counter reaches KICK_MAX but does not overflow | :132-133 |
| E053 | Kick counter at KICK_MAX (255), one more hit (saturation) | Bin X with kick=255, one more hit | `kick_ram(X)` stays 255 (saturates); `overflow_count_q` increments by 1 | :132,239-241 |
| E054 | Kick counter at KICK_MAX, simultaneous drain | Drain head = bin X (kick=255), hit to bin X same cycle | Drain captures 255; bin X re-enqueued fresh with kick=1; no overflow counted | :124-128,130-138 |
| E055 | Queue pointer wrap: enqueue 256 distinct bins, drain all, enqueue 256 more | Two full queue cycles | `queue_wr_ptr` wraps from 255->0; `queue_rd_ptr` wraps from 255->0; all entries correct | :219,244 |
| E056 | Queue level = 1, drain fires (queue becomes empty) | Single entry in queue, drain_ready=1 | `queue_level == 0`, `queue_head_valid_q == 0`; no stale head | :222-227 |
| E057 | Queue level = 2, drain fires (next head loaded) | Two entries, drain_ready=1 | After drain: `next_head_bin_v = queue_mem(rd_ptr_v)`; head transitions to second entry | :222-224 |
| E058 | Queue empty, hit arrives (first entry) | Empty queue, single hit to bin 5 | `queue_level == 1`, `queue_mem(0) == 5`, `kick_ram(5) == 1`, `queued(5) == 1` | :242-245 |
| E059 | Clear_active blocks drain_fire_c | Assert `i_clear`, then provide `i_drain_ready=1` and `queue_head_valid_q=1` | `drain_fire_c == 0` because `clear_active = 1`; no data drained during clear | :98 |
| E060 | Clear walks through all N_BINS = 256 entries | Assert `i_clear`, count cycles until `clear_active` deasserts | Exactly 256 cycles; `clear_index` counts 0..255; all `kick_ram` and `queued` zeroed | :187-197 |
| E061 | Clear_active blocks incoming hits | Hits arriving while `clear_active=1` | Hits are lost because gen_kick_bins sees `clear_active=1` and only processes clear_index match | :114-118 |
| E062 | Occupancy_max tracks peak then persists after drain | Fill to 200, drain to 50, fill to 180 | `queue_level_max == 200` (first peak); does not update to 180 since 180 < 200 | :258-259 |
| E063 | Overflow counter saturation (16-bit) | Force `overflow_count_q` to 0xFFFE, trigger 2 more overflows | Counter reaches 0xFFFF then saturates via `sat_inc` | :241,247 |
| E064 | Drain output: drain_count_q captures kick_ram value at head | Bin 10 with kick=42, drain fires | `o_drain_count == 42`; verifies head_count_c reads kick_ram combinationally before clear | :100,208-209 |

---

## 5. Ping-Pong SRAM Timing (PPS) -- 18 cases

The `pingpong_sram` has a 4-stage update pipeline (issue -> read -> add -> sum -> write), dual banks, interval timer, and burst-read logic. These cases probe RAW forwarding, bank swap timing, and read/update collision.

| ID | Scenario | Stimulus | Checker | RTL Ref |
|----|----------|----------|---------|---------|
| E065 | Back-to-back updates to same bin (RAW hazard, 1-cycle apart) | Update bin 0 on cycle T, update bin 0 on cycle T+1 | Pipeline forwarding: `upd_add` stage matches `upd_read` bin; second read sees forwarded value, not stale RAM | pingpong_sram.vhd:436-443 |
| E066 | Back-to-back updates to same bin (2-cycle gap, sum-stage forward) | Update bin 0 on T, then bin 0 on T+2 | `upd_sum` stage matches; forwarded value from sum pipeline stage | :439 |
| E067 | Back-to-back updates to same bin (3-cycle gap, write-stage forward) | Update bin 0 on T, then bin 0 on T+3 | `upd_write` stage matches; forwarded value from write pipeline stage | :441 |
| E068 | Back-to-back updates to same bin (4-cycle gap, no forward needed) | Update bin 0 on T, then bin 0 on T+4 | No forwarding; RAM read returns the written value from first update (1-cycle RAM latency + write committed) | :434-435 |
| E069 | Bank swap at exact timer boundary (timer_count = interval_clocks - 1) | Set `i_interval_clocks = 100`, let timer reach 99 | `interval_pulse` fires on cycle 99; `active_bank` flips; `clear_active` asserts for new bank | :330-339 |
| E070 | Update rejected on exact bank-swap cycle | Provide `i_upd_valid=1` on the cycle where `ram_v_switch_fire=true` | Update NOT accepted (`ram_v_accept_update` is false due to `not ram_v_switch_fire`) | :421 |
| E071 | Update accepted one cycle after bank swap (clear still active) | `i_upd_valid=1` on cycle after swap | `upd_ready_int = 0` because `clear_active = 1`; update not accepted until clear finishes | :138-140 |
| E072 | Clear completion timing: clear_both on reset (clears both banks) | Assert `i_rst`, release, count cycles until `clear_active` deasserts | 2 * N_BINS = 512 cycles (bank 0: 256 cycles, bank 1: 256 cycles); `clear_both` flip at midpoint | :350-356 |
| E073 | Clear completion timing: clear_bank on swap (clears new active only) | Trigger bank swap via interval timer | N_BINS = 256 cycles for single-bank clear; `clear_both = 0` | :337-339,349 |
| E074 | Burst read deferred when update pipeline active (non-pingpong mode) | `i_enable_pingpong=0`, start update, then issue `i_hist_read=1` | Burst read pended (`hist_read_pending=1`) until all `upd_*_valid` signals and `i_upd_valid` are 0 | :375-381 |
| E075 | Burst read during active update (pingpong mode) | `i_enable_pingpong=1`, active update to bank A, burst read to frozen bank B | Burst read starts immediately (reads different bank); no pipeline conflict | :369-374 |
| E076 | Burst read with burstcount = 0 (minimum treated as 1) | `i_hist_burstcount = 0` | Internally treated as 1 (`if ram_v_burst_count = 0 then ... := 1`); exactly 1 `readdatavalid` | :365-367 |
| E077 | Burst read with burstcount = N_BINS (full histogram readout) | `i_hist_burstcount = 255` (max for 8-bit address) | 255 `readdatavalid` pulses; burst_addr wraps correctly; all bins read | :412-418 |
| E078 | Burst read bank latching in pingpong mode | Bank swap during burst read | `read_bank_latched` was set at burst start; read continues from frozen bank despite swap | :385-388 |
| E079 | Saturating bin count: update count that would overflow 32-bit counter | Bin at 0xFFFFFFFF, update with count=1 | `sat_add(0xFFFFFFFF, 1) = 0xFFFFFFFF`; count saturates, does not wrap to 0 | :461 |
| E080 | Bin count accumulation: 255 updates of count=1 to same bin | 255 individual drain events to same bin | Bin count = 255; verifies repeated read-modify-write with forwarding | :436-461 |
| E081 | hist_readdatavalid is 2-cycle latency from burst issue | Issue burst read, measure cycle count to first `o_hist_readdatavalid` | Exactly 2 cycles: issue -> hist_issue_valid -> hist_read_valid -> readdatavalid | :425-429 |
| E082 | Pending burst read fires after update pipeline drains | `hist_read_pending=1`, then last update drains | Burst starts on next cycle after all `upd_*_valid` deassert and `i_upd_valid=0` | :395-401 |

---

## 6. Config Apply Timing (CAT) -- 14 cases

The `cfg_apply_pending` flag gates all `ingress_accept` signals. Apply completes when all `ingress_stage_valid` bits are 0 (pipeline drained). These cases probe the interlock timing.

| ID | Scenario | Stimulus | Checker | RTL Ref |
|----|----------|----------|---------|---------|
| E083 | Apply with 0 hits in ingress pipeline (immediate apply) | Write CSR[0] with apply bit=1, no hits in flight | `cfg_apply_pending` asserts for 1 cycle (ingress_empty_v is already true), deasserts next cycle; cfg updated | histogram_statistics_v2.vhd:1001-1016 |
| E084 | Apply with 1 hit in ingress_stage (waits 1 cycle) | 1 hit in `ingress_stage_valid`, then apply | `cfg_apply_pending` blocks new ingress; waits until `ingress_stage_valid` clears; apply completes 1 cycle later | :994-1001 |
| E085 | Apply with hits on all 8 ports in ingress_stage (waits for all to drain) | All 8 `ingress_stage_valid` bits = 1, then apply | Apply delayed until all 8 ports' stage registers clear | :994-999 |
| E086 | Apply during measure_clear_pulse (measure_clear clears ingress_stage) | Assert apply, then `measure_clear_pulse` fires | `ingress_stage_valid` forced to 0 by clear; apply can complete immediately | :612-613 |
| E087 | cfg_apply_pending blocks port_ready (backpressure) | Apply pending, port 0 has valid data | `port_ready(0) == 0`; upstream sees backpressure; no data accepted | :534 |
| E088 | Rapid apply-then-hit: hit arrives 1 cycle after apply completes | Apply finishes on cycle T, hit on T+1 | Hit uses new config values (cfg_left_bound, etc.); bin assignment reflects updated bounds | :1003-1016 |
| E089 | Double apply: second apply request while first still pending | Apply pending, second CSR write with apply bit | `cfg_apply_request` pulse is single-cycle; second request is latched but `cfg_apply_pending` already high, so first completes then second is processed | :1017-1019 |
| E090 | Apply with cfg_bin_width = 0 and right_bound <= left_bound (error path) | CSR write: bin_width=0, right_bound < left_bound, then apply | `csr_error=1`, `csr_error_info=0x1`; `cfg_apply_request` NOT asserted; config not updated | :1061-1066 |
| E091 | Apply with cfg_bin_width > 0 (auto-compute right_bound) | CSR write: bin_width=10, left_bound=0, then apply | `csr_right_bound = 0 + 10*256 = 2560`; right_bound auto-computed from bin_width | :1068-1069 |
| E092 | Apply clears csr_error from previous failed apply | First apply fails (bin_width=0, bad bounds), second apply succeeds | `csr_error=0` after successful apply; error flag is cleared on new commit attempt | :1059-1060 |
| E093 | cfg_mode transition from normal (0) to debug (-1) via apply | Write `csr_mode = 0xF` (-1 in 4-bit signed), apply | After apply: `cfg_mode = 0xF`; ingress_comb switches to debug data source on port 0 | :503,507-528 |
| E094 | Apply timing: ingress_stage_valid deasserts on same cycle as apply check | Hit clears from stage on exact cycle that cfg_apply_reg checks ingress_empty_v | Apply completes on that cycle; no 1-cycle delay; verifies combinational check of ingress_stage_valid | :994-1001 |
| E095 | Filter mode changes via apply: enable filter mid-stream | Initially filter disabled, hits flowing; apply enables filter | Hits after apply are filtered; hits before apply (in pipeline) use old config (no filter) | :581-591 |
| E096 | Config apply during bank swap cycle | `cfg_apply_request=1` on same cycle as `interval_pulse` | Both proceed independently; apply blocks ingress, interval resets timer; no deadlock | :1001-1019 |

---

## 7. Statistics Counter Boundaries (SCB) -- 14 cases

The `stats_reg` process uses `sat_add` and `sat_inc` for 32-bit counters. Up to 8 ports can fire accept/drop pulses simultaneously. The `stats_reset_pulse_d1` pipeline adds 1 cycle of latency.

| ID | Scenario | Stimulus | Checker | RTL Ref |
|----|----------|----------|---------|---------|
| E097 | total_hits at 0xFFFFFFFF, 1 more accept (saturation) | Force `csr_total_hits = 0xFFFFFFFF`, inject 1 hit | Counter stays 0xFFFFFFFF; `sat_add` prevents wrap | histogram_statistics_v2.vhd:853-855 |
| E098 | total_hits at 0xFFFFFFF8, 8 accepts in one cycle (max multi-port) | All 8 ports accept simultaneously | `accept_count_v = 8`; `sat_add(0xFFFFFFF8, 8) = 0xFFFFFFFF` (saturates, doesn't wrap to 0) | :844-851,853-855 |
| E099 | dropped_hits at 0xFFFFFFFF, 8 drops in one cycle | All 8 FIFOs full, 8 more hits rejected simultaneously | Counter stays 0xFFFFFFFF | :856-858 |
| E100 | All 8 ports accept on same cycle (accept_count_v = 8) | 8 valid hits, all filters pass, all FIFOs have room | `csr_total_hits += 8` in one cycle; verifies 4-bit accumulator width is sufficient | :844-847 |
| E101 | underflow_count at 0xFFFFFFFF, 1 more underflow (saturation) | Force counter to max, inject key below left_bound | `sat_inc` prevents wrap; counter stays 0xFFFFFFFF | :862 |
| E102 | overflow_count at 0xFFFFFFFF, 1 more overflow | Force counter to max, inject key at right_bound | Counter stays 0xFFFFFFFF | :864 |
| E103 | Stats reset racing with increment: reset and accept on same d1 cycle | `stats_reset_pulse_d1=1` and `accept_stat_pulse_d1(0)=1` on same cycle | Reset takes priority (applied first in process); counter resets to 0 then increments to 1 | :837-842,853 |
| E104 | Stats reset clears all 4 counters atomically | `stats_reset_pulse_d1=1` | `csr_underflow_count=0, csr_overflow_count=0, csr_total_hits=0, csr_dropped_hits=0` all on same cycle | :838-842 |
| E105 | Stats pipeline latency: accept on cycle T, csr_total_hits increments on T+2 | Single accept pulse, sample counter each cycle | T+0: accept_pulse, T+1: accept_stat_pulse_d1, T+2: stats_reg updates | :805,853 |
| E106 | Simultaneous underflow and overflow from different pipeline entries | Two divider results in consecutive cycles: one underflow, one overflow | Both counters increment by 1 each (processed in separate cycles due to pipeline) | :860-865 |
| E107 | Dropped hits increment: FIFO full, valid hit, ingress_stage_write_req=1 | FIFO full for port 3, valid hit arrives | `drop_pulse(3)=1`; `csr_dropped_hits += 1` two cycles later | :601-602,856-858 |
| E108 | Zero accepts over many cycles (counters stay 0) | No valid hits for 1000 cycles | All counters remain 0; no spurious increments from pipeline noise | -- |
| E109 | measure_clear_pulse triggers stats reset via interval_pulse | `interval_pulse=1` on cycle T | `stats_reset_pulse_d1=1` on T+1; all stats counters reset on T+2 | :810 |
| E110 | Stats reset from clear_pulse (write 0x0 to hist_bin) vs interval_pulse | Write 0 to hist_bin slave, check stats reset | `clear_pulse -> measure_clear_comb -> measure_clear_pulse (T+1) -> stats_reset_pulse_d1 (T+2)` | :464-466,810 |

---

## 8. CSR Access Boundaries (CSA) -- 18 cases

CSR reads go through `csr_read_comb` (combinational mux) then `csr_read_reg` (registered). Writes are synchronous. Word 0 = UID (read-only ASCII "HIST"), word 1 = META mux (4 pages selected by write). These cases probe read-write ordering, latency, address-space boundaries, and the identity header.

| ID | Scenario | Stimulus | Checker | RTL Ref |
|----|----------|----------|---------|---------|
| E111 | CSR read latency: data valid 1 cycle after avs_csr_read | Assert `avs_csr_read=1` on cycle T, sample `avs_csr_readdata` | Data valid on T+1 (registered output from `csr_read_reg`) | histogram_statistics_v2.vhd:1162-1171 |
| E112 | CSR read during write to same address (address 3: left_bound) | `avs_csr_write=1` and `avs_csr_read=1` on same cycle, address=3 | Read returns old value (combinational mux samples csr_left_bound before write updates it); write updates register for next read | :1050,1076,1124 |
| E113 | Back-to-back CSR reads to different addresses | Read address 8 on T, address 9 on T+1 | `readdata` on T+1 = underflow_count; `readdata` on T+2 = overflow_count; no stale data | :1162-1170 |
| E114 | CSR read to address 0 (UID register, read-only) | Read address 0 | Returns IP_UID generic = 0x48495354 (ASCII "HIST") | :1120 |
| E114a | CSR write to address 0 (UID) ignored | Write 0xDEADBEEF to address 0, then read address 0 | Read still returns 0x48495354; UID is read-only, write falls into `when others => null` | :1051-1094 |
| E114b | CSR META page 0: VERSION | Write 0 to address 1 (select page 0), read address 1 | Returns `{VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH, BUILD}` packed correctly | :1114-1118 |
| E114c | CSR META page 1: DATE | Write 1 to address 1 (select page 1), read address 1 | Returns VERSION_DATE generic (default 20260410) | :1114-1118 |
| E114d | CSR META page 2: GIT | Write 2 to address 1 (select page 2), read address 1 | Returns VERSION_GIT generic (default 0) | :1114-1118 |
| E114e | CSR META page 3: INSTANCE_ID | Write 3 to address 1 (select page 3), read address 1 | Returns INSTANCE_ID generic (default 0) | :1114-1118 |
| E114f | CSR META selector persistence across reads | Write 2 to address 1, read address 1 twice | Both reads return same value (VERSION_GIT); `csr_meta_sel` is registered, persists until next write to address 1 | :1114-1118 |
| E115 | CSR write to read-only address (version via META page 0) | Write to address 1 sets `csr_meta_sel` only | No effect on actual VERSION/DATE/GIT/INSTANCE_ID values; they are generic constants | :1051-1094 |
| E116 | CSR write to address 16 (scratch register) then read back | Write 0xCAFEBABE to address 16, read address 16 | Read returns 0xCAFEBABE; scratch register is pure read-write | :1092,1156 |
| E117 | CSR address out of range (address 17+ on 5-bit address bus) | Address = 5'd17 or higher | Reads return `when others` default (zeros or last registered value); write has no effect | :1120 |
| E118 | CSR read without asserting avs_csr_read (stale hold) | Read address 2 on T, no read on T+1, sample readdata on T+1 | `csr_readdata_reg` holds last read value (registered, only updates when `avs_csr_read=1`) | :1167 |
| E119 | CSR write to address 2 with apply bit=0 (no commit) | Write control word with bit[0]=0 | Mode and filter fields update in csr_* shadow registers, but `cfg_apply_request` does NOT fire | :1052-1074 |
| E120 | CSR write to address 6: key bit fields packed correctly | Write `0xAABBCCDD` to address 6 | `csr_update_key_low=0xDD, csr_update_key_high=0xCC, csr_filter_key_low=0xBB, csr_filter_key_high=0xAA` | :1082-1085 |
| E121 | CSR read address 11 (bank_status): flushing flag visible during clear | Read address 11 while `clear_active=1` (after reset) | `bank_status[1]=1` (flushing); verifies shadow pipeline captures flushing state | :953-954,1144 |
| E122 | CSR read address 15 (coal_status): queue overflow visible | Read address 15 after coalescing queue overflow | `coal_status[31:16] = overflow_count`; verifies shadow pipeline captures queue overflow | :964,1154 |

---

## 9. Interval/Clear Boundaries (ICB) -- 14 cases

`measure_clear_pulse` (registered) resets ingress stages, FIFOs, arbiter, divider pipeline, and queue. `interval_pulse` triggers bank swap and stats reset. These cases probe their timing interactions.

| ID | Scenario | Stimulus | Checker | RTL Ref |
|----|----------|----------|---------|---------|
| E123 | measure_clear_pulse is 1-cycle delayed from clear_pulse | Write 0x0 to hist_bin on cycle T | `measure_clear_comb=1` on T; `measure_clear_pulse=1` on T+1 (registered) | histogram_statistics_v2.vhd:464-473 |
| E124 | i_interval_reset generates measure_clear_pulse | Assert `i_interval_reset=1` for 1 cycle | `measure_clear_comb=1` same cycle; `measure_clear_pulse=1` next cycle; full pipeline flush | :465 |
| E125 | Back-to-back clears: two writes of 0x0 on consecutive cycles | Write 0x0 on T and T+1 | `measure_clear_pulse` asserted on T+1 and T+2 (two consecutive clear pulses); pipeline flushed twice | :471 |
| E126 | measure_clear during bank swap (interval_pulse on same cycle) | `interval_pulse=1` and `measure_clear_pulse=1` on same cycle | Both effects apply: pingpong_sram clears new bank, queue/FIFOs also clear; no deadlock | :677,735-738,755 |
| E127 | measure_clear with hits in every pipeline stage | Hits in ingress_stage, FIFOs, arbiter, divider, queue | All pipeline stages flushed: `ingress_stage_valid=0`, FIFOs cleared, arbiter cleared, divider cleared, queue cleared | :612,643,663,677,735 |
| E128 | interval_pulse with i_interval_clocks = 1 (fastest possible interval) | Set interval to 1 clock | Timer fires every cycle; bank swaps every cycle; clear never completes (always re-triggered) -- tests whether design handles this gracefully or saturates | pingpong_sram.vhd:330-335 |
| E129 | interval_pulse with i_interval_clocks = 0 (disabled timer) | Set interval to 0 | Timer never fires; `timer_count` stays 0; no bank swap occurs; non-pingpong behavior | :329 |
| E130 | Clear completion then immediate update acceptance | Wait for `clear_active=0` after clear, inject update on next cycle | `upd_ready_int=1` on first cycle after clear deasserts; update accepted | :138-140 |
| E131 | measure_clear_pulse resets peak level trackers in FIFOs | FIFO peak at 12, then `measure_clear_pulse` | `o_level_max = 0` after clear (FIFO i_clear resets max_reg) | hit_fifo.vhd:86 |
| E132 | measure_clear_pulse resets coalescing queue peak occupancy | Queue peak at 200, then `measure_clear_pulse` | `o_occupancy_max = 0` after clear | coalescing_queue.vhd:179 |
| E133 | interval_pulse on exact cycle T, new data on T+1 | Timer fires on T, first post-interval hit on T+1 | Hit enters new active bank (bank flipped); old bank frozen for readout | pingpong_sram.vhd:330-339 |
| E134 | hist_writeresp_valid timing: 1 cycle after avs_hist_bin_write | Assert `avs_hist_bin_write=1` on T | `avs_hist_bin_writeresponsevalid=1` on T+1; response code = 0 | histogram_statistics_v2.vhd:475-484 |
| E135 | measure_clear during cfg_apply_pending | Apply pending (blocking ingress), then clear | Ingress stages already empty (blocked by apply); clear has no visible effect on ingress but resets FIFOs/queue/divider | :612,677 |
| E136 | i_interval_reset held high for multiple cycles | Assert `i_interval_reset=1` for 5 consecutive cycles | `measure_clear_pulse=1` for 5 cycles (registered copy follows input); pipeline stays flushed | :465,471 |

---

## 10. Filter Boundaries (FLT) -- 10 cases

The `match_filter` function extracts a bit field from the data word and compares against `cfg_filter_key`. Filter can be disabled, accept-mode, or reject-mode.

| ID | Scenario | Stimulus | Checker | RTL Ref |
|----|----------|----------|---------|---------|
| E137 | Filter disabled: all hits pass regardless of data content | `cfg_filter_enable=0`, any data word | `match_filter` returns true; all hits reach FIFO | histogram_statistics_v2.vhd:339-340 |
| E138 | Filter accept mode: field matches key exactly | `cfg_filter_enable=1, cfg_filter_reject=0`, field = key | Hit passes filter; `ingress_write_req=1` | :348-350 |
| E139 | Filter accept mode: field differs from key by 1 bit | Field = key XOR 1 | Hit rejected; `ingress_write_req=0`; `accept_pulse` still fires (sampled) but no FIFO write | :589-591 |
| E140 | Filter reject mode: field matches key (hit rejected) | `cfg_filter_reject=1`, field = key | `match_v=true`, function returns `not match_v = false`; hit filtered out | :347-348 |
| E141 | Filter reject mode: field does NOT match key (hit passes) | `cfg_filter_reject=1`, field /= key | Hit passes; filter rejects only exact matches | :347-350 |
| E142 | Filter key at bit field MSB edge: filter_key_high = AVST_DATA_WIDTH-1 = 38 | `filter_hi=38, filter_lo=35`, data[38:35] = filter_key | Extraction at top of data word; catches off-by-one in extract_unsigned range check | histogram_statistics_v2_pkg.vhd:106 |
| E143 | Filter key at bit field LSB edge: filter_key_low = 0 | `filter_lo=0, filter_hi=3`, data[3:0] = filter_key | Extraction at bottom of data word | :106 |
| E144 | Filter with key_lo > key_hi (degenerate configuration) | `cfg_filter_key_low = 10, cfg_filter_key_high = 5` | `extract_unsigned` loop has `bit_hi - bit_lo` underflow; field is all zeros; may always match 0 or never match -- verifies no hang | :106 |
| E145 | Filter key width wider than field (resize truncation) | 4-bit field extracted, compared against 16-bit `cfg_filter_key` with upper bits nonzero | Field resized to 16 bits (zero-extended); upper bits differ from key; hit rejected | :342-345 |
| E146 | Filter field all zeros vs filter key all zeros | Both field and key = 0 | Match; hit passes in accept mode; verifies zero-equality works | :346 |

---

## 11. Round-Robin Arbiter Boundaries (RRA) -- 8 cases

The `rr_arbiter` scans 8 FIFOs in round-robin starting from `last_served + 1`. It excludes the currently output port from re-grant to allow the FIFO read latency.

| ID | Scenario | Stimulus | Checker | RTL Ref |
|----|----------|----------|---------|---------|
| E147 | Single port active: only port 0 has data, all others empty | Port 0 FIFO non-empty; ports 1-7 empty | Arbiter continuously grants port 0; no starvation or hang from single-source | rr_arbiter.vhd:67-76 |
| E148 | All 8 ports active simultaneously: round-robin fairness | All 8 FIFOs have 1 entry each | Each port served exactly once over 8 grant cycles; `last_served` cycles 0->1->...->7 | :66-76 |
| E149 | Port wrap: last_served = 7, next grant starts probing port 0 | `last_served = 7`, port 0 FIFO non-empty | Grant goes to port 0 (wraps via `(7+1) mod 8 = 0`); verifies modular arithmetic | :66 |
| E150 | No ports active (all FIFOs empty) | All `i_fifo_valid = 0` | `grant_valid_c = 0`; `out_valid_q` deasserts; arbiter idles cleanly | :62-76 |
| E151 | Arbiter exclusion: currently output port not re-granted | Port 0 just granted and still valid on output; port 0 FIFO has another entry | Port 0 skipped this cycle (exclusion guard); next non-empty port granted; port 0 served on subsequent cycle after pop completes | :71 |
| E152 | Arbiter clear during active grant | `i_clear=1` while `out_valid_q=1` | `out_valid_q=0, last_served=0`; output invalidated; arbiter restarts from port 0 | :97-101 |
| E153 | Back-to-back grants: pop on T, new data on T+1 | Port 0 popped on T, port 0 FIFO refilled on T+1 (write on same cycle as read) | Arbiter sees port 0 valid again after FIFO processes the write; serves port 0 after full round-robin | :67-76 |
| E154 | Sink not ready: arbiter holds output | `i_sink_ready=0`, multiple ports have data | `load_new_v=false`; output holds; no pop issued; arbiter stalls | :103 |

---

## 12. Ingress Pipeline Boundaries (IPB) -- 6 cases

The ingress pipeline has a combinational stage (`ingress_comb`) followed by a registered stage (`ingress_stage_reg`). `measure_clear_pulse` clears the registered stage. Snoop mode affects port 0 ready logic.

| ID | Scenario | Stimulus | Checker | RTL Ref |
|----|----------|----------|---------|---------|
| E155 | Snoop mode: port 0 ready gated by downstream ready | `SNOOP_EN=true`, `aso_hist_fill_out_ready=0` | `port_ready(0)=0`; port 0 backpressured by snoop output | histogram_statistics_v2.vhd:536-538 |
| E156 | No-snoop mode: port 0 always locally ready | `SNOOP_EN=false` | `port_ready(0)=1` regardless of `aso_hist_fill_out_ready` | :540-542 |
| E157 | ingress_stage_reg cleared by measure_clear_pulse | Hit accepted on T, `measure_clear_pulse=1` on T+1 (while ingress_stage_valid=1) | `ingress_stage_valid` forced to 0; hit lost before reaching FIFO | :612-615 |
| E158 | ingress_stage self-clearing: valid for exactly 1 cycle | Hit accepted on T | `ingress_stage_valid(idx)=1` on T+1, then self-clears to 0 on T+2 (no new accept) | :618-621 |
| E159 | Accept and drop on same port, same cycle (impossible by design) | `ingress_stage_write_req=1`, `fifo_full=1` | Drop takes priority; `drop_pulse=1`; `fifo_write=0`; hit not written to FIFO | :597-603 |
| E160 | accept_pulse fires even when filter rejects hit | Filter rejects hit (write_req=0), but sampled_v=1 | `accept_pulse(idx)=1` (tracks total input); but `ingress_write_req=0` so no FIFO write | :560-591 |

---

## 13. Dual-Port RAM Bypass Logic (DPR) -- 4 cases

The `true_dual_port_ram_single_clock` has bypass logic for mixed-port same-address read-during-write. These cases verify the bypass mux.

| ID | Scenario | Stimulus | Checker | RTL Ref |
|----|----------|----------|---------|---------|
| E161 | Port A read, port B write, same address (bypass to port A) | `we_a=0, we_b=1, addr_a=addr_b=5` | `q_a` returns `data_b` (via bypass); not stale RAM value | true_dual_port_ram_single_clock.vhd:69-74 |
| E162 | Port B read, port A write, same address (bypass to port B) | `we_a=1, we_b=0, addr_a=addr_b=5` | `q_b` returns `data_a` (via bypass) | :76-81 |
| E163 | Both ports write same address (no bypass, both get own write data) | `we_a=1, we_b=1, addr_a=addr_b=5` | `q_a = data_a, q_b = data_b`; last writer wins in RAM; bypass NOT active (both we=1) | :55-67 |
| E164 | Read-read same address (no bypass, both get RAM value) | `we_a=0, we_b=0, addr_a=addr_b=5` | `q_a = q_b = ram(5)`; bypass not active; both read same location | :58-59,65-66 |

---

## 14. Package Helper Function Boundaries (PHF) -- 8 cases

The `histogram_statistics_v2_pkg` provides `sat_inc`, `sat_add`, `extract_unsigned`, `extract_signed`, and `clog2`. These cases verify boundary arithmetic.

| ID | Scenario | Stimulus | Checker | RTL Ref |
|----|----------|----------|---------|---------|
| E165 | sat_inc: value = 0 (minimum) | `sat_inc(x"00000000")` | Returns 0x00000001 | histogram_statistics_v2_pkg.vhd:70-77 |
| E166 | sat_inc: value = 0xFFFFFFFF (saturation) | `sat_inc(x"FFFFFFFF")` | Returns 0xFFFFFFFF; does not wrap to 0 | :73 |
| E167 | sat_add: lhs=0xFFFFFFFF, rhs=1 (saturation) | `sat_add(0xFFFFFFFF, 1)` | Returns 0xFFFFFFFF; carry bit detected | :88 |
| E168 | sat_add: lhs=0x7FFFFFFF, rhs=0x7FFFFFFF (no saturation) | `sat_add(0x7FFFFFFF, 0x7FFFFFFF)` | Returns 0xFFFFFFFE; no carry; correct sum | :87 |
| E169 | sat_add: lhs=0x80000000, rhs=0x80000000 (saturation) | `sat_add(0x80000000, 0x80000000)` | Returns 0xFFFFFFFF; carry bit set | :88 |
| E170 | extract_unsigned: bit_hi = bit_lo (single bit extraction) | `extract_unsigned(data, 5, 5)` where data(5)='1' | Returns 1 in bit 0; all other bits 0 | :104-110 |
| E171 | extract_signed: negative value sign extension | `extract_signed(data, 7, 0)` where data(7)='1' (sign bit) | Returns sign-extended value: bits 7..data'high all '1' | :123-136 |
| E172 | clog2: value = 1 (boundary: log2(1)=0) | `clog2(1)` | Returns 0; `shifted_v = 0` immediately | :51-59 |
