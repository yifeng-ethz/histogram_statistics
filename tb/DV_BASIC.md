# histogram_statistics_v2 DV -- Basic Functional Cases

**ID Range:** B001-B999
**Total:** 150 cases

This document covers bring-up, protocol correctness, and basic feature validation for the `histogram_statistics_v2` multi-port online histogram IP. Every test here must pass before performance, edge, or error tests are meaningful. All tests use minimal stimulus (1-16 hits, 1-4 iterations) unless the scenario specifically requires filling multiple ports or bins.

**Methodology key:**
- **D** = Directed (hand-crafted stimulus)
- **R** = Constrained-random (LCG-based PRNG; no SystemVerilog rand)

---

## 1. Single-Hit Smoke (SH) -- 12 cases

One hit through the entire pipeline. Proves that the data path from AVST ingress through key extraction, hit_fifo, rr_arbiter, bin_divider, coalescing_queue, and pingpong_sram is connected and functional. A failure here means nothing downstream can work.

| ID | Method | Scenario | Iter | Stimulus | Checker |
|----|--------|----------|------|----------|---------|
| B001 | D | Single hit, port 0, key maps to bin 0 | 1 | 1 hit on port 0, key = left_bound (default -1000). Wait for interval swap, read bin 0. | Bin 0 count == 1; all other bins == 0 |
| B002 | D | Single hit, port 0, key maps to last bin (255) | 1 | 1 hit on port 0, key = left_bound + 255*bin_width. Wait for interval swap, read bin 255. | Bin 255 count == 1. Catches off-by-one in right_bound check (key < right_bound, not <=). |
| B003 | D | Single hit, port 0, key maps to mid-range bin (128) | 1 | 1 hit, key = left_bound + 128*16. | Bin 128 count == 1. Verifies divider computes correct quotient for a power-of-two bin index. |
| B004 | D | Single hit, port 7 (last port), key = 0 | 1 | 1 hit on port 7, raw key = 0. Effective key = 0 + 7*32 = 224. | Bin for key 224 has count == 1. Verifies port offset (key += port_index * CHANNELS_PER_PORT) applied to highest port. |
| B005 | D | Single hit, port 0, key exactly equals left_bound | 1 | key = DEF_LEFT_BOUND = -1000 (signed). | Bin 0 count == 1, underflow_cnt == 0. Boundary: left_bound is inclusive. |
| B006 | D | Single hit, port 0, key = right_bound - 1 | 1 | key = left_bound + N_BINS * bin_width - 1. | Highest valid bin has count == 1, overflow_cnt == 0. Boundary: right_bound is exclusive. |
| B007 | D | Single hit, port 0, default config, key = 0 (unsigned extraction) | 1 | 1 hit with data[29:17] = 0. Key = 0. With default left_bound = -1000, key 0 maps to bin 62 (= (0 - (-1000))/16 = 62). | Bin 62 count == 1. Smoke test for unsigned key extraction with default generics. |
| B008 | D | Single hit, port 0, verify snoop passthrough | 1 | 1 hit on port 0 with known data pattern. | aso_hist_fill_out mirrors input data, valid, channel exactly. Proves SNOOP_EN=true path works. |
| B009 | D | Single hit during flush (immediately after reset release) | 1 | Inject 1 hit while pingpong clear_active = '1' (first 256 cycles after reset). | Hit enters FIFO but pingpong deasserts upd_ready; hit queued in coalescer. After flush completes, bin count == 1. No data loss. |
| B010 | D | Single hit, unsigned key, bin_width = 1 | 1 | Configure bin_width = 1, left_bound = 0. 1 hit, key = 100. | Bin 100 count == 1. Verifies divider handles trivial division (quotient = delta). |
| B011 | D | Single hit, signed key extraction (negative key) | 1 | Set csr_key_unsigned = 0, key bits yield -500 (signed). left_bound = -1000. bin = (-500 - (-1000))/16 = 31. | Bin 31 count == 1. Verifies extract_signed sign-extends correctly. |
| B012 | D | Single hit, port 1, verify port offset adds 32 | 1 | 1 hit on port 1, raw key = 10. Effective key = 10 + 1*32 = 42. bin = (42 - (-1000))/16 = 65. | Bin 65 count == 1. Catches bugs where port offset is missing or uses wrong CHANNELS_PER_PORT. |

---

## 2. CSR Access (CSR) -- 22 cases

Read/write every CSR register. The CSR slave has 17 registers addressed by a 5-bit address bus, with a standard identity header at words 0-1. CSR reads have 1-cycle latency (registered output via csr_readdata_reg). A bug in the address decode silently maps writes to the wrong register, corrupting configuration.

| ID | Method | Scenario | Iter | Stimulus | Checker |
|----|--------|----------|------|----------|---------|
| B013 | D | Read UID (addr 0) | 1 | CSR read addr 0 after reset. | Returns 0x48495354 (ASCII "HIST"). Confirms identity header word 0. |
| B013a | D | Write to UID is ignored | 1 | Write 0xDEADBEEF to addr 0, then read back. | Still returns 0x48495354. UID is read-only. |
| B013b | D | Read META page 0 = VERSION (addr 1) | 1 | Write selector 0 to addr 1, then read addr 1. | Returns VERSION_MAJOR=26 in [31:24], VERSION_MINOR=0 in [23:16], patch+build in lower bits. |
| B013c | D | Read META page 1 = DATE (addr 1) | 1 | Write selector 1 to addr 1, then read addr 1. | Returns VERSION_DATE generic value (20260410). |
| B013d | D | Read META page 2 = GIT (addr 1) | 1 | Write selector 2 to addr 1, then read addr 1. | Returns VERSION_GIT generic value (default 0). |
| B013e | D | Read META page 3 = INSTANCE_ID (addr 1) | 1 | Write selector 3 to addr 1, then read addr 1. | Returns INSTANCE_ID generic value (default 0). |
| B014 | D | Read CONTROL (addr 2) at reset default | 1 | CSR read addr 2 after reset. | Returns default: mode=0, key_unsigned per generic, filter_enable=0, apply_pending=0, error=0. |
| B015 | D | Write+read LEFT_BOUND (addr 3) | 1 | Write 0xFFFFFC18 (-1000 in 32-bit signed), readback. | Readback == 0xFFFFFC18. Confirms signed CSR storage. |
| B016 | D | Write+read RIGHT_BOUND (addr 4) | 1 | Write 0x00000F00, readback. | Readback == 0x00000F00. Note: direct write to addr 4 is allowed but apply recalculates right_bound from left_bound + bin_width * N_BINS when bin_width != 0. |
| B017 | D | Write+read BIN_WIDTH (addr 5) | 1 | Write 0x00000020 (32), readback. | Readback == 0x00000020 (lower 16 bits). |
| B018 | D | Write+read KEY_FILTER_BITS (addr 6) | 1 | Write 0x26231D11 (filter_key_high=0x26, filter_key_low=0x23, update_key_high=0x1D, update_key_low=0x11). | Readback == 0x26231D11. Packed byte fields. |
| B019 | D | Write+read KEY_FILTER_VAL (addr 7) | 1 | Write 0x000F000A (filter_key=0x000F, update_key=0x000A). | Readback == 0x000F000A. |
| B020 | D | Read UNDERFLOW_CNT (addr 8) at idle | 1 | No hits injected. | Returns 0. |
| B021 | D | Read OVERFLOW_CNT (addr 9) at idle | 1 | No hits injected. | Returns 0. |
| B022 | D | Write+read INTERVAL_CFG (addr 10) | 1 | Write 0x000F4240 (1000000 clocks). | Readback == 0x000F4240. |
| B023 | D | Read BANK_STATUS (addr 11) at idle | 1 | After reset + flush complete. | active_bank=0, flushing=0 (bit 1=0). |
| B024 | D | Read PORT_STATUS (addr 12) at idle | 1 | No hits in flight. | fifo_empty[7:0] = 0xFF (all empty), fifo_pair_max = 0. |
| B025 | D | Read TOTAL_HITS (addr 13) at idle | 1 | No hits. | Returns 0. |
| B026 | D | Read DROPPED_HITS (addr 14) at idle | 1 | No hits. | Returns 0. |
| B027 | D | Read COAL_STATUS (addr 15) at idle | 1 | No hits in flight. | occupancy=0, occupancy_max=0, overflow=0. |
| B028 | D | Write+read SCRATCH (addr 16) | 1 | Write 0xDEADBEEF, readback. | Readback == 0xDEADBEEF. SCRATCH is the only fully read/write register; catches bus connectivity issues. |

---

## 3. Config Apply Mechanism (CFG) -- 12 cases

The apply flow: host writes CSR shadow registers, then sets bit 0 of CONTROL. This triggers cfg_apply_request, which sets cfg_apply_pending. Pending blocks ingress. When all ingress_stage_valid bits are zero, the shadow copy is committed to the active cfg_* signals. Bugs here cause config to never apply, or to apply while hits are mid-pipeline (corrupting bin mapping).

| ID | Method | Scenario | Iter | Stimulus | Checker |
|----|--------|----------|------|----------|---------|
| B029 | D | Apply with no traffic: immediate commit | 1 | Write left_bound=0, bin_width=10, then CONTROL.apply=1. No hits. | cfg_apply_pending goes high for 1-2 cycles (ingress already empty), then low. Read LEFT_BOUND via CSR: 0. Effective left_bound == 0 on next hit. |
| B030 | D | Apply pending visible in CONTROL readback | 1 | Inject continuous hits on port 0. Write CONTROL.apply=1 while hits flowing. Read CONTROL. | Bit 1 (cfg_apply_pending) == 1 while ingress_stage_valid != 0. |
| B031 | D | Apply blocks ingress (port_ready deasserted) | 1 | Start apply while traffic is flowing. Monitor port_ready signals. | All port_ready[7:0] == 0 while cfg_apply_pending == 1. New data is backpressured, not lost. |
| B032 | D | Apply commits after pipeline drain | 1 | Inject 4 hits on port 0, then immediately apply with new left_bound. Inject 4 more hits after apply completes. | First 4 hits use old config; next 4 use new config. Bin indices differ according to the two left_bound values. |
| B033 | D | Apply with bin_width change: right_bound auto-recalculated | 1 | Set left_bound = 0, bin_width = 8. Apply. | right_bound == 0 + 8 * 256 = 2048. Read CSR addr 2 confirms 2048. |
| B034 | D | Apply with bin_width = 0: right_bound not recalculated | 1 | Set bin_width = 0, manually write right_bound = 500. Apply. | right_bound stays at manually written value (500). bin_width=0 causes overflow for all keys, so overflow_cnt increments on hits. |
| B035 | D | Apply error: bin_width = 0 and right_bound <= left_bound | 1 | Set bin_width = 0, left_bound = 100, right_bound = 50. Apply. | CONTROL.error (bit 24) == 1, error_info (bits 31:28) == 0x1. cfg_apply_request NOT raised; config not committed. |
| B036 | D | Apply does not clear error from previous apply | 1 | Trigger error per B035. Then write valid config, apply. | Error clears on new successful apply (CONTROL.error = 0). |
| B037 | D | Double apply: second apply while first is pending | 1 | Start apply, immediately write CONTROL.apply=1 again before first completes. | Second apply request is ignored (cfg_apply_request is pulsed, but pending is already '1'). Final config matches first apply's shadow values. |
| B038 | D | Apply with mode change (positive to negative) | 1 | Start in mode=0 (normal). Apply with mode = -1 (debug input 1). | After apply, port 0 ingress uses debug_1 input instead of AVST port data. |
| B039 | D | Apply with filter_enable toggle | 1 | Start with filter_enable=0. Apply with filter_enable=1, filter_key=0x5. | Hits not matching filter_key are rejected after apply. Before apply, all hits pass. |
| B040 | D | Apply preserves interval_cfg | 1 | Set interval_cfg = 5000, apply. | Readback interval_cfg == 5000. Timer uses new interval length. Catches cfg_interval_cfg not included in shadow copy. |

---

## 4. Key Extraction (KEY) -- 12 cases

Key extraction uses configurable bit ranges (UPDATE_KEY_BIT_HI/LO) and signed/unsigned interpretation. The extract_unsigned and extract_signed functions are the core logic. Bugs here silently put hits in wrong bins -- the histogram looks plausible but is shifted or mirrored.

| ID | Method | Scenario | Iter | Stimulus | Checker |
|----|--------|----------|------|----------|---------|
| B041 | D | Unsigned key: data[29:17] = 0x0100 (256) | 1 | Default bit range. 1 hit, data word has bits [29:17] = 256. | Extracted key = 256 (unsigned, zero-extended to SAR_TICK_WIDTH). Bin = (256 - (-1000))/16 = 78. |
| B042 | D | Unsigned key: data[29:17] = 0x1FFF (max 13-bit) | 1 | 1 hit, bits [29:17] = 8191. | Key = 8191. Verifies no truncation in 13-bit unsigned extraction. |
| B043 | D | Signed key: data[29:17] = 0x1FFF (all ones = -1 in signed 13-bit) | 1 | Set key_unsigned=0. 1 hit, bits [29:17] = 0x1FFF. | Key = -1 (sign-extended to 32-bit). Bin = (-1 - (-1000))/16 = 62. Catches missing sign extension. |
| B044 | D | Signed key: data[29:17] = 0x1000 (MSB set = -4096 in signed 13-bit) | 1 | Set key_unsigned=0. | Key = -4096. With left_bound = -1000, key < left_bound: underflow. |
| B045 | D | Custom bit range: key_low=0, key_high=7 (extract lowest byte) | 1 | Write KEY_FILTER_BITS with update_key_low=0, update_key_high=7. Apply. 1 hit, data[7:0] = 0xAB. | Key = 0xAB = 171 (unsigned). Verifies runtime-configurable bit range. |
| B046 | D | Custom bit range: key_low=35, key_high=38 (extract top nibble of 39-bit word) | 1 | Configure, apply. 1 hit, data[38:35] = 0xF. | Key = 15 (unsigned). Verifies extraction at top of AVST_DATA_WIDTH. |
| B047 | D | Key extraction with key_low = key_high (single bit) | 1 | Set key_low = key_high = 20. Apply. 1 hit, data[20] = 1. | Key = 1. Verifies degenerate 1-bit field. |
| B048 | D | Unsigned key with port offset: port 3, raw key = 50 | 1 | 1 hit on port 3. Effective key = 50 + 3*32 = 146. | Bin = (146 - (-1000))/16 = 71. Verifies offset is added to extracted key, not to raw data. |
| B049 | D | Signed key with port offset: port 2, raw key = -10 | 1 | Set key_unsigned=0. Effective key = -10 + 2*32 = 54. | Bin = (54 - (-1000))/16 = 65. Port offset is applied to signed result. |
| B050 | R | Random unsigned keys across full 13-bit range, 8 hits | 4 | 8 random keys per iteration, unsigned mode. | Each hit lands in correct bin per golden model (key - left_bound) / bin_width. |
| B051 | R | Random signed keys across full 13-bit range, 8 hits | 4 | 8 random signed keys per iteration. | Bin matches golden model; underflow/overflow counts match out-of-range keys. |
| B052 | D | Key extraction is decoupled from filter: key computed even when filtered out | 1 | Enable filter with mismatch. Inject 1 hit. | Hit is rejected (total_hits increments but bin count stays 0). Key computation still occurs (verified by checking no timing violation in synthesis -- functional test confirms filter_pass gates write_req, not key). |

---

## 5. Filter Logic (FLT) -- 12 cases

Filter examines a separate bit field (FILTER_KEY_BIT_HI/LO) and compares against a reference value. In pass mode, only matching hits enter the histogram. In reject mode, matching hits are discarded. The filter operates at ingress before the FIFO -- filtered hits never consume FIFO space.

| ID | Method | Scenario | Iter | Stimulus | Checker |
|----|--------|----------|------|----------|---------|
| B053 | D | Filter disabled (default): all hits pass | 1 | filter_enable=0. Inject 4 hits with varying filter fields. | All 4 hits enter histogram. total_hits == 4. |
| B054 | D | Filter enabled, pass mode, matching hit | 1 | filter_enable=1, filter_reject=0, filter_key=0x5. Inject 1 hit with data[38:35]=0x5. | Hit passes filter. Bin count == 1. |
| B055 | D | Filter enabled, pass mode, non-matching hit | 1 | filter_enable=1, filter_reject=0, filter_key=0x5. Inject 1 hit with data[38:35]=0x3. | Hit rejected. total_hits == 1 (accepted at ingress) but no bin update. Dropped_hits == 0 (filter rejection is not a drop). |
| B056 | D | Filter enabled, reject mode, matching hit | 1 | filter_enable=1, filter_reject=1, filter_key=0x5. Inject 1 hit with data[38:35]=0x5. | Hit rejected. No bin update. |
| B057 | D | Filter enabled, reject mode, non-matching hit | 1 | filter_enable=1, filter_reject=1, filter_key=0x5. Inject 1 hit with data[38:35]=0x3. | Hit passes. Bin count == 1. |
| B058 | D | Filter with custom bit range | 1 | Set filter_key_low=0, filter_key_high=3. filter_key=0xA. Inject 1 hit with data[3:0]=0xA. | Hit passes (pass mode). |
| B059 | D | Filter pass mode: 8 hits, 4 matching, 4 non-matching | 1 | Alternating filter field values. | Exactly 4 hits in histogram. total_hits == 8, but only 4 bin updates. |
| B060 | D | Filter reject mode: 8 hits, 4 matching, 4 non-matching | 1 | Same as B059, but reject mode. | Exactly 4 hits in histogram (the non-matching ones). |
| B061 | D | Filter applied at ingress: filtered hits do not consume FIFO space | 1 | Fill FIFO close to capacity (14 hits), then inject 2 filtered-out hits followed by 2 valid hits. | FIFO does not overflow. The 2 filtered hits are never written to FIFO. |
| B062 | D | Filter key = 0: match against zero field | 1 | filter_key=0, pass mode. Inject hit with filter field = 0. | Hit passes. Catches improper zero comparison. |
| B063 | D | Filter field spans AVST word boundary bits | 1 | Set filter_key_low=36, filter_key_high=38 (top 3 bits of 39-bit word). filter_key=0x7. | Hit with data[38:36]=0x7 passes. Verifies extraction at word boundary. |
| B064 | D | Filter toggled via apply: hits before apply unfiltered, after apply filtered | 1 | Inject 2 hits (no filter). Apply with filter_enable=1. Inject 2 more hits (1 matching, 1 not). | 3 total bin updates (2 + 1). Proves filter takes effect only after apply commits. |

---

## 6. Bin Mapping (BIN) -- 14 cases

The bin_divider computes bin_index = (key - left_bound) / bin_width using a pipelined restoring divider with BIN_INDEX_WIDTH (8) stages. Underflow if key < left_bound, overflow if key >= right_bound or bin_width = 0. The division is the most complex arithmetic in the design; off-by-one and rounding errors are the primary risk.

| ID | Method | Scenario | Iter | Stimulus | Checker |
|----|--------|----------|------|----------|---------|
| B065 | D | Uniform distribution: 256 hits, one per bin | 1 | 256 hits with keys = left_bound + i*bin_width for i in 0..255. | Each bin has count == 1. Proves all 256 bins are reachable. |
| B066 | D | All hits in bin 0: keys in [left_bound, left_bound + bin_width) | 1 | 4 hits with keys = {-1000, -999, -990, -985}. bin_width=16. | Bin 0 count == 4. All map to same bin. |
| B067 | D | Boundary: key = left_bound + bin_width (first key in bin 1) | 1 | key = -1000 + 16 = -984. | Bin 1 count == 1. Catches off-by-one in division. |
| B068 | D | Boundary: key = left_bound + bin_width - 1 (last key in bin 0) | 1 | key = -1000 + 15 = -985. | Bin 0 count == 1. |
| B069 | D | Underflow: key = left_bound - 1 | 1 | key = -1001. | underflow_cnt == 1, no bin update. |
| B070 | D | Overflow: key = right_bound | 1 | key = left_bound + 256*16 = 3096. | overflow_cnt == 1. right_bound is exclusive. |
| B071 | D | Overflow: key = right_bound + 1000 (far overflow) | 1 | key = 4096. | overflow_cnt == 1. No array-out-of-bounds. |
| B072 | D | Overflow: bin_width = 0 | 1 | Configure bin_width = 0, apply. Inject 1 hit within [left_bound, right_bound). | overflow_cnt == 1. bin_divider treats bin_width=0 as overflow. |
| B073 | D | Non-power-of-2 bin_width: bin_width = 7 | 1 | left_bound = 0, bin_width = 7. Inject hits at keys {0, 7, 14, 21}. | Bins {0, 1, 2, 3} each have count 1. Restoring divider correctly handles non-power-of-2. |
| B074 | D | Non-power-of-2 bin_width: bin_width = 3, keys at boundaries | 1 | left_bound = 0, bin_width = 3. Keys = {0, 2, 3, 5, 6, 8}. | Bins = {0, 0, 1, 1, 2, 2}. Bin 0 count=2, bin 1 count=2, bin 2 count=2. |
| B075 | D | Large bin_width: bin_width = 256, 4 hits across 4 bins | 1 | left_bound = 0, bin_width = 256. Keys = {0, 256, 512, 768}. | Bins {0, 1, 2, 3}. |
| B076 | D | bin_width = 1: every key maps to unique bin | 1 | left_bound = 0, bin_width = 1. Keys = {0, 1, 2, 3, 100, 255}. | Each key goes to its own bin. |
| B077 | D | Signed negative left_bound with positive keys | 1 | left_bound = -128, bin_width = 1. Key = 0. bin = 128. | Bin 128 count == 1. Verifies signed subtraction (key - left_bound) does not overflow. |
| B078 | R | Random bin_width (1-100), random keys, 16 hits | 4 | Randomize bin_width and left_bound per iteration. | Golden model (key - left_bound) / bin_width matches RTL bin index for each hit. |

---

## 7. Multi-Port Fill (MPF) -- 12 cases

8 Avalon-ST input ports share one round-robin arbiter and one bin_divider. The per-port hit FIFOs (depth 16) serialize concurrent arrivals. Port offset key += port_index * 32 makes each port map to a distinct bin region. The rr_arbiter skips the port it just served (starvation avoidance). Bugs here cause port starvation, wrong offset, or FIFO overflow.

| ID | Method | Scenario | Iter | Stimulus | Checker |
|----|--------|----------|------|----------|---------|
| B079 | D | 1 hit per port, all 8 ports, same raw key = 0 | 1 | 8 hits, one per port, raw key = 0 on each. Effective keys = {0, 32, 64, ..., 224}. | 8 distinct bins updated, each count == 1. Verifies port offset applied independently per port. |
| B080 | D | All hits on port 0 only (other ports idle) | 1 | 8 hits on port 0. | 8 hits processed. Round-robin arbiter does not block when only 1 FIFO has data. |
| B081 | D | 2 hits per port, all 8 ports, simultaneous injection | 1 | 16 hits total (2 per port in same cycle if possible; else consecutive). | All 16 processed. total_hits == 16. Arbiter round-robins fairly. |
| B082 | D | Port 0 and port 7 only, alternating hits | 1 | 4 hits alternating: port 0, port 7, port 0, port 7. | All 4 processed. Arbiter alternates between the two active ports. |
| B083 | D | FIFO backpressure: inject 16 hits on port 0 back-to-back | 1 | 16 hits at line rate. FIFO depth is 16. | All 16 processed without drop. FIFO level peaks at or near 16. fifo_level_max >= 14. |
| B084 | D | FIFO overflow: inject 17+ hits on port 0 without pipeline drain | 1 | Stall downstream (measure_clear to pause pipeline), inject 17 hits on port 0. | 16 enter FIFO, hit 17 triggers drop_pulse. dropped_hits >= 1. |
| B085 | D | Port offset correctness for each port | 1 | Inject 1 hit per port, raw key = left_bound. Expected effective keys = {left_bound, left_bound+32, ..., left_bound+224}. | 8 bins updated at calculated offsets. |
| B086 | D | Round-robin fairness: 4 hits per port, all 8 ports | 1 | 32 hits total. Monitor arbiter output port sequence. | Each port served 4 times. No port starved. |
| B087 | D | Port ready deasserted during config apply | 1 | Start continuous hits on all ports. Trigger config apply. | All port_ready signals go low during cfg_apply_pending. Hits resume after apply completes. No data lost. |
| B088 | D | Only odd ports active (1, 3, 5, 7) | 1 | 4 hits, one per odd port. | All 4 processed with correct port offsets. Arbiter handles non-contiguous active ports. |
| B089 | D | Port 0 with snoop: ready gated by downstream | 1 | Drive aso_hist_fill_out_ready = 0 on port 0. Inject hit on port 0. | port_ready(0) == 0 (backpressured by snoop sink). Hit is stalled, not lost. |
| B090 | D | Port 0 snoop disabled (SNOOP_EN=false generic override) | 1 | Instantiate with SNOOP_EN=false. Inject hit on port 0. | port_ready(0) == 1 always (not gated by snoop). aso_hist_fill_out_valid == 0. |

---

## 8. Bank Swap & Readout (BSR) -- 12 cases

The pingpong_sram has two 256x32 RAM banks. A timer counts up to interval_clocks-1, then fires interval_pulse, swaps active_bank, and initiates a clear of the newly active bank. The host reads the frozen (inactive) bank. Bugs here cause reading stale data, reading the active (live) bank, or clearing the wrong bank.

| ID | Method | Scenario | Iter | Stimulus | Checker |
|----|--------|----------|------|----------|---------|
| B091 | D | Timer fires at interval_clocks boundary | 1 | Set interval_cfg = 100. Wait 100 cycles. | interval_pulse fires exactly once. active_bank flips from 0 to 1. |
| B092 | D | Bank swap clears the new active bank | 1 | Inject 4 hits to bins {0,1,2,3}. Wait for swap. Inject 4 more hits to same bins. Wait for second swap. Read frozen bank. | Frozen bank shows counts from second interval only (not accumulated from first). |
| B093 | D | Host reads frozen bank, not active bank | 1 | Inject hits. Wait for swap. Read bins via AVMM hist_bin slave. | Read data matches previous interval's histogram, not current in-flight data. |
| B094 | D | Flush completes before new updates accepted | 1 | Wait for swap. Monitor o_flushing and o_upd_ready. | o_upd_ready == 0 while o_flushing == 1. After 256 cycles (clearing 256 bins), o_upd_ready == 1. |
| B095 | D | Two consecutive swaps: data integrity across 3 intervals | 1 | Interval 1: 4 hits in bin 0. Interval 2: 8 hits in bin 0. Wait for swap to interval 3. Read frozen bank. | Frozen bank (interval 2 data) has bin 0 == 8, not 12. |
| B096 | D | Bank status CSR reflects active bank and flush state | 1 | Read BANK_STATUS during flush and after. | During flush: bit 1 (flushing) == 1, bits [15:8] show flush_addr progressing 0..255. After: flushing == 0. |
| B097 | D | Timer resets on measure_clear_pulse | 1 | Start timer. Before it fires, issue measure_clear (write 0 to hist_bin slave). | Timer resets to 0. active_bank resets to 0. Both banks cleared. |
| B098 | D | measure_clear_pulse resets both banks | 1 | Inject hits. Issue measure_clear. Wait for clear to complete. Read all bins. | All 256 bins == 0 on both banks. |
| B099 | D | Interval_cfg = 0 disables timer (no automatic swap) | 1 | Set interval_cfg = 0. Wait 10000 cycles. | No interval_pulse. active_bank stays at 0. Timer stays at 0. |
| B100 | D | Read during active update (pingpong enabled): no conflict | 1 | Inject hits continuously. Issue burst read of frozen bank. | Read returns valid data. No bus hang. upd_ready may deassert briefly if update pipeline conflicts with read. |
| B101 | D | Pingpong disabled (ENABLE_PINGPONG=false): read active bank | 1 | Instantiate with ENABLE_PINGPONG=false. Inject hits. Read bins. | Read returns current (live) counts. Timer disabled. read_bank_latched == active_bank (same bank for read and write). |
| B102 | D | i_interval_reset triggers measure_clear_pulse externally | 1 | Assert i_interval_reset for 1 cycle. | measure_clear_pulse fires. Pipeline flushed, banks cleared, stats reset. Same effect as writing 0 to hist_bin slave. |

---

## 9. Coalescing Queue (COA) -- 12 cases

The coalescing_queue receives (bin_index, count=1) from the divider and merges repeated hits to the same bin into (bin, accumulated_count) before writing to SRAM. A circular queue of depth 256 tracks which bins are "in flight". Each bin has a kick_ram counter (8-bit). When the queue head drains, the accumulated count is forwarded to pingpong_sram. Bugs cause lost counts, double-counting, or queue overflow under bursty traffic.

| ID | Method | Scenario | Iter | Stimulus | Checker |
|----|--------|----------|------|----------|---------|
| B103 | D | 4 consecutive hits to same bin: coalesced into 1 SRAM update | 1 | 4 hits all mapping to bin 10. | Bin 10 count == 4. Queue occupancy peaked at 1 (single entry for bin 10). |
| B104 | D | 4 hits to 4 different bins: 4 separate queue entries | 1 | Hits to bins {0, 1, 2, 3}. | Each bin count == 1. Queue occupancy peaked at 4. |
| B105 | D | Coalescing: 8 hits alternating between 2 bins | 1 | Hits: bin 0, bin 1, bin 0, bin 1, ... | Bin 0 count == 4, bin 1 count == 4. Queue had 2 entries, kick counts accumulated. |
| B106 | D | Queue drain order is FIFO: first bin entered drains first | 1 | Hits to bins {5, 10, 15} in that order. Monitor drain output. | Drain order: bin 5, then bin 10, then bin 15. |
| B107 | D | Queue occupancy and occupancy_max reported correctly | 1 | Inject 10 hits to 10 different bins. Wait for full drain. | COAL_STATUS: occupancy == 0 at end, occupancy_max >= 10. |
| B108 | D | Queue clear on measure_clear_pulse | 1 | Inject 8 hits (8 distinct bins queued). Issue measure_clear. | Queue level == 0 after clear. No stale entries. |
| B109 | D | Kick counter saturation at 255 (KICK_WIDTH=8) | 1 | Inject 256 hits to same bin before it drains. | kick_ram saturates at 255. 256th hit triggers overflow (queue overflow_count increments). |
| B110 | D | Queue full: 256 distinct bins, then 1 more distinct bin | 1 | Inject 256 hits to 256 distinct bins (filling queue). Then inject 1 hit to bin 0 (already queued: coalesces, no overflow). | No overflow. Bin 0 count == 2. Proves coalescing avoids queue overflow when bin is already tracked. |
| B111 | D | Queue overflow: 256 distinct bins queued, new distinct bin arrives | 1 | Inject 257 hits to 257 distinct bins. Only 256 queue slots exist. | queue_overflow_count >= 1. 257th hit to new bin is lost. |
| B112 | D | Simultaneous drain and hit to same bin: race condition | 1 | Time a hit to arrive at the exact cycle the queue head (same bin) is draining. | Hit is not lost: if drain frees the slot, the new hit re-enqueues the bin. Final count correct. Exercises the drain_fire_c == 1 and drain_head_bin_c == BIN_INDEX_C path. |
| B113 | D | Queue clear sweep takes N_BINS cycles | 1 | Trigger clear. Count cycles until clear_active deasserts. | Exactly 256 cycles (one per bin to zero kick_ram and queued flags). |
| B114 | D | Queue occupancy_max survives across multiple bursts | 1 | Burst 1: 5 distinct bins. Drain. Burst 2: 3 distinct bins. | occupancy_max == 5 (high-water mark from burst 1, not reset by drain). |

---

## 10. Snoop Passthrough (SNP) -- 8 cases

Port 0 has a snoop path: input data is forwarded to aso_hist_fill_out regardless of filter/bin processing. The snoop is controlled by SNOOP_EN generic and gated by downstream ready. Snoop-related bugs cause data corruption on the passthrough or unintended backpressure on the histogram ingress.

| ID | Method | Scenario | Iter | Stimulus | Checker |
|----|--------|----------|------|----------|---------|
| B115 | D | Snoop mirrors data exactly | 1 | 1 hit on port 0, data = 0x7FFFFFFFFF. | aso_hist_fill_out_data == 0x7FFFFFFFFF, valid == 1. |
| B116 | D | Snoop mirrors channel field | 1 | 1 hit on port 0, channel = 0xA. | aso_hist_fill_out_channel == 0xA. |
| B117 | D | Snoop mirrors SOP/EOP when ENABLE_PACKET=true | 1 | 1 hit with startofpacket=1, endofpacket=1. | aso_hist_fill_out_startofpacket == 1, endofpacket == 1. |
| B118 | D | Snoop SOP/EOP forced 0 when ENABLE_PACKET=false | 1 | Instantiate with ENABLE_PACKET=false. 1 hit with SOP=1, EOP=1. | aso_hist_fill_out_startofpacket == 0, endofpacket == 0 (forced by generic). |
| B119 | D | Snoop backpressure propagates to port_ready(0) | 1 | Deassert aso_hist_fill_out_ready. Inject hit on port 0. | port_ready(0) == 0. Hit is stalled. |
| B120 | D | Snoop does not exist on ports 1-7 | 1 | Inject hit on port 1. | aso_hist_fill_out_valid not toggled (only port 0 feeds snoop). Port 1 data enters histogram only. |
| B121 | D | Snoop output valid follows input valid | 1 | Toggle asi_hist_fill_in_valid for 4 cycles: 1,0,1,0. | aso_hist_fill_out_valid matches 1,0,1,0 on same cycles (combinational path). |
| B122 | D | Snoop with filter active: snoop passes data even when filter rejects | 1 | Enable filter, inject hit that fails filter on port 0. | Snoop output still valid (snoop is before filter). Bin not updated (filter rejected). |

---

## 11. Debug Mode (DBG) -- 8 cases

Negative cfg_mode values select one of 6 debug inputs (asi_debug_1 through asi_debug_6). In debug mode, only port 0 is used; its data comes from the selected debug input (16-bit) instead of the AVST port. Mode -1 uses signed interpretation; other negative modes use unsigned. This is the only way to histogram internal signals.

| ID | Method | Scenario | Iter | Stimulus | Checker |
|----|--------|----------|------|----------|---------|
| B123 | D | Debug mode -1: select debug input 1, signed | 1 | Set mode = -1 (0xF in 4-bit two's complement), apply. Drive asi_debug_1_valid=1, data=0x0064 (100). | Key = 100 (sign-extended). Correct bin updated. |
| B124 | D | Debug mode -2: select debug input 2, unsigned | 1 | Set mode = -2 (0xE), apply. Drive asi_debug_2_valid=1, data=0x00C8 (200). | Key = 200 (unsigned). Correct bin updated. |
| B125 | D | Debug mode -6: select debug input 6 (last) | 1 | Set mode = -6 (0xA), apply. Drive asi_debug_6_valid=1, data=0x0001. | Key = 1. Bin updated. Verifies all 6 debug ports are wired. |
| B126 | D | Debug mode: AVST ports 1-7 ignored | 1 | Set mode = -1. Inject hits on ports 1-7. | No bin updates from ports 1-7. Only debug_1 input produces hits. |
| B127 | D | Debug mode: port 0 AVST data ignored | 1 | Set mode = -1. Drive asi_hist_fill_in_valid=1 on port 0. Simultaneously drive asi_debug_1_valid=0. | No bin update (debug_valid overrides AVST valid for port 0). |
| B128 | D | Debug mode -1: negative 16-bit value, sign-extended | 1 | Set mode = -1. Drive debug_1_data = 0xFFFF (-1 signed 16-bit). | Key = -1 (sign-extended to 32-bit). Exercises signed path in build_debug_key. |
| B129 | D | Debug mode -3: positive 16-bit value, unsigned | 1 | Set mode = -3. Drive debug_3_data = 0xFFFF. | Key = 65535 (unsigned, zero-extended). Different from mode -1. |
| B130 | D | Switch from debug mode back to normal mode | 1 | Start in mode = -1 (debug). Apply with mode = 0 (normal). Inject AVST hit on port 0. | Hit processed normally (from AVST data, not debug input). |

---

## 12. Statistics Counters (STC) -- 12 cases

Four 32-bit saturating counters: total_hits, dropped_hits, underflow_cnt, overflow_cnt. They reset on interval_pulse or measure_clear_pulse (via stats_reset_pulse_d1). The counters are pipelined (1-cycle delayed via stats_pipe) to avoid timing issues. A counter bug means the host cannot monitor histogram health.

| ID | Method | Scenario | Iter | Stimulus | Checker |
|----|--------|----------|------|----------|---------|
| B131 | D | total_hits increments by 1 per accepted hit | 1 | Inject 5 hits on port 0. | TOTAL_HITS CSR == 5. |
| B132 | D | total_hits counts across all ports simultaneously | 1 | Inject 1 hit on each of 8 ports in same cycle. | TOTAL_HITS == 8. Exercises parallel accept_count_v accumulation. |
| B133 | D | dropped_hits increments on FIFO-full drop | 1 | Overflow port 0 FIFO (17 hits without drain). | DROPPED_HITS >= 1. |
| B134 | D | underflow_cnt increments for key < left_bound | 1 | Inject 3 hits with keys below left_bound. | UNDERFLOW_CNT == 3, OVERFLOW_CNT == 0. |
| B135 | D | overflow_cnt increments for key >= right_bound | 1 | Inject 2 hits with keys at or above right_bound. | OVERFLOW_CNT == 2, UNDERFLOW_CNT == 0. |
| B136 | D | Stats reset on interval_pulse | 1 | Inject 4 hits. Wait for interval_pulse. Read counters. | All 4 counters == 0 (reset by stats_reset_pulse_d1). |
| B137 | D | Stats reset on measure_clear_pulse | 1 | Inject 4 hits. Issue measure_clear. Read counters. | All 4 counters == 0. |
| B138 | D | underflow and overflow are mutually exclusive per hit | 1 | Inject 1 hit with key < left_bound, then 1 with key >= right_bound. | underflow == 1, overflow == 1. They track different conditions. |
| B139 | D | Filtered-out hits still count in total_hits | 1 | Enable filter. Inject 3 hits, all filtered out. | total_hits == 3, bin counts all 0. accept_pulse fires for every ingress-sampled hit, regardless of filter result. |
| B140 | D | Saturating counter: total_hits does not wrap at 0xFFFFFFFF | 1 | Pre-load total_hits near max (via long injection). Inject 1 more. | Counter stays at 0xFFFFFFFF (sat_add). Does not wrap to 0. |
| B141 | D | Stats pipeline latency: counter update appears 2 cycles after hit | 1 | Inject 1 hit. Read TOTAL_HITS after 1 cycle, then after 3 cycles. | After 1 cycle: may still be 0 (pipeline delay). After 3 cycles: == 1. Verifies stats_pipe 1-cycle + stats_reg 1-cycle delay. |
| B142 | D | Stats not affected by coalescing queue overflow | 1 | Trigger coalescing queue overflow (257 distinct bins). | total_hits counts all 257 accepted hits. overflow_cnt counts only divider overflows, not queue overflows. Queue overflow is tracked separately in COAL_STATUS. |

---

## 13. Burst Read (BRD) -- 8 cases

The hist_bin AVMM slave supports burst reads of histogram bins from the frozen bank. The burst engine auto-increments the address and returns readdatavalid for each word. A write of 0x00000000 to the slave triggers measure_clear. Read/update conflicts are arbitrated: burst_active blocks updates in non-pingpong mode, and the burst waits for the update pipeline to drain before starting.

| ID | Method | Scenario | Iter | Stimulus | Checker |
|----|--------|----------|------|----------|---------|
| B143 | D | Single-word read (burstcount=1) of bin 0 | 1 | Inject 3 hits to bin 0. Wait for swap. Read hist_bin addr 0, burstcount=1. | readdatavalid fires once. readdata == 3. |
| B144 | D | Burst read of 16 consecutive bins | 1 | Inject known hits to bins 0-15. Wait for swap. Read addr 0, burstcount=16. | 16 consecutive readdatavalid pulses. Each readdata matches expected count for that bin. |
| B145 | D | Full burst read of all 256 bins | 1 | Inject uniform histogram. Wait for swap. Read addr 0, burstcount=256. | 256 readdatavalid pulses. All counts match expected. |
| B146 | D | Burst read with start address != 0 | 1 | Inject hits to bins 100-103. Wait for swap. Read addr 100, burstcount=4. | 4 readdata values match bins 100-103. |
| B147 | D | measure_clear via hist_bin write (writedata=0x00000000) | 1 | Inject hits. Write 0 to hist_bin slave. Wait for clear. Read bins. | All bins == 0. Stats counters reset. writeresponsevalid fires 1 cycle after write. |
| B148 | D | Burst read does not interfere with concurrent updates (pingpong mode) | 1 | Inject hits continuously while reading frozen bank via burst. | Read data is consistent (from frozen bank). Update pipeline not corrupted. |
| B149 | D | burstcount = 0 treated as burstcount = 1 | 1 | Issue read with burstcount=0. | Exactly 1 readdatavalid fires. Avoids infinite burst. Defensive behavior in RTL (ram_v_burst_count := to_unsigned(1, ...)). |
| B150 | D | Back-to-back burst reads | 1 | Issue burst read of 4 bins. Immediately after completion, issue another burst read of 4 bins. | Both bursts complete. No bus hang. Second burst returns correct data. |

---

## Summary

| Section | Cases | ID Range | What it Proves |
|---------|-------|----------|----------------|
| SH (Single-Hit Smoke) | 12 | B001-B012 | Data path is connected; a single hit traverses the full pipeline and lands in the correct bin |
| CSR (CSR Access) | 16 | B013-B028 | Register map is correct; every CSR is reachable and returns expected reset values |
| CFG (Config Apply) | 12 | B029-B040 | Apply flow works: pending blocks ingress, shadow copy commits when pipeline drains |
| KEY (Key Extraction) | 12 | B041-B052 | Unsigned/signed extraction, configurable bit ranges, port offset arithmetic |
| FLT (Filter Logic) | 12 | B053-B064 | Pass/reject modes, custom bit ranges, filter-ingress interaction |
| BIN (Bin Mapping) | 14 | B065-B078 | Restoring divider correctness, boundary cases, non-power-of-2 widths |
| MPF (Multi-Port Fill) | 12 | B079-B090 | Per-port injection, round-robin fairness, FIFO overflow, snoop interaction |
| BSR (Bank Swap & Readout) | 12 | B091-B102 | Timer-driven swap, bank clearing, frozen-bank readout, measure_clear |
| COA (Coalescing Queue) | 12 | B103-B114 | Same-bin merging, queue drain order, saturation, overflow, clear sweep |
| SNP (Snoop Passthrough) | 8 | B115-B122 | Port 0 passthrough integrity, backpressure, packet control, filter independence |
| DBG (Debug Mode) | 8 | B123-B130 | Debug input selection, signed/unsigned interpretation, mode switching |
| STC (Statistics Counters) | 12 | B131-B142 | Counter increment accuracy, reset behavior, saturation, pipeline latency |
| BRD (Burst Read) | 8 | B143-B150 | Single/burst reads, measure_clear via write, concurrent access, edge cases |
