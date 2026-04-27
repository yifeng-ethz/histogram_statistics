# histogram_statistics_v2 DV — Error, Reset, and Fault Handling Cases

**Parent:** [DV_PLAN.md](DV_PLAN.md)
**Companion docs:** [DV_PLAN.md](DV_PLAN.md), [DV_HARNESS.md](DV_HARNESS.md), [DV_REPORT.md](DV_REPORT.md), [BUG_HISTORY.md](BUG_HISTORY.md)
**ID Range:** X001-X999
**Total:** 144 cases
**Method:** All directed (D)

The histogram_statistics_v2 IP runs continuously on an FPGA frontend, accumulating hit counts across measurement intervals. Errors are not rare: FIFO overflow happens under burst traffic, configuration mistakes happen during commissioning, and interval resets arrive asynchronously relative to the data pipeline. This document exhaustively covers every recovery path, every silent-corruption risk, and every state-machine corner that a reset or fault can expose.

## Architecture Summary for Error Context

The data pipeline has 6 distinct stages where state can be stranded by a reset or fault:

```
Port valid -> [ingress_stage_reg] -> [hit_fifo x8] -> [rr_arbiter]
           -> [divider_pipe 3-stage] -> [bin_divider 8-stage] -> [queue_hit_pipe]
           -> [coalescing_queue] -> [pingpong_sram 4-stage update pipeline]
```

Two reset domains exist:
- **i_rst** (hard reset): clears everything including CSR register values and config shadow registers. Returns IP to generic-default state.
- **measure_clear_pulse** (soft clear): clears pipeline, FIFOs, queue, SRAM banks, and stats counters, but preserves CSR register values and active config shadow. Triggered by `i_interval_reset` or writing `0x00000000` to `avs_hist_bin`.

Key numeric parameters: N_BINS=256, N_PORTS=8, FIFO depth=256, COAL_QUEUE_DEPTH=256, KICK_WIDTH=8 (max 255 per coalesced entry), COUNT_WIDTH=32, bin_divider pipeline depth = BIN_INDEX_WIDTH = 8 stages + 1 range-check = 9 total.

| Tier | Name | Definition | DUT Response | Recovery |
|------|------|-----------|--------------|----------|
| **1 - RST** | Hard Reset Behavior | i_rst=1 clears all state to generic defaults | All registers, FIFOs, queues, SRAM banks zeroed; CSR values reset to generic defaults; config shadow reset | Self (deassert i_rst) |
| **2 - CLR** | Measure Clear | measure_clear_pulse flushes pipeline and data, preserves CSR | Pipeline flushed; FIFOs emptied; queue cleared with 256-cycle walk; SRAM both-bank cleared; stats zeroed; CSR values preserved | Self (clear_active goes low after N_BINS cycles per bank) |
| **3 - CER** | Configuration Errors | Invalid config detected at apply time | csr_error=1, error_info set, cfg_apply_request blocked | Software corrects config, re-applies |
| **4 - FOD** | FIFO Overflow & Drop | Hit arrives when per-port FIFO is full | Hit silently dropped; drop_pulse fires; csr_dropped_hits incremented | Self (FIFO drains, new hits accepted) |
| **5 - CQO** | Coalescing Queue Overflow | Queue full + new bin, or kick counter saturated | overflow_count incremented; hit lost from bin perspective | Self (queue drains, new entries accepted) |
| **6 - PFR** | Pipeline Flush & Recovery | measure_clear_pulse arriving mid-pipeline | All pipeline stages zeroed; in-flight hits lost; clear_active blocks upd_ready for ~256 cycles | Self (clear_active deasserts) |
| **7 - ILL** | Illegal/Unexpected Sequences | Out-of-spec stimulus or degenerate timing | Varies: silently ignored, gracefully clamped, or blocked | Depends on case |

---

## 1. Hard Reset Behavior (RST) -- 27 cases

Hard reset (i_rst=1) is the most fundamental recovery mechanism. Every register, every FIFO, every RAM bank must return to a known-good state. These cases verify that no state survives reset, and that the IP is fully operational on the first cycle after reset deasserts.

### 1.1 Basic Reset State Verification

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X001 | All CSR registers at default after reset | Assert i_rst for 10 cycles, deassert, read all 16 CSR addresses | CSR[0] control: apply_pending=0, mode=0, key_unsigned=generic default, filter_enable=0, error=0. CSR[1]=DEF_LEFT_BOUND. CSR[2]=DEF_LEFT_BOUND+DEF_BIN_WIDTH*N_BINS. CSR[3]=DEF_BIN_WIDTH. CSR[6..7]=0 (stats). CSR[11..12]=0 (total/dropped). CSR[15]=0 (scratch). | First CSR read after reset returns correct defaults; no bus hang. |
| X002 | Config shadow registers at default after reset | Assert i_rst, deassert, immediately inject hits spanning the default bin range | Hits land in correct default bins (cfg_left_bound, cfg_bin_width at generic defaults). bin_divider computes correct indices using default config, not stale pre-reset config. | Burst-read histogram; verify bin counts match expected default-config mapping. |
| X003 | All 8 hit FIFOs empty after reset | Assert i_rst, deassert, read FIFO diagnostics via CSR[10] port_status | fifo_empty shadow = 0xFF (all 8 bits set). fifo_level_max = 0. No stale data from pre-reset FIFO contents. | Inject 1 hit on port 0; verify it appears in histogram (FIFO accepted it). |
| X004 | Coalescing queue empty after reset | Assert i_rst, deassert, read CSR[14] coal_status | queue_occupancy=0, queue_occupancy_max=0, queue_overflow_count=0. queue_rd_ptr=queue_wr_ptr=0 (inferred from occupancy=0). | Inject hit; verify queue accepts it and drains to SRAM. |
| X005 | Both SRAM banks zeroed after reset | Assert i_rst, deassert, wait for clear_active to go low (~512 cycles for both banks), burst-read all 256 bins | All 256 bins read as 0x00000000. Both bank A and bank B cleared (verify by toggling active_bank if possible, or reading from each via pingpong). | Inject hits; verify counts start from 0, not from pre-reset residual. |
| X006 | Stats counters zeroed after reset | Inject 100 hits (some causing underflow/overflow), assert i_rst, deassert, read CSR[6], CSR[7], CSR[11], CSR[12] | csr_underflow_count=0, csr_overflow_count=0, csr_total_hits=0, csr_dropped_hits=0. | Inject 1 hit; verify total_hits increments from 0, not from pre-reset value. |
| X007 | active_bank=0 after reset | Run for several intervals (active_bank toggles), then assert i_rst, deassert, read CSR[9] bank_status | active_bank bit = 0. clear_active initially 1 (flushing bit set). | Wait for flushing=0; verify active_bank still 0. |
| X008 | Interval timer zeroed after reset | Set interval to 100 clocks, run for 50 clocks, assert i_rst for 5 cycles, deassert, wait 100 clocks | Interval pulse fires at exactly cycle 100 after reset deasserts (timer restarted from 0, not from pre-reset count of 50). | Verify interval_pulse timing via bank_status flushing bit transition. |

### 1.2 Reset During Active Pipeline

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X009 | Reset with hits in ingress stage | Inject 8 hits simultaneously (all ports), assert i_rst 1 cycle later (hits are in ingress_stage_reg) | ingress_stage_valid all cleared to 0. Hits are lost (expected). No residual write to FIFOs after reset deasserts. | Inject new hits after reset; verify they are processed correctly with no ghost counts from pre-reset ingress data. |
| X010 | Reset with data in hit FIFOs | Fill all 8 FIFOs to depth 8, then assert i_rst before arbiter drains them | All FIFOs empty after reset (level=0, empty=1). rd_ptr=wr_ptr=0. No FIFO data leaks into post-reset pipeline. | Inject 1 hit per port; verify each FIFO accepts it at position 0 (pointers truly reset). |
| X011 | Reset with arbiter mid-grant | Inject hits on ports 0 and 1, wait for arbiter to grant port 0 (out_valid_q=1), assert i_rst | Arbiter last_served=0, out_valid_q=0. No partial data forwarded to divider_pipe. | Inject hit on port 1 after reset; verify arbiter serves port 1 (round-robin starts from port 0+1=1 after last_served reset to 0). |
| X012 | Reset with data in bin_divider pipeline (stage 4 of 8) | Inject 4 hits in quick succession (they fill divider stages 0-3), assert i_rst | All valid_pipe stages cleared to 0. No partial quotient emerges from divider after reset. | Inject hit after reset; verify it traverses all 9 divider stages cleanly. |
| X013 | Reset with data in coalescing queue (50 entries queued) | Inject 50 distinct-bin hits, wait for them to enter queue, assert i_rst | queue_level=0, all queued[] flags cleared, all kick_ram[] entries zeroed (after 256-cycle clear walk). | Inject same 50 bins after reset; verify each gets kick_ram=1, not residual pre-reset value. |
| X014 | Reset with SRAM update pipeline mid-flight | Inject hit, wait for queue drain to issue upd_valid, assert i_rst when upd_read_valid=1 (mid read-modify-write) | upd_issue_valid through upd_write_valid all cleared to 0. No partial write to SRAM. | Burst-read the bin that was mid-update; verify it reads 0 (not a partial/corrupt count). |
| X015 | Reset during SRAM clear (clear_active=1, clear_addr=128) | Assert i_rst while clear_active is already running (e.g., after a measure_clear) at clear_addr=128 | Reset restarts clear from address 0 with clear_both=1. Both banks cleared completely from 0 to 255. No gap at addresses 0-128 of the second bank. | Burst-read all 256 bins; verify all are 0. |

### 1.3 Reset During Host Interface Operations

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X016 | Reset during burst read (burst_remaining=100) | Start burst read of 200 bins, assert i_rst after 100 readdatavalid pulses | burst_active=0, burst_remaining=0. No more readdatavalid pulses after reset. hist_read_pending=0. | Issue new burst read after reset; verify it starts cleanly from requested address, returns correct data. |
| X017 | Reset during CSR read | Assert avs_csr_read=1 for address 6, assert i_rst on same cycle | csr_readdata_reg cleared to 0. No bus hang. | Read CSR[6] after reset; verify returns 0 (counter reset). |
| X018 | Reset during CSR write (config apply in progress) | Write new config to CSR[1..5], write CSR[0] with apply bit, assert i_rst before cfg_apply_pending clears | cfg_apply_pending=0 after reset. CSR registers at generic defaults (not the values written before reset). cfg shadow at generic defaults. | Apply new config after reset; verify it uses fresh values, not stale pre-reset write. |
| X019 | Reset during hist_bin write (measure_clear trigger) | Write 0x00000000 to avs_hist_bin (triggers measure_clear_comb), assert i_rst on same cycle | i_rst takes priority. Full hard reset occurs. measure_clear_pulse may fire but is irrelevant since i_rst resets everything. | Verify full default state after reset. |

### 1.4 Reset Pulse Width and Edge Cases

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X020 | 1-cycle reset pulse (minimum duration) | Assert i_rst for exactly 1 clock cycle | All state fully reset. Synchronous reset needs only 1 rising edge to take effect. | Full functional test after reset: config apply, hit injection, burst read. |
| X021 | 100-cycle reset pulse (sustained) | Assert i_rst for 100 cycles while hits arrive on all ports | All incoming hits during reset are ignored (port_ready driven by ingress_comb which depends on cfg_apply_pending, but ingress_stage_reg is cleared). No state corruption from sustained reset. | Deassert reset; verify clean operation. |
| X022 | Reset deassert + immediate hit on cycle 0 | Deassert i_rst, drive port_valid(0)=1 with valid data on the exact same rising edge | Hit may or may not be accepted (depends on whether ingress sees non-reset state on that edge). If accepted, it must be processed correctly. If rejected, no corruption. Verify either outcome is clean. | Check total_hits: either 0 (rejected) or 1 (accepted). No intermediate/corrupt state. |
| X023 | Reset deassert + immediate CSR write on cycle 0 | Deassert i_rst, drive avs_csr_write=1 with address=15 (scratch), writedata=0xDEADBEEF on same edge | Write either takes effect (scratch=0xDEADBEEF) or is lost (scratch=0). Either is acceptable. No bus hang. No partial write. | Read CSR[15]; verify it is either 0x00000000 or 0xDEADBEEF. |
| X024 | Double reset (assert during assert) | Assert i_rst, wait 5 cycles, deassert for 1 cycle, reassert for 5 more cycles, deassert | State reflects the second reset. No metastable FSM state from the brief deassert gap. | Full functional test: all defaults, clean operation. |
| X025 | Reset glitch: 1-cycle deassert gap in sustained reset | Assert i_rst for 10 cycles, deassert for 1 cycle (gap), reassert for 10 cycles | The 1-cycle gap may allow 1 clock of non-reset operation. After second reset, all state is clean. The gap must not leave any FSM in an illegal state. | Full state verification after final deassert. |
| X026 | Reset immediately after power-on (no prior operation) | Assert i_rst as the very first stimulus after simulation start (no warm-up) | All signal initial values overridden by reset. VHDL signal initialization values (`:= ...`) are consistent with reset assignments. | Verify every CSR reads its generic default. |
| X027 | Reset during cfg_apply_pending=1 with error flag set | Set csr_error=1 via bad config, then assert i_rst | csr_error cleared to 0. csr_error_info cleared to 0. cfg_apply_pending cleared to 0. Error state does not persist across hard reset. | Read CSR[0]; verify error bit = 0. Apply valid config; verify it succeeds. |

---

## 2. Measure Clear (CLR) -- 22 cases

Measure clear (measure_clear_pulse) is the soft reset used between measurement intervals. It must flush all pipeline data and zero all counts without disturbing CSR register values or active configuration. The 1-cycle registration delay (measure_clear_comb -> measure_clear_pulse) means there is always a 1-cycle window where new hits can enter the pipeline after the clear trigger but before the clear takes effect.

### 2.1 Clear vs Reset: CSR Preservation

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X028 | CSR register values preserved across clear | Write distinctive values to all writable CSRs (left_bound=-500, bin_width=8, scratch=0xCAFEBABE, etc.), trigger measure_clear via hist_bin write of 0x00000000, read all CSRs | CSR[1] still -500. CSR[3] still 8. CSR[15] still 0xCAFEBABE. CSR[8] interval_cfg unchanged. Only stats counters (CSR[6,7,11,12]) are zeroed. | Inject hits; verify they use the preserved config (bins computed with left_bound=-500, bin_width=8). |
| X029 | Active config shadow preserved across clear | Apply config (left_bound=-200, bin_width=4), trigger clear, inject hit at key=-198 | Hit lands in bin index (-198 - (-200))/4 = 0. Config shadow cfg_left_bound, cfg_bin_width unchanged by clear. | Histogram bin 0 count = 1. |
| X030 | csr_error flag preserved across clear | Set invalid config (bin_width=0, right_bound <= left_bound), observe csr_error=1, trigger measure_clear | csr_error still 1 after clear. csr_error_info still 0x1. Error flag is a CSR register value, not pipeline state. | Read CSR[0]; verify error bit still set. Write valid config to clear it. |

### 2.2 Clear Trigger Mechanisms

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X031 | Clear via hist_bin write of 0x00000000 | Write avs_hist_bin_writedata=0x00000000 with avs_hist_bin_write=1 | clear_pulse fires on write cycle. measure_clear_pulse fires 1 cycle later (registered). Pipeline flushed. writeresponsevalid fires (hist_writeresp_valid). | Wait for clear_active to deassert (~512 cycles); inject hits; verify clean histogram. |
| X032 | Clear via i_interval_reset | Pulse i_interval_reset=1 for 1 cycle | measure_clear_comb=1. measure_clear_pulse fires 1 cycle later. Same pipeline flush as hist_bin write clear. | Verify same behavior as X031. |
| X033 | Clear via hist_bin write of non-zero value: NO clear | Write avs_hist_bin_writedata=0x00000001 with avs_hist_bin_write=1 | clear_pulse does NOT fire (writedata != 0x00000000). Pipeline continues undisturbed. writeresponsevalid still fires (it is unconditional on hist_bin write). | Verify pipeline state unchanged; in-flight hits not lost. |
| X034 | Both clear triggers simultaneously (hist_bin write + i_interval_reset) | Drive both clear_pulse and i_interval_reset=1 on the same cycle | measure_clear_comb=1 (OR of both). Only one measure_clear_pulse fires (they are OR'd before registration). No double-clear or state corruption. | Verify single clean flush; no double-decrement of queue_level or double-clear of SRAM. |

### 2.3 Clear During Active Data

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X035 | Clear with hits in ingress stage | Inject 8 hits (all ports), trigger clear 1 cycle later | ingress_stage_valid all cleared. Hits that entered ingress_stage_reg on the same cycle as measure_clear_pulse are discarded. | Inject new hits; verify they start fresh (total_hits counts from 0). |
| X036 | Clear with FIFOs partially full | Fill each FIFO to depth 8, trigger clear | All FIFOs: level=0, empty=1. max_level reset to 0. Data in FIFOs is lost (expected -- these are stale interval hits). | CSR[10] port_status: fifo_empty=0xFF. |
| X037 | Clear with coalescing queue at 200 entries | Inject 200 distinct-bin hits, wait for queue to accumulate, trigger clear | queue_level=0 after clear walk completes (256 cycles). All queued[] flags cleared. All kick_ram[] entries zeroed. overflow_count=0. | CSR[14] coal_status: occupancy=0, max=0, overflow=0. |
| X038 | Clear with SRAM update pipeline mid-flight | Inject hit, wait for upd_issue_valid=1, trigger clear | upd_issue_valid through upd_write_valid all cleared. Partial read-modify-write abandoned. SRAM bank cleared by clear walk. | Burst-read target bin; verify 0 (not partial count). |
| X039 | Clear during burst read | Start 256-bin burst read, trigger clear after 128 readdatavalid pulses | burst_active=0 (cleared). burst_remaining=0. Remaining 128 reads do NOT fire. The 128 already-returned values may contain pre-clear counts (acceptable -- they were issued before clear). | Issue new burst read after clear completes; verify all bins are 0. |
| X040 | Clear during bank swap (interval_pulse and measure_clear coincidence) | Set interval to N cycles. On the exact interval boundary, also pulse i_interval_reset | Both interval_pulse and measure_clear_pulse fire. The pingpong_sram clear walk is reset (clear_both=1 from measure_clear, overriding the single-bank clear from interval swap). Both banks zeroed. | Verify active_bank=0 (measure_clear resets it), both banks read 0. |

### 2.4 Clear Timing and Sequencing

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X041 | Back-to-back clears (2 clears, 1 cycle apart) | Trigger clear, then trigger clear again on the next cycle | First measure_clear_pulse fires at T+1. Second measure_clear_comb at T+1 -> measure_clear_pulse at T+2. The second clear restarts the clear walk from address 0. No state corruption from overlapping clears. | Wait for clear_active to deassert; verify clean state. |
| X042 | Clear immediately followed by hit injection (cycle after measure_clear_pulse) | Trigger clear, inject hit on the very next cycle | Hit arrives while clear_active=1 in coalescing_queue and pingpong_sram. FIFO accepts the hit (FIFO clear is synchronous, completes in 1 cycle). But queue blocks it (clear_active=1 blocks normal queue operation for 256 cycles). Hit sits in FIFO until clear walk completes. | After clear walk, hit drains through pipeline into correct bin. Histogram shows count=1 for that bin. |
| X043 | measure_clear_pulse 1-cycle registration delay window | Inject hit on exact cycle of measure_clear_comb (before registration to measure_clear_pulse) | The hit enters ingress_stage_reg. On the next cycle, measure_clear_pulse fires and clears ingress_stage_valid. The hit is lost despite being valid when accepted. This is the expected 1-cycle vulnerability window. | total_hits may have been incremented (accept_pulse fired before clear), but hit never reaches histogram. Verify total_hits and histogram bin are consistent (total_hits may be 1 higher than sum of bins + underflow + overflow). |
| X044 | Stats counters reset by clear (via stats_reset_pulse_d1) | Inject 50 hits, verify total_hits=50, trigger clear, read stats | csr_total_hits=0 (stats_reset_pulse_d1 fires from measure_clear_pulse). csr_dropped_hits=0. csr_underflow_count=0. csr_overflow_count=0. The 2-cycle delay (measure_clear_pulse -> stats_reset_pulse_d1 -> stats_reg) means stats are zeroed 2 cycles after clear trigger. | Inject 1 hit after clear; verify total_hits=1 (counting from 0). |
| X045 | SRAM both-bank clear timing: exactly 512 cycles | Trigger clear, count cycles until clear_active deasserts | clear_both=1 (from measure_clear). Bank 0 cleared: addresses 0-255 (256 cycles). Bank 1 cleared: addresses 0-255 (256 cycles). Total = 512 cycles. clear_active deasserts on cycle 513. | On cycle 513, upd_ready goes high (queue can drain). |
| X046 | Queue clear walk timing: exactly 256 cycles | Trigger clear, count cycles until queue clear_active deasserts | Coalescing queue clear_index walks 0 to N_BINS-1 = 255. Takes 256 cycles. clear_active deasserts on cycle 257 (clear_index wraps). | After 257 cycles, queue accepts new hits (drain_fire_c possible). |
| X047 | Clear during config apply pending | Set cfg_apply_pending=1 (write config + apply bit, ingress not yet empty), trigger clear | measure_clear_pulse clears ingress_stage_valid, making ingress_empty_v true. cfg_apply_pending resolves on next cycle (apply completes). Clear and apply interact: apply sees empty ingress, commits config. Config shadow updated to new values. Clear proceeds normally. | Verify new config is active AND pipeline is cleared. Both operations complete. |
| X048 | Interval-triggered clear preserves interval timer phase | Set interval to 1000 clocks. At cycle 500, trigger i_interval_reset (external clear) | measure_clear_pulse fires. pingpong_sram: clear resets timer_count to 0 (via i_clear branch). After clear, interval timer restarts from 0 -- next interval fires at cycle 500+1000 = 1500, not 500+500=1000. The external clear resets the timer. | Verify interval_pulse timing matches restarted timer. |
| X049 | Clear with filter-rejected hits in pipeline | Enable filter (cfg_filter_enable=1), inject hits where some pass and some are rejected, trigger clear mid-pipeline | All pipeline stages cleared regardless of filter status. Rejected hits (ingress_write_req=0) and accepted hits alike are discarded by clear. | Inject new filtered hits after clear; verify correct filter behavior with fresh pipeline. |

---

## 3. Configuration Errors (CER) -- 22 cases

Configuration errors are detected at apply time (CSR[0] write with bit 0 = 1). The error condition is: `bin_width=0 AND right_bound <= left_bound`. When bin_width != 0, right_bound is auto-computed from left_bound + bin_width * N_BINS, so the user cannot set an invalid right_bound in that case. The error flag blocks cfg_apply_request, preventing bad config from reaching the active config shadow.

### 3.1 Error Detection

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X050 | bin_width=0, right_bound < left_bound: error | Write CSR[1]=100 (left), CSR[2]=50 (right), CSR[3]=0 (bin_width), apply | csr_error=1, csr_error_info=0x1. cfg_apply_request does NOT fire. cfg shadow retains previous config. | Read CSR[0]: bit 24 = 1, bits 31:28 = 0x1. |
| X051 | bin_width=0, right_bound = left_bound: error | Write CSR[1]=100, CSR[2]=100, CSR[3]=0, apply | csr_error=1, csr_error_info=0x1. right_bound <= left_bound is true when equal. Apply blocked. | Same as X050. |
| X052 | bin_width=0, right_bound > left_bound: NO error (manual bounds mode) | Write CSR[1]=0, CSR[2]=256, CSR[3]=0, apply | csr_error=0. cfg_apply_request fires. This is the manual-bounds mode where bin_width=0 means "use right_bound as-is" but the bin_divider will flag every hit as overflow (bin_width=0 -> overflow_pipe). The config is technically valid even though every hit overflows. | Inject hit; verify divider_overflow=1 for every hit. |
| X053 | bin_width=1, any bounds: auto-compute right_bound, NO error | Write CSR[1]=-128, CSR[3]=1, apply | right_bound auto-computed: -128 + 1*256 = 128. csr_error=0. cfg_apply_request fires. csr_right_bound updated to 128. | Read CSR[2]; verify right_bound=128. |
| X054 | bin_width=0xFFFF (maximum), auto-compute right_bound | Write CSR[1]=0, CSR[3]=0xFFFF, apply | right_bound = 0 + 65535*256 = 16776960. If this exceeds 32-bit signed range, the to_signed wraps. This is a potential silent config corruption -- verify the actual value stored. | Read CSR[2]; verify value. Inject hit at boundary; verify correct bin assignment. |
| X055 | Negative left_bound with bin_width=0: error conditions | Write CSR[1]=-100 (left), CSR[2]=-200 (right), CSR[3]=0, apply | right_bound (-200) <= left_bound (-100) is true. csr_error=1. | Read CSR[0]; verify error. |
| X056 | Large negative left_bound, right_bound just above: NO error | Write CSR[1]=-2147483648 (INT32_MIN), CSR[2]=-2147483647, CSR[3]=0, apply | right_bound > left_bound. csr_error=0. Valid config for 1-unit range. | Inject hit at -2147483648; verify it lands in bin 0 (if bin_width allows). |

### 3.2 Error Flag Lifecycle

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X057 | Error flag cleared on next valid apply | Set error config (bin_width=0, right<=left), apply -> error. Then write valid config (bin_width=4), apply | On second apply: commit_ok_v=true, csr_error set to 0, csr_error_info set to 0x0 (cleared before error check in same process). cfg_apply_request fires. | Read CSR[0]; verify error=0, error_info=0. |
| X058 | Error flag cleared on apply even if new config also has error | Set error config A, apply -> error. Write different error config B (still bin_width=0, right<=left), apply | csr_error first cleared to 0, then re-set to 1 in the same process. Net result: csr_error=1, csr_error_info=0x1. The clear-then-set is not observable externally (single-cycle). | Read CSR[0]; verify error=1. |
| X059 | Error flag survives multiple CSR writes to non-apply addresses | Set error config, apply -> error. Write CSR[15]=0xABCD (scratch). Write CSR[1]=-500 (left_bound). | csr_error remains 1. Only a new apply (CSR[0] bit 0) can clear it. Non-apply writes do not touch csr_error. | Read CSR[0]; verify error still 1. |
| X060 | Apply without bit 0 set: no apply, no error clear | csr_error=1 from previous bad apply. Write CSR[0]=0x00000000 (bit 0 = 0) | No apply triggered. csr_error remains 1. The mode/filter fields in CSR[0] are updated but apply does not fire. | Read CSR[0]; verify error=1, mode field updated to 0. |
| X061 | Error flag blocks config shadow update | Set error config, apply -> error. Verify config shadow | cfg_left_bound, cfg_bin_width, etc. retain their previous values (from last successful apply or generic defaults). The bad config is in CSR registers but NOT in cfg shadow. | Inject hits; verify they are processed with the old (correct) config, not the error config. |

### 3.3 Config Apply Interaction with Pipeline

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X062 | Apply blocked while ingress not empty | Inject hit (ingress_stage_valid(0)=1), write CSR[0] with apply bit | cfg_apply_pending=1 (CSR[0] bit 1). Apply deferred until ingress_stage_valid all clear. cfg shadow NOT yet updated. | Wait for ingress to drain; verify config shadow updates. Read CSR[0]; verify apply_pending=0. |
| X063 | Apply with all ports sending hits continuously | Start continuous hit stream on all 8 ports, apply new config | cfg_apply_pending=1. ingress_stage_valid never all-zero because hits keep arriving. However, ingress_comb gates port_ready by cfg_apply_pending: when pending=1, stream_ready=0, so new hits are backpressured. Ingress drains, apply completes. | Verify apply completes within bounded time. Hits resume with new config. |
| X064 | Config apply during error: pending flag behavior | Set error config, apply -> error (cfg_apply_request blocked). Verify cfg_apply_pending | cfg_apply_request=0 (blocked by error). cfg_apply_pending should NOT be set to 1 (request never fires). But CSR[0] bit 1 read should show pending=0. | Read CSR[0]; verify bit 1 = 0. |
| X065 | Multiple rapid applies (write apply bit every cycle for 10 cycles) | Write CSR[0] with bit 0 = 1 for 10 consecutive cycles, valid config | cfg_apply_request fires on first cycle. cfg_apply_pending=1. Subsequent writes set cfg_apply_request=1 again but cfg_apply_pending is already 1. Only one apply occurs. Config shadow updated once. | Verify config shadow has expected values. No double-apply corruption. |
| X066 | Config change without apply: no effect on active config | Write CSR[1]=-999 (new left_bound) but never write CSR[0] with apply bit | CSR[1] reads -999. But cfg_left_bound (shadow) retains old value. Hits are binned with old config. | Inject hit; verify bin assignment uses old left_bound, not -999. |
| X067 | Apply during measure_clear_pulse | Trigger clear, write CSR[0] with apply bit on same cycle | measure_clear_pulse clears ingress_stage_valid (makes ingress empty). cfg_apply_pending can resolve immediately. Both clear and apply complete. Config updated AND pipeline cleared. | Verify new config active and histogram clean. |
| X068 | bin_width change from 0 to non-zero: right_bound auto-computed | Initially bin_width=0, right_bound=256 (manual mode). Write bin_width=2, apply | right_bound auto-computed: left_bound + 2*256. The manually-set right_bound=256 is overwritten. | Read CSR[2]; verify right_bound = left_bound + 512. |
| X069 | Right_bound write after bin_width change: overwritten by next apply | Write CSR[2]=1000, write CSR[3]=4, apply | On apply, bin_width != 0, so right_bound = left_bound + 4*256. The CSR[2]=1000 is overwritten in csr_reg. | Read CSR[2]; verify right_bound = left_bound + 1024, NOT 1000. |
| X070 | Key extraction bit range: key_hi < key_lo (degenerate) | Write CSR[4] with update_key_high=5, update_key_low=10, apply | No config error raised (error check only covers bin_width/bounds). extract_unsigned returns garbled data. Hits land in unpredictable bins. This is a silent misconfiguration. | Inject known hit; read histogram. Document expected (possibly wrong) bin assignment. |
| X071 | Filter key matching with filter_reject=1 (inverted filter) | Set filter_enable=1, filter_reject=1, filter_key=0x5. Inject hits with filter field = 0x5 and 0x3 | Hits with field=0x5 are rejected (match + reject = no pass). Hits with field=0x3 are accepted (no match + reject = pass). | Verify total_hits counts only accepted hits. Histogram reflects only non-matching hits. |

---

## 4. FIFO Overflow and Drop (FOD) -- 20 cases

Each of the 8 ingress ports has a 256-deep FIFO (2^FIFO_ADDR_WIDTH, FIFO_ADDR_WIDTH=8). When a FIFO is full and a new hit arrives, the hit is dropped: `drop_pulse` fires, `csr_dropped_hits` is incremented. The FIFO never corrupts -- it simply refuses the write. The key failure mode is inaccurate drop counting when multiple ports overflow simultaneously.

### 4.1 Single-Port Overflow

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X072 | FIFO at depth 256 (exactly full), 1 more hit | Inject 256 hits on port 0 faster than arbiter can drain (back-to-back), then inject hit 257 | fifo_full(0)=1 on hit 256. Hit 257: drop_pulse(0)=1. csr_dropped_hits incremented by 1. Hit 257 data is lost. FIFO level stays at 256. | Drain FIFO (arbiter processes all 256). Inject hit 258; verify FIFO accepts it (full=0). |
| X073 | FIFO at depth 255, simultaneous read and write | FIFO has 255 entries. Arbiter reads 1 (fifo_read=1) on same cycle as new hit write | Simultaneous read+write: level stays 255. FIFO not full, write succeeds. No drop. | Verify level=255, no drop_pulse. |
| X074 | FIFO at depth 256, simultaneous read and write | FIFO has 256 entries. Arbiter reads 1 on same cycle as new hit write | Read decrements level to 255. Write sees level_v (after read) < 256, so write succeeds. Net level = 256. No drop. This is the critical corner: the read makes room for the write in the same cycle. | Verify no drop_pulse. Level still 256. |
| X075 | Burst overflow: 512 hits to port 0 (256 accepted, 256 dropped) | Inject 512 back-to-back hits on port 0, arbiter not draining | First 256 accepted. Hits 257-512: 256 drops. csr_dropped_hits=256. fifo_level_max=256. | Drain FIFO; verify 256 entries. Inject 256 more; all accepted (FIFO recovered). |
| X076 | Drop counter accuracy: 1000 hits to full FIFO | Fill FIFO to 256, then inject 1000 more hits while arbiter is stalled | csr_dropped_hits = 1000. Uses sat_add for multi-port counting but drop_count_v is 4-bit (counts per cycle across all ports). With single-port, max 1 drop/cycle. 1000 cycles of drops. | Verify csr_dropped_hits exactly 1000. |

### 4.2 Multi-Port Overflow

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X077 | All 8 ports overflow simultaneously (1 drop each, same cycle) | Fill all 8 FIFOs to 256, inject 1 hit on all 8 ports simultaneously | drop_pulse for all 8 ports. drop_count_v = 8 (4-bit counter, max 8 fits). csr_dropped_hits += 8. | Verify csr_dropped_hits incremented by exactly 8. |
| X078 | All 8 ports overflow simultaneously (sustained, 100 cycles) | Fill all 8 FIFOs to 256, inject hits on all 8 ports for 100 consecutive cycles | 8 drops per cycle * 100 cycles = 800 total. csr_dropped_hits = 800. drop_count_v = 8 each cycle (fits in 4-bit counter). | Verify csr_dropped_hits = 800. |
| X079 | Mixed: 4 ports overflow, 4 ports accept | Ports 0-3: FIFO full (256). Ports 4-7: FIFO empty. Inject on all 8 simultaneously | Ports 0-3: drop. Ports 4-7: accept. csr_total_hits += 8 (all accepted at ingress). csr_dropped_hits += 4 (drops happen at FIFO write stage, after accept_pulse). Note: accept_pulse fires for all 8 (ingress acceptance), but drop_pulse fires for 4. | Verify total_hits = 8, dropped_hits = 4. Only 4 hits reach histogram. |
| X080 | Port 0 overflow with snoop: backpressure from aso_hist_fill_out_ready=0 | Port 0 is special (snoop path). Set aso_hist_fill_out_ready=0 | port_ready(0) = 0 (stream_ready_v = aso_hist_fill_out_ready when SNOOP_EN). Hits on port 0 are backpressured, not dropped. Port 0 FIFO does not fill because ingress does not accept. | Verify csr_dropped_hits = 0. Port 0 hits are stalled, not lost. |
| X081 | Port 0 overflow: snoop ready, FIFO full | aso_hist_fill_out_ready=1, FIFO(0) full (256 entries). Inject hit on port 0 | Hit accepted by ingress (stream_sampled_v=1, accept_pulse fires). Enters ingress_stage_reg. On next cycle, fifo_full(0)=1, drop_pulse(0)=1. Hit lost at FIFO write stage. | total_hits incremented. dropped_hits incremented. Net histogram count does not increase for that hit. |

### 4.3 FIFO Recovery and Level Tracking

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X082 | FIFO drain after overflow: verify rd_ptr/wr_ptr integrity | Fill FIFO to 256, drop 4 more, then let arbiter drain all 256 | After drain: level=0, empty=1. rd_ptr advanced by 256, wr_ptr advanced by 256 (both wrap). Pointers are consistent. | Inject 256 more hits; verify all 256 are stored correctly (no address aliasing from wrapped pointers). |
| X083 | FIFO level_max tracks peak across overflow | Start empty. Fill to 10, drain to 5, fill to 16, drop 3, drain to 0 | level_max = 16 (peak was full). level_max is only reset by i_rst or i_clear. | Read CSR[10]; verify fifo_pair_max reflects 16. |
| X084 | FIFO level_max reset by measure_clear | level_max=16 from prior activity. Trigger measure_clear | max_reg reset to 0. | Read CSR[10]; verify fifo_pair_max = 0. |
| X085 | FIFO overflow does not corrupt existing entries | Fill FIFO with keys [K0..K15]. Inject K16 (dropped). Drain FIFO | Read-out order: K0, K1, ..., K15. K16 is absent. No existing entry overwritten by the dropped write. The write was blocked (write_v = false when level >= FIFO_DEPTH). | Verify histogram bins match K0..K15 pattern exactly. |
| X086 | FIFO write during read: pointer wrap consistency | FIFO has 1 entry. Read and write simultaneously for 100 cycles (steady state) | Level stays at 1. rd_ptr and wr_ptr both advance by 100, wrapping at 16. Wrap math is modular (ptr width = 4 bits). No overflow, no underflow. | After 100 cycles, drain the 1 remaining entry; verify it is the most recently written key. |

### 4.4 Drop Counter Saturation

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X087 | csr_dropped_hits saturation at 0xFFFFFFFF | Inject enough drops to reach near-max (set via initial value cheat or long run), then 1 more | sat_add: when sum overflows 32 bits, clamp at 0xFFFFFFFF. No wraparound. | Read CSR[12]; verify 0xFFFFFFFF. Inject 100 more drops; still 0xFFFFFFFF. |
| X088 | csr_total_hits saturation at 0xFFFFFFFF | Same as X087 but for total_hits counter | sat_add clamps. | Read CSR[11]; verify 0xFFFFFFFF. |
| X089 | Multiple ports drop on same cycle: drop_count_v accuracy | 5 ports drop simultaneously | drop_count_v = 5 (unsigned 4-bit, max 8). sat_add(csr_dropped_hits, 5). | Verify csr_dropped_hits increases by exactly 5. |
| X090 | Accept and drop on same port, same cycle: impossible by design | Port 0: ingress accepts hit (accept_pulse=1), but FIFO is full (drop_pulse=1). Both fire on same cycle? | accept_pulse fires in ingress_comb from sampled_v. drop_pulse fires from ingress_stage_valid check (one cycle later due to ingress_stage_reg). They cannot fire on the same cycle for the same hit. But accept_stat_pulse and drop_stat_pulse can fire on the same cycle for DIFFERENT hits (hit N accepted at ingress, hit N-1 dropped at FIFO). | Verify counters are consistent: total_hits = accepted (including those later dropped). dropped_hits = FIFO drops only. |
| X091 | Stats counter reset racing with new drops | Trigger measure_clear. On the exact cycle stats_reset_pulse_d1 fires, a drop also occurs (from a hit that entered ingress before clear but reaches FIFO after clear) | stats_reg: stats_reset_pulse_d1 clears counters to 0, then drop_count_v adds to 0. Net: csr_dropped_hits = number of drops on that one cycle. The drop is not lost. | Verify dropped_hits = 1 (or however many drops coincided with reset). |

---

## 5. Coalescing Queue Overflow (CQO) -- 17 cases

The coalescing queue has QUEUE_DEPTH=256 entries and N_BINS=256 bin slots. Each bin has a queued flag and an 8-bit kick counter (max 255). Overflow happens in two ways: (1) queue full + new bin index not yet queued, or (2) kick counter at 255 + new hit to same bin. Both increment `overflow_count`.

### 5.1 Queue Full Overflow

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X092 | Queue at exactly QUEUE_DEPTH, new bin arrives | Inject 256 distinct-bin hits (one per bin, fills queue to 256). Inject 1 more hit to an un-queued bin (impossible since all 256 bins are queued). Actually: all 256 bins are queued. Any new hit to ANY bin will coalesce (queued=1, kick_ram incremented). Queue overflow for a "new bin" can only happen if queue is full and the hit's bin is NOT queued -- but with N_BINS=QUEUE_DEPTH=256, this means all bins are already queued. So this case requires N_BINS > QUEUE_DEPTH or partial queue usage. | With N_BINS=256 and QUEUE_DEPTH=256: if all 256 bins are in queue, any hit coalesces. Queue overflow from "new bin" never happens because N_BINS = QUEUE_DEPTH. | This case documents a design invariant: when N_BINS <= QUEUE_DEPTH, queue-full overflow from new bins is impossible (all bins fit). Test with modified QUEUE_DEPTH=128 generic if needed. |
| X093 | Queue at QUEUE_DEPTH-1, drain fires, new bin arrives same cycle | Queue has 255 entries. drain_fire_c=1 (drains one). New hit to un-queued bin on same cycle | queue_room_c: `(queue_level < QUEUE_DEPTH) or (drain_fire_c = '1')`. With level=255 < 256, room=1 even without drain. New bin accepted. Level goes to 255-1+1=255 (drain reduces by 1, new bin adds 1). | Verify new bin queued, no overflow. |
| X094 | Queue at QUEUE_DEPTH (256), drain fires, new bin arrives same cycle | Queue has 256 entries. drain_fire_c=1. New hit to a bin that was just drained (queued goes from 1 to 0 in drain, then back to 1 from new hit) | drain_fire_c=1 so queue_room_c=1. In queue_reg: drain reduces level to 255, new bin write increments to 256. In gen_kick_bins: drain clears queued, then hit sets queued back to 1 with kick=1. No overflow. | Verify kick_ram for that bin = 1 (reset by drain, set by new hit). |
| X095 | Queue overflow with QUEUE_DEPTH < N_BINS (generic override) | Instantiate with QUEUE_DEPTH=128, N_BINS=256. Inject 129 distinct-bin hits | First 128 fill queue. Hit 129 to new bin: queue_room_c=0, overflow_v incremented. Hit lost. | overflow_count = 1. |

### 5.2 Kick Counter Saturation

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X096 | Kick counter at 255, new hit to same bin | Inject 255 hits to bin 0 (kick_ram(0) = 255). Inject hit 256 to bin 0 | In gen_kick_bins: `kick_ram(0) = KICK_MAX`, so no increment. In queue_reg: `kick_ram(hit_bin_v) = KICK_MAX`, so `overflow_v = sat_inc(overflow_v)`. Hit is lost from count perspective. | overflow_count = 1. Bin 0 histogram count = 255 (from kick_ram drain). |
| X097 | Kick counter saturation + drain on same cycle | kick_ram(0) = 255. drain_fire_c=1 for bin 0 (drains 255). New hit to bin 0 on same cycle | gen_kick_bins: drain clears kick_ram(0) to 0, sets queued_effective to 0. Then hit re-queues with kick=1. No saturation because drain happened first. | overflow_count = 0. Bin 0 gets 255 (from drain) + 1 (new entry) = 256 total in SRAM. |
| X098 | All 256 bins at kick_max=255, new hit to any bin | Fill all bins to 255 kicks each. Inject 1 more hit to bin 42 | kick_ram(42) = KICK_MAX, no increment. overflow_count incremented. | overflow_count = 1. |
| X099 | Kick counter value preserved across multiple drains | Inject 10 hits to bin 0 (kick=10). Let drain fire (outputs count=10). Before queue processes bin 0 again, inject 5 more hits to bin 0 | First drain: count=10 sent to SRAM. Bin 0 re-queued with kick=5 (or queued during drain and accumulated 5). Second drain: count=5. | SRAM bin 0 total = 10 + 5 = 15 (via read-modify-write in pingpong_sram). |

### 5.3 Queue Overflow Counter Behavior

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X100 | overflow_count accuracy over 1000 saturation events | Saturate bin 0 kick to 255, inject 1000 more hits to bin 0 | Each of 1000 hits: kick at max, overflow incremented. overflow_count = 1000. OVERFLOW_WIDTH=16, so 1000 fits. | Read CSR[14]; verify overflow field = 1000. |
| X101 | overflow_count saturation at 2^16 - 1 = 65535 | Inject enough saturation events to reach 65535, then 1 more | sat_inc: clamps at 65535. No wraparound. | Read CSR[14]; verify overflow = 65535. More events do not increase it. |
| X102 | overflow_count reset by measure_clear | overflow_count = 500 from prior activity. Trigger measure_clear | Queue clear resets overflow_count_q to 0 (explicit in i_clear branch). | CSR[14] overflow field = 0. |
| X103 | overflow_count reset by i_rst | overflow_count = 500. Assert i_rst | overflow_count_q reset to 0 (explicit in i_rst branch). | CSR[14] overflow field = 0. |
| X104 | Queue occupancy_max tracking | Inject hits to create queue_level = 200, drain to 50, fill to 230 | queue_level_max = 230 (peak). max only updated when level_v > current max. | CSR[14] occupancy_max field = 230 (clipped to 8-bit via clip_status_byte_f if > 255, but 230 < 255). |
| X105 | Queue occupancy_max clipping to 8-bit status | Queue level reaches 300 (only possible with QUEUE_DEPTH > 255 generic override) | clip_status_byte_f: if value > 255, returns 0xFF. status_queue_occupancy_max_shadow = 0xFF. | CSR[14] max field = 0xFF. |

### 5.4 Queue and Pipeline Interaction

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X106 | Drain blocked by clear_active: hits accumulate | Trigger clear (clear_active=1 for 256 cycles). During clear, inject hits that pass through divider into queue_hit_valid | queue accepts hits (gen_kick_bins and queue_reg skip normal processing during clear_active). Actually: during clear_active, gen_kick_bins only processes clear_index, and queue_reg only processes clear walk. Hits arriving during clear_active are lost (i_hit_valid ignored because else clause not reached). | Verify hits injected during queue clear_active are lost. overflow_count NOT incremented (they are simply not processed). This is a silent loss. |
| X107 | Drain ready gated by SRAM clear | Queue has entries to drain. pingpong_sram clear_active=1 (SRAM clearing). queue_drain_ready=0 (upd_ready_int=0 during SRAM clear) | drain_fire_c=0 because i_drain_ready=0. Queue entries wait. No overflow from drain blockage alone (hits still coalesce if bin already queued). | After SRAM clear completes (upd_ready=1), drain resumes. All queued counts reach SRAM. |
| X108 | Simultaneous hit arrival and drain for different bins | Bin 0 draining (drain_fire_c=1, bin 0). New hit to bin 5 (different bin) | No interaction. Bin 0 drained normally. Bin 5 queued or coalesced normally. | Both operations complete independently. |

---

## 6. Pipeline Flush and Recovery (PFR) -- 17 cases

When measure_clear_pulse fires, it must flush every pipeline stage simultaneously. The critical concern is that the bin_divider has 9 registered stages (1 range-check + 8 quotient stages), and the pipeline from arbiter to queue_hit_pipe adds 4 more stages. A total of 13+ flip-flop stages must be cleared in one cycle.

### 6.1 Flush at Each Pipeline Stage

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X109 | Flush with hit in ingress_stage_reg only | Inject hit, wait 1 cycle (hit in ingress_stage_reg), fire clear | ingress_stage_valid cleared. Hit never reaches FIFO. | Inject new hit; verify it enters FIFO as if pipeline was empty. |
| X110 | Flush with hit in hit_fifo only (ingress drained) | Inject hit, wait 2 cycles (hit written to FIFO, ingress empty), fire clear | FIFO cleared (i_clear to hit_fifo). Hit lost from FIFO. Arbiter cleared. | FIFO level = 0. New hit accepted and processed cleanly. |
| X111 | Flush with hit in rr_arbiter output register | Inject hit, wait until arb_valid=1 (arbiter has granted), fire clear | Arbiter: out_valid_q=0 (cleared by i_clear). Hit lost at arbiter. | New hit; verify arbiter starts fresh round-robin from port 0. |
| X112 | Flush with hit in divider_pipe stage 1 (arb_pipe_valid) | Inject hit, wait until arb_pipe_valid=1, fire clear | arb_pipe_valid=0. key_pipe_valid=0. divider_in_valid=0. All pre-divider pipe stages cleared. | New hit passes through all 3 pre-divider stages cleanly. |
| X113 | Flush with hit in bin_divider stage 4 of 8 | Inject hit, wait 4+3 cycles (3 pre-divider + 4 divider stages), fire clear | All bin_divider valid_pipe stages cleared to 0. The partial quotient computation is discarded. No corrupt bin_index emerges. | New hit enters divider stage 0 cleanly. Verify output is correct. |
| X114 | Flush with hit at bin_divider output (stage 8) | Inject hit, wait full divider latency (11 cycles from ingress), fire clear just before divider output valid | divider_valid may fire on same cycle as clear (race). queue_hit_pipe clears queue_hit_valid on measure_clear_pulse. Even if divider outputs valid, queue_hit_pipe discards it. | No hit reaches coalescing queue. Queue level stays 0. |
| X115 | Flush with hit in queue_hit_pipe register | Inject hit, wait until queue_hit_valid=1, fire clear | queue_hit_valid=0. queue_hit_bin=0. Hit does not reach coalescing_queue i_hit_valid. | Queue unchanged. |
| X116 | Flush with hit in coalescing queue (queued, kick_ram > 0) | Inject hit, wait for it to be queued (queued(bin)=1, kick_ram(bin)=1), fire clear | Queue clear_active=1. clear_index walks 0 to 255, clearing each kick_ram and queued flag. Hit's kick count lost. | After 256-cycle clear walk: kick_ram all 0, queued all 0, level=0. |
| X117 | Flush with SRAM update in write-back stage (upd_write_valid=1) | Inject hit, wait for full pipeline to SRAM write-back, fire clear | upd_write_valid=0 (cleared by i_clear). The SRAM write may or may not have completed on that cycle (depends on timing). SRAM is then cleared by clear walk anyway. | After SRAM clear: all bins read 0. No partial count survives. |

### 6.2 Multi-Hit Pipeline Flush

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X118 | Flush with hits at every pipeline stage simultaneously | Inject 13+ hits in rapid succession (filling all pipeline stages), fire clear | Every stage cleared. 13+ in-flight hits all lost. This is the worst-case data loss from a single clear. | Inject 13 hits after clear; verify all processed correctly with no ghost data from pre-clear pipeline. |
| X119 | Flush with 8 FIFOs full + hits in all stages | Fill all 8 FIFOs to 16 (128 hits total), plus hits in arbiter/divider/queue. Fire clear | 128 FIFO entries + ~13 pipeline entries = ~141 in-flight hits all lost. All FIFOs level=0. All pipeline stages valid=0. | Inject 1 hit per port; verify each processes cleanly. Total count = 8 (not 8+141). |

### 6.3 Pipeline Flush and SRAM Clear Interaction

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X120 | Clear blocks upd_ready for 512 cycles (both-bank clear) | Trigger measure_clear, inject hits immediately | Hits pass through divider and queue within ~20 cycles. But queue_drain_ready = 0 (SRAM clear_active=1, upd_ready_int=0). Queue accumulates entries during the 512-cycle SRAM clear. | After SRAM clear: queue drains. All accumulated hits reach SRAM. No data loss (unless queue overflows). |
| X121 | Clear with non-pingpong mode: burst read deferred | ENABLE_PINGPONG=false. Clear in progress. Host issues burst read | In non-pingpong mode, burst read requires no update pipeline activity. During clear, upd_ready=0. If burst read arrives: it checks upd pipeline idle AND i_upd_valid=0. Burst may be deferred (hist_read_pending=1) or execute if pipeline is idle. | Burst read returns correct data (zeros, since SRAM is being/was cleared). |
| X122 | SRAM clear_both vs single-bank clear timing | measure_clear: clear_both=1, clears bank 0 then bank 1 (512 cycles). Interval swap: clear_both=0, clears only new active bank (256 cycles) | Verify measure_clear takes exactly 512 cycles for clear_active. Interval clear takes exactly 256 cycles. | Time clear_active duration. |
| X123 | SRAM forward path during flush: upd_add/upd_sum/upd_write all zero | Trigger clear, then inject hit immediately after SRAM clear completes | upd_add_valid, upd_sum_valid, upd_write_valid all 0 after clear. No stale forwarding data. First post-clear update reads SRAM (value=0), adds kick count, writes back correctly. | Verify bin count = kick count (starting from 0, no stale forward). |
| X124 | Rapid clear-inject-clear: pipeline never fully recovers | Trigger clear, inject 5 hits during 512-cycle clear window, trigger another clear before first clear completes | Second clear restarts clear walk from address 0. First clear's partially-cleared state is overwritten. 5 hits injected during first clear are in queue (if queue clear already completed by then). Second clear flushes them. | After second clear: fully clean state. No residual from either the 5 hits or the first clear's partial state. |
| X125 | Queue clear completes before SRAM clear: queue accepts hits during SRAM clear | Queue clear takes 256 cycles. SRAM clear takes 512 cycles. Between cycles 257-512: queue clear_active=0 but SRAM clear_active=1 | Queue accepts new hit entries. Queue_drain_ready=0 (SRAM not ready). Hits coalesce in queue. Queue level grows. | After SRAM clear (cycle 513): drain starts. All queued hits reach SRAM. |

---

## 7. Illegal/Unexpected Sequences (ILL) -- 15 cases

These cases cover stimulus patterns that violate assumptions, exploit ambiguous specifications, or probe degenerate edge conditions. The IP must either reject them cleanly or handle them gracefully -- never corrupt state silently.

### 7.1 CSR Interface Abuse

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X126 | CSR write to read-only address (address 6: underflow_count) | Write avs_csr_writedata=0xDEAD to avs_csr_address=6 with avs_csr_write=1 | Silently ignored. csr_reg process has no `when 6` case for writes (falls to `when others => null`). csr_underflow_count unchanged. | Read CSR[6]; verify original value (not 0xDEAD). |
| X127 | CSR write to address > 15 (out of 4-bit range) | avs_csr_address is 4-bit, so max address is 15. If bus presents address=15: valid (scratch). Address out of range is impossible due to port width. | This case documents that the 4-bit address width prevents out-of-range access. No test needed unless Qsys address adapter maps wider addresses. | N/A. |
| X128 | CSR read and write on same cycle, same address | Drive avs_csr_read=1 and avs_csr_write=1 simultaneously for address 15 (scratch) | Write takes effect in csr_reg (synchronous). Read returns the MUX output from csr_read_comb (combinational, sees OLD value of csr_scratch since write hasn't registered yet). csr_readdata_reg captures old value. | Read returns old scratch value. Next read returns new value. |
| X129 | CSR write during i_rst=1 | Assert i_rst=1, drive avs_csr_write=1 for address 15 | csr_reg: i_rst=1 branch takes priority. All CSR registers reset to defaults. Write is lost. | Deassert reset; read CSR[15]; verify 0 (default), not the written value. |

### 7.2 Histogram Bin Interface Abuse

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X130 | Burst read with burstcount=0 | Drive avs_hist_bin_read=1 with avs_hist_bin_burstcount=0 | pingpong_sram: `if ram_v_burst_count = 0 then ram_v_burst_count := 1`. Treats 0-length burst as 1-word read. Returns 1 readdatavalid pulse. | Verify exactly 1 readdatavalid. No hang. |
| X131 | Burst read of full 256 bins | Drive burst read with burstcount=255 (max for 8-bit address), address=0 | 255 readdatavalid pulses (or 256 if burstcount=256 is representable). Reads addresses 0 through 254 sequentially. | Verify all 255 returned values match SRAM contents. |
| X132 | Burst read spanning address wrap (address=250, burstcount=10) | Start at address 250, read 10 | Addresses 250, 251, 252, 253, 254, 255, 0, 1, 2, 3 (wraps due to addr_t modular arithmetic). burst_addr increments with modular wrap. | Verify 10 readdatavalid. Addresses wrap correctly. |
| X133 | hist_bin write with non-zero data (not a clear trigger) | Write avs_hist_bin_writedata=0x12345678 with avs_hist_bin_write=1 | clear_pulse = 0 (writedata != 0x00000000). hist_writeresp_valid fires. No clear triggered. The write has no other effect (there is no data path from hist_bin writedata to SRAM content). | Pipeline undisturbed. Histogram unchanged. |
| X134 | Burst read during non-pingpong mode with update in progress | ENABLE_PINGPONG=false. Hit being processed (upd_issue_valid=1). Host issues burst read | Burst read deferred: non-pingpong mode requires all upd stages idle AND i_upd_valid=0. hist_read_pending=1. Read executes after pipeline drains. | Burst read eventually returns correct data. No bus hang (avs_hist_bin_waitrequest is always 0, but readdatavalid is delayed). |

### 7.3 Hit Injection Edge Cases

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X135 | Hit injection before initial clear completes (flushing=1 after reset) | Deassert i_rst (triggers clear_active=1 for 512 cycles). Immediately inject hits | Ingress accepts hits (ingress_comb does not check flushing). Hits enter FIFOs. Arbiter forwards them. Divider processes them. Queue: clear_active=1 for 256 cycles, so hits arriving at queue are lost (queue_reg in clear_active branch ignores i_hit_valid). SRAM: clear_active=1 for 512 cycles, upd_ready=0. | Hits that arrive at queue during clear_active are silently discarded. Hits that arrive after queue clear (cycle 257) but before SRAM clear (cycle 512) accumulate in queue and drain after SRAM clear. |
| X136 | valid held high for multiple cycles on same port (sustained assertion) | Drive port_valid(0)=1 with same data for 10 consecutive cycles | ingress_comb: accept_pulse fires every cycle where stream_sampled_v=1. If port_ready(0)=1, each cycle is treated as a new hit. 10 hits injected. This is valid behavior (Avalon-ST: valid + ready = transfer each cycle). | total_hits += 10. FIFO receives 10 entries (if not full). |
| X137 | Hit with all-zero data word | Drive port_valid(0)=1, port_data(0)=0x000...0 | Key extracted from all-zero data: depends on key_hi/key_lo fields. Likely key=0. If 0 is within [left_bound, right_bound), it maps to a valid bin. If outside, underflow/overflow. | Verify bin assignment is consistent with config bounds. No crash or hang from zero data. |
| X138 | Hit with all-ones data word (0xFFF...F) | Drive port_valid(0)=1, port_data(0)=0x7FF...F (39 bits all 1) | Key extracted: depends on representation (unsigned vs signed) and bit positions. If key_unsigned=1: large positive number, likely overflow. If key_unsigned=0 (signed): could be -1 or large positive depending on bit range. | Verify correct bin assignment or underflow/overflow flag. |
| X139 | Apply bit set without any config change (re-apply identical config) | Write CSR[0] with apply bit, but all config registers still at defaults | cfg_apply_request fires. cfg_apply_pending=1. Ingress stalled until empty. Config shadow re-written with identical values. No functional change. | Verify operation continues with same behavior. No error, no state corruption. |
| X140 | Simultaneous interval_pulse and measure_clear from different sources | Pingpong timer fires interval_pulse. External i_interval_reset arrives on same cycle | interval_pulse from pingpong timer fires (bank swap starts). measure_clear_comb from i_interval_reset fires. measure_clear_pulse clears entire pipeline including SRAM (clear_both=1). The bank swap from interval_pulse sets clear_active=1 with clear_both=0 in pingpong_sram, but measure_clear overrides it 1 cycle later (i_clear in pingpong_sram sets clear_both=1). | Net result: both banks cleared (measure_clear dominates). active_bank may be toggled by interval then reset by clear. Verify final state is consistent. |

---

## 8. Cross-Tier Compound Scenarios

These cases combine multiple error conditions to verify that the IP handles concurrent faults correctly.

| ID | Scenario | Stimulus | Checker | Recovery Verification |
|----|----------|----------|---------|----------------------|
| X141 | Reset during FIFO overflow (drop_pulse and i_rst on same cycle) | Fill FIFO, inject hit (drop_pulse fires), assert i_rst on same cycle | Reset clears everything. The drop may or may not be counted (depends on whether stats_reg processes it before reset). After reset: all counters 0. | csr_dropped_hits = 0 after reset regardless of pre-reset drop. |
| X142 | Config error + measure_clear + hits in pipeline (triple fault) | Set error config (csr_error=1). Pipeline has hits in-flight. Trigger measure_clear | Clear flushes pipeline. Error flag preserved (CSR value). Hits lost. Config shadow retains previous valid config (error blocked the bad config from being applied). | After clear: csr_error=1, pipeline empty, histogram zeroed, config shadow still valid. |
| X143 | Queue overflow + SRAM clear + new hits (sustained overload) | SRAM clear in progress (upd_ready=0). Queue full (256 entries). New hits arrive | Queue cannot drain (upd_ready=0). New hits to already-queued bins: kick_ram incremented (up to 255). New hits to un-queued bins: overflow (if N_BINS > QUEUE_DEPTH) or coalesce (if bin already queued). Kick saturation: overflow_count incremented. | After SRAM clear: queue drains all 256 entries. Saturated kicks deliver 255 to SRAM. overflow_count reflects total kick saturations. |
| X144 | Reset during queue clear walk (clear_index=128) | Queue clear_active=1, clear_index=128. Assert i_rst | Hard reset overrides queue clear. Restarts clear from index 0. Bins 128-255 from old clear walk are re-cleared by new reset-initiated clear. No gap. | After reset clear completes: all 256 bins have kick_ram=0, queued=0. |
