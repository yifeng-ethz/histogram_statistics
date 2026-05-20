# histogram_statistics — BUG_HISTORY

## B-HIST-01 — TYPE0 ingress only sampled port 0 (2026-05-20)
The per-port ingress loop gated the TYPE0 stream sample on `idx = 0`, so lanes
1..7 were never sampled even though the 8-port FIFO/rr_arbiter infrastructure was
wired. On silicon only ASIC0 (bins 0-31) ever populated; lanes 1-7 egressed from
the arbiter (~13.6M hits, 0 drops) but the histogram never sampled them.
Fix: added an `(idx > 0) and cfg_source_select = HIST_SOURCE_TYPE0_CONST` readyless
sample branch. All 8 ASIC lanes now bin.

## B-HIST-02 — TYPE0 key double-encoded the ASIC via port_offset (2026-05-20)
`key_pipe <= arb_pipe_key + arb_port*CHANNELS_PER_PORT`, but the TYPE0 key
`data[43:36]` already carries ASIC<<5|CH (both the real frame_rcv_ip path and the
emulator pack the asic into data[44:41]). For the 8 independent per-lane ports the
ASIC was added twice, collapsing the bin. Masked before because only port 0 was
sampled (offset 0). Fix: zero `port_offset` for `cfg_source_select = TYPE0`.

## B-HIST-03 — pingpong held waitrequest across bank swap -> sc_hub 0xEEEEEEEE (2026-05-21)
A host burst read of the hist_bin ping-pong SRAM that straddled an interval bank
swap parked in `hist_read_pending` and held `waitrequest` 258+ cycles, exceeding the
FEB sc_hub's ~200-cycle downstream-read watchdog -> RD_TIMEOUT -> RD_PADDING ->
0xEEEEEEEE padding of the SC reply. Fix: seamless bank-follow — the read source bank
tracks `hist_wait_bank` every cycle, the `hist_wait_bank_busy`/`hist_read_pending`
stalls are removed from `waitrequest`, and an in-flight beat that read the now-
clearing bank is squashed and reissued against the just-frozen bank. A swap-
straddling burst returns all burstcount beats valid (interval N before the seam,
N+1 after), 0-cycle wait-hold, no SLVERR. Silicon-validated: 8 live bursts, 0
0xEEEEEEEE, 256/256 bins, hub ERR_FLAGS=0.
