# BUG_HISTORY.md — histogram_statistics_v2 bug ledger

This ledger records Phase-5 debug issues for `histogram_statistics_v2`.

## 2026-04-27

### `BUG-001-R` Phase-5 queue-depth timing miss after FIFO expansion

- First seen in: Phase-5 standalone Quartus signoff after raising the ingress FIFO default to 256 entries.
- Symptom: slow-85 standalone setup slack missed timing on the histogram critical path.
- Root cause: the expanded FIFO/filter hot path and coalescing peak-level accounting left too much logic between registers.
- Fix status: fixed; registered the FIFO head in `hit_fifo`, replicated filter fields per ingress port, and updated `coalescing_queue` peak occupancy from the registered queue level.
- Evidence: standalone Quartus closes at slow-85 setup slack +0.129 ns; standalone DV remains `27 PASS, 0 FAIL`.

## 2026-05-01

### `BUG-002-R` Pending ping-pong host reads selected the active bank

- First seen in: Phase-6 real-MuTRiG 100 kHz/channel rate sweep, where the live
  last-interval counter was near the expected 25.6 Mhit/s but the per-channel
  JTAG bin dump alternated between about 100 kHz and partial-current-interval
  bins around 5 kHz.
- Symptom: low-rate bins moved with read order and increased with active
  traffic, which is not a stable physical channel mask or MuTRiG tuning
  signature.
- Root cause: `pingpong_sram` deferred host reads correctly latched the host
  address/count after an active update pipeline drained, but in ping-pong mode
  they selected `ram_v_next_bank`, the active write bank, instead of
  `not ram_v_next_bank`, the frozen snapshot bank.
- Fix status: fixed in release `26.1.8.0501`; the deferred-read path now uses
  the same frozen-bank selection as the immediate read path when
  `ENABLE_PINGPONG=true`.
- Evidence: `make -C tb run_all SEED=42` passes `46 PASS, 0 FAIL`, including
  `P05_pending_read_frozen_bank`; `make -C tb run TEST=B04_version SEED=42`
  reports `META[VERSION]=0x1a0181f5`; Questa static screen passes with lint
  error `0`, CDC violations `0`, and RDC violations `0`; standalone Quartus
  closes at the 7.273 ns signoff period with slow-85 setup slack `+0.210 ns`
  and worst hold slack `+0.149 ns`.

### `BUG-003-R` Header-sync delay plots used `ts_delta` instead of hit latency

- First seen in: Phase-6 all-ASIC header-sync delay contact sheet review.
- Symptom: several per-ASIC delay plots were centered at or near zero cycles.
  A zero-latency rbCAM hit is physically impossible, so the plot was measuring
  the wrong quantity.
- Root cause: the negative debug histogram modes consumed MTS `ts_delta`,
  which is an inter-hit timestamp delta, not
  `counter_gts_8n - selected_timestamp`.
- Fix status: fixed in release `26.1.9.0501` for the normal hit path. Positive
  `mode=+1` derives the key from normal hit_type1 traffic as
  `local_run_counter[12:0] - hit_type1.data[29:17]`, keeping ASIC/channel
  filters active and allowing all eight ASIC lanes to be scanned together.
  The legacy `-1`/`-2` debug modes remain for direct MTS `debug_ts`
  comparison after the system Qsys patch routes those sources.
- Evidence: `make -C tb run TEST=B12_normal_hit_delay_t SEED=42` passes the
  +512 cycle, wrong-ASIC reject, and wrapped -32 cycle checks.
  `make -C tb run_all SEED=42` passes `50 PASS, 0 FAIL`.
  Questa static screen passes with lint/CDC/RDC reports under
  `/data3/yifeng/mu3e_ip_dev/qverify/histogram_statistics_20260501/histogram_v2_static_release_26_1_9_delay_t_clean`.
  Standalone Quartus full compile has 0 errors and a small slow-85 setup miss
  of `-0.051 ns` at the 7.273 ns over-constrained wrapper period; the violating
  paths are debug/config decode paths, not the normal hit-data RAM write path.
