# BUG_HISTORY.md - histogram_statistics_v2 bug ledger

This ledger records Phase-5 debug issues for `histogram_statistics_v2`.

## 2026-05-11

### `BUG-002-R` Standalone STA regression at VERSION 26.1.6.0429

- Status: REGRESSION.
- First seen in: fresh standalone Quartus signoff from HEAD `c035c35a96478387e21d692c2a17554bcff20806` at the 1.1x F_target corner.
- Symptom: Slow 1100mV 85C setup slack is `-0.234 ns` on `i_clk` at the `7.273 ns` standalone signoff period.
- Note: Failed standalone signoff at corner Slow 1100mV 85C after VERSION `26.1.6.0429` - integration must apply SDC false_path only if the arc is architecturally false, or the IP must register-split the path.
- Failing path: from `queue_hit_bin[3]` to `coalescing_queue:queue_inst|overflow_count_q[0]`; data delay `7.429 ns`; clock skew `-0.078 ns`; logic depth `7`.
- Corner summary: Slow 1100mV 85C setup `-0.234 ns`; Slow 1100mV 0C setup `0.036 ns`; Fast 1100mV 85C setup `2.459 ns`; Fast 1100mV 0C setup `2.950 ns`.
- Evidence: `syn/quartus/compile_histogram_statistics_v2_standalone_20260511.log`; `syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary`; `syn/quartus/output_files/histogram_statistics_v2_standalone.worst_setup_paths.rpt`.
- Fix status: open. Newest version fails standalone signoff; no RTL fix, false-path exception, VERSION downgrade, or older-build fallback was applied in this record.

## 2026-04-27

### `BUG-001-R` Phase-5 queue-depth timing miss after FIFO expansion

- First seen in: Phase-5 standalone Quartus signoff after raising the ingress FIFO default to 256 entries.
- Symptom: slow-85 standalone setup slack missed timing on the histogram critical path.
- Root cause: the expanded FIFO/filter hot path and coalescing peak-level accounting left too much logic between registers.
- Fix status: fixed; registered the FIFO head in `hit_fifo`, replicated filter fields per ingress port, and updated `coalescing_queue` peak occupancy from the registered queue level.
- Evidence: standalone Quartus closes at slow-85 setup slack +0.129 ns; standalone DV remains `27 PASS, 0 FAIL`.
