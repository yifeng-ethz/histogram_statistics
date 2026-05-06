# BUG_HISTORY.md — histogram_statistics_v2 bug ledger

This ledger records Phase-5 debug issues for `histogram_statistics_v2`.

## 2026-04-27

### `BUG-001-R` Phase-5 queue-depth timing miss after FIFO expansion

- First seen in: Phase-5 standalone Quartus signoff after raising the ingress FIFO default to 256 entries.
- Symptom: slow-85 standalone setup slack missed timing on the histogram critical path.
- Root cause: the expanded FIFO/filter hot path and coalescing peak-level accounting left too much logic between registers.
- Fix status: fixed; registered the FIFO head in `hit_fifo`, replicated filter fields per ingress port, and updated `coalescing_queue` peak occupancy from the registered queue level.
- Evidence: standalone Quartus closes at slow-85 setup slack +0.129 ns; standalone DV remains `27 PASS, 0 FAIL`.

## 2026-05-06

### `BUG-002-R` Histogram tap backpressured the primary pre-hit-stack path

- First seen in: `tb_int` PROF-INT-002 full-pipeline smoke while debugging the missing pre-rbCAM/post-rbCAM/FEB latency plots.
- Symptom: MTS accepted hits from the mux but emitted no type-1 hits; the pre-rbCAM monitor stayed at zero even though MTS and rbCAM CSR readbacks showed RUNNING/GO.
- Root cause: `histogram_ingress_bridge` tied `asi_pre_ready` to the histogram sink readiness when the pre source was selected, so an unready histogram tap could stall the primary MTS-to-hit-stack path.
- Fix status: fixed; `pre_out` now depends only on the primary downstream ready, and the histogram pre source is a lossy tap when the histogram consumer is not ready.
- Evidence: `tb_histogram_ingress_bridge` now includes a directed not-ready pre-tap regression; full `tb_int` evidence is tracked in the integration BUG_HISTORY.
