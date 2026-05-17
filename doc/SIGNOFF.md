# ⚠️ Signoff — histogram_statistics_v2 V3 direct input profile

**DUT:** `histogram_statistics_v2` &nbsp; **Date:** `2026-05-17` &nbsp;
**Release under check:** dirty worktree V3 direct Type0/Type1 resource profile

This page is the master signoff dashboard for the current standalone V3
histogram IP slice. Detailed synthesis evidence lives in
[`../syn/SYN_REPORT.md`](../syn/SYN_REPORT.md); detailed DV intent and evidence
live in [`../tb/DV_PLAN.md`](../tb/DV_PLAN.md).

## Legend

✅ pass / closed &middot; ⚠️ partial / caveat &middot; ❌ failed / blocked &middot; ❓ pending &middot; ℹ️ informational

## Health

| status | field | value |
|:---:|---|---|
| ⚠️ | standalone_syn | `2,745` ALMs, `2,310` registers, `2` M10Ks, `0` DSPs; timing closes, but resource is above the requested `<2k ALM` target |
| ✅ | timing | closes the 7.273 ns standalone signoff period; worst slow-85C setup slack `+0.809 ns`, worst hold slack `+0.154 ns` across reported corners |
| ✅ | v3_direct_uvm | Type0 direct 5 ms / 1 ms ping-pong plus Type1 up/down rate and delay pass with zero UVM errors/fatals |
| ✅ | direct_tb_int | FEB V3 direct `tb_int` matrix runs 10 ms with 1 ms ping-pong and passes Type0 rate plus Type1 up/down rate and latency |
| ⚠️ | full_coverage | direct V3 smoke UCDB exists, but full strict merged coverage closure is not claimed in this dirty worktree |

## Verification

| status | area | result | source |
|:---:|---|---|---|
| ✅ | Type0 direct | 5 ms RUNNING, 1 ms ping-pong, 100 kHz one selected channel per ASIC; CSR readback `last_interval_total=1600`, `live_total=16`, `dropped=0`, `coal_status=0x00000400` | `/tmp/hist_v3_direct_q4.log` |
| ✅ | Type1 up rate | `delta_total=256`, `dropped=0`, `coal_status=0x00000100` | `/tmp/hist_v3_direct_q4.log` |
| ✅ | Type1 up delay | `delta_total=256`, `dropped=0`, `coal_status=0x00000100` | `/tmp/hist_v3_direct_q4.log` |
| ✅ | Type1 down rate | `delta_total=256`, `dropped=0`, `coal_status=0x00000100` | `/tmp/hist_v3_direct_q4.log` |
| ✅ | Type1 down delay | `delta_total=256`, `dropped=0`, `coal_status=0x00000100` | `/tmp/hist_v3_direct_q4.log` |
| ✅ | coverage-enabled replay | same direct V3 test passes with UCDB `/tmp/hist_v3_direct_q4.ucdb`; total instance coverage for this focused smoke is `35.87%` | `/tmp/hist_v3_direct_q4_cov.log` |
| ✅ | basic smoke | `hist_smoke_test SEED=7` passes with zero UVM errors/fatals | `/tmp/hist_smoke_q4.log` |
| ✅ | Type0 `tb_int` rate | `100 kHz`, `500 kHz`, and `1 MHz` per ASIC pass for one-random-channel and all-channel patterns; 10 intervals, zero drops | `firmware_builds/systems/v3_pretest-260511-emutype0-dualport-260512/tb_int/REPORT/hist_direct_v3_matrix_20260517_021842_summary.csv` |
| ✅ | Type1 `tb_int` rate | up/down banks pass `100 kHz`, `500 kHz`, and `1 MHz` per ASIC for one-random-channel and all-channel patterns; zero drops | same CSV |
| ✅ | Type1 `tb_int` latency | up/down banks pass latency mode at all rates/patterns; delay bin range is `[128,128]` in the direct timestamp-sideband test | same CSV |

## Synthesis

| status | item | value |
|:---:|---|---|
| ✅ | revision | `histogram_statistics_v2_standalone` |
| ✅ | device | Arria V `5AGXBA7D4F31C5` |
| ✅ | nominal target | `125 MHz` |
| ✅ | signoff constraint | `137.5 MHz` / `7.273 ns` (`1.1 x 125 MHz`) |
| ✅ | slow 85C WNS / TNS | `+0.809 ns` / `0.000 ns` |
| ✅ | slow 0C WNS / TNS | `+0.909 ns` / `0.000 ns` |
| ✅ | worst hold slack | `+0.154 ns` |
| ⚠️ | fitted resources | `2,745 ALMs`, `2,310 regs`, `2 M10Ks`, `10,240` block memory bits, `0 DSPs` |
| ❌ | ALM target | `2,745 > 2,000`; resource closure remains open |
| ℹ️ | largest resource nodes | top self about `1,298.5` ALMs, `pingpong_sram` about `803.8` ALMs, 8x FIFOs about `45-47` ALMs each |

## Evidence Index

- [`../RTL_V3_NOTE.md`](../RTL_V3_NOTE.md) — direct V3 interface contract and resource rationale
- [`../tb/DV_PLAN.md`](../tb/DV_PLAN.md) — standalone DV plan and direct-input evidence
- [`../tb/DV_HARNESS.md`](../tb/DV_HARNESS.md) — UVM direct Type0/Type1 harness notes
- [`../tb/DV_PROF.md`](../tb/DV_PROF.md) — performance/stress plan with V3 resource-profile caveats
- [`../syn/SYN_REPORT.md`](../syn/SYN_REPORT.md) — standalone synthesis report
- [`../syn/quartus/output_files/histogram_statistics_v2_standalone.fit.summary`](../syn/quartus/output_files/histogram_statistics_v2_standalone.fit.summary) — fitter resource summary
- [`../syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary`](../syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary) — TimeQuest summary
- [`../syn/quartus/output_files/worst_setup_paths.rpt`](../syn/quartus/output_files/worst_setup_paths.rpt) — regenerated worst setup paths

## Notes

- The signoff claim is limited to the standalone V3 direct-input IP profile.
- The direct FEB V3 `tb_int` harness now instantiates `histogram_statistics_v2`
  directly and does not use `histogram_ingress_bridge` as stimulus evidence.
- Resource closure is still not done: the next RTL work should reduce the
  `pingpong_sram` bank-valid/pending/readback logic and top-level
  CSR/source-select/readback cone.
