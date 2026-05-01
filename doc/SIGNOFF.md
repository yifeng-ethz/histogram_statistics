# ✅ Signoff — histogram_statistics_v2 Phase-6 Histogram Debug Timing

**DUT:** `histogram_statistics_v2` &nbsp; **Date:** `2026-05-01` &nbsp; **Release under check:** `26.1.7.0501`

This page is the master signoff dashboard for the Phase-6 histogram statistics
debug-timing and global-key update.

## Legend

✅ pass / closed &middot; ⚠️ partial / known limitation &middot; ❌ failed / blocked &middot; ❓ pending &middot; ℹ️ informational

## Decision

✅ The Phase-6 histogram statistics IP checkpoint is ready to regenerate into
`system_20260427_testplanphase5`.

## Evidence

| status | Layer | Evidence |
|:---:|---|---|
| ✅ | Model | `histogram_statistics/model/scripts/run_queue_tlm_rtl.sh`: TLM/RTL queue comparison passed with 1190 cycles and 0 mismatches. |
| ✅ | Standalone DV | `make -C tb run_all`: `45 PASS, 0 FAIL`; `make -C tb run TEST=B04_version SEED=42`: release metadata passed with date `20260501`. |
| ✅ | Focused UVM smoke | `hist_debug_test`, `hist_error_queue_test`, and `hist_prof_qst_test` passed at `SEED=42` with 0 UVM errors/fatals. |
| ✅ | Static screen | `questa_static_screen.py`: PASS for release `26.1.7.0501`; transcript under `/data3/yifeng/mu3e_ip_dev/qverify/histogram_statistics_20260501/histogram_v2_static_release_26_1_7/`. |
| ✅ | Standalone synthesis | [SYN_REPORT.md](../syn/SYN_REPORT.md): slow-85 setup slack +0.210 ns at 7.273 ns. |
| ⚠️ | Active Qsys generation | `qsys-generate scifi_datapath_system_v3_pipe.qsys --synthesis=VHDL --simulation=VHDL` returned warning code 3, but the report completed with `qsys-generate succeeded`. |
| ✅ | Integration simulation | DP E2E: `7 PASSED, 0 FAILED`, histogram total 506, dropped 0. |

## Phase-6 Contract

The delivered configuration uses:

```text
FIFO_ADDR_WIDTH = 8
per-port FIFO depth = 256
COAL_QUEUE_DEPTH = 256
N_PORTS = 8
CHANNELS_PER_PORT = 0 or 32
```

`CHANNELS_PER_PORT=32` remains the normal local-channel MuTRiG view.
`CHANNELS_PER_PORT=0` means the stream key is already global, which is the
Phase-6 `{ASIC, channel}` histogram view required to check upper and lower FEB
lanes without collapsing ASIC identity.

Any future build reducing FIFO depth, coalescing queue depth, or debug-source
pipelining must rerun model, standalone DV, static screen, synthesis, and
active-system integration gates before rate or delay data is trusted.
