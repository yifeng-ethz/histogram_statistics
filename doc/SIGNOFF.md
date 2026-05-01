# ⚠️ Signoff — histogram_statistics_v2 Phase-6 Delay Observability

**DUT:** `histogram_statistics_v2` &nbsp; **Date:** `2026-05-01` &nbsp; **Release under check:** `26.1.9.0501`

This page is the master signoff dashboard for the Phase-6 histogram statistics
debug-timing, global-key, and normal-hit delay update.

## Legend

✅ pass / closed &middot; ⚠️ partial / known limitation &middot; ❌ failed / blocked &middot; ❓ pending &middot; ℹ️ informational

## Decision

⚠️ The Phase-6 histogram statistics IP checkpoint is ready to regenerate into
`system_20260427_testplanphase5` for FEB debug firmware, with one explicit
timing caveat: standalone Arria V 7.273 ns over-constraint has slow-85 setup
WNS `-0.051 ns` on debug/config decode. The full FEB compile result is tracked
in the system HTML report before programming.

## Evidence

| status | Layer | Evidence |
|:---:|---|---|
| ✅ | Model | `histogram_statistics/model/scripts/run_queue_tlm_rtl.sh`: TLM/RTL queue comparison passed with 1190 cycles and 0 mismatches. |
| ✅ | Standalone DV | `make -C tb run_all SEED=42`: `50 PASS, 0 FAIL`; `B12_normal_hit_delay_t` verifies positive `mode=+1` delay-T binning and ASIC filtering. |
| ✅ | Focused UVM smoke | `hist_debug_test`, `hist_error_queue_test`, and `hist_prof_qst_test` passed at `SEED=42` with 0 UVM errors/fatals. |
| ✅ | Static screen | `questa_static_screen.py`: PASS for release `26.1.9.0501`; transcript under `/data3/yifeng/mu3e_ip_dev/qverify/histogram_statistics_20260501/histogram_v2_static_release_26_1_9_delay_t_clean/`. |
| ⚠️ | Standalone synthesis | [SYN_REPORT.md](../syn/SYN_REPORT.md): full compile 0 errors; slow-85 setup WNS `-0.051 ns` at 7.273 ns. |
| ✅ | Active Qsys generation | `scifi_datapath_system_v3_pipe` and `feb_system_v3_pipe` regenerated with no `Error:` or `Critical Warning:` entries in the 26.1.9 logs. |
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

Positive `mode=+1` is the normal-hit delay mode for FE SciFi:

```text
delay_key = local_run_counter[12:0] - hit_type1.data[29:17]
```

It preserves the normal ingress path and the configured ASIC/channel filter.
Negative modes `-1` and `-2` remain debug-source comparisons for upper and
lower MTS `debug_ts`.

Any future build reducing FIFO depth, coalescing queue depth, or debug-source
pipelining, or normal-hit delay timing must rerun model, standalone DV, static
screen, synthesis, and active-system integration gates before rate or delay data
is trusted.
