# ✅ DV Report — histogram_statistics_v2 Phase-5 Gate

**DUT:** `histogram_statistics_v2` &nbsp; **Date:** `2026-04-27` &nbsp; **Scope:** `Phase-5 queue-depth gate`

This dashboard records the hand-authored Phase-5 evidence slice. Broader
generated per-case dashboards are not yet available for this IP.

## Legend

✅ pass / closed &middot; ⚠️ partial / known limitation &middot; ❌ failed / blocked &middot; ❓ pending &middot; ℹ️ informational

## Result

✅ Standalone regression completed with `27 PASS, 0 FAIL`.

Command:

```sh
make -C histogram_statistics/tb run_all
```

Tool:

```text
QuestaSim-64 2026.1_1
```

## Delivered Configuration

| Parameter | Value |
|---|---:|
| `N_PORTS` | 8 |
| `CHANNELS_PER_PORT` | 32 |
| `N_BINS` | 256 |
| `FIFO_ADDR_WIDTH` | 8 |
| Per-port FIFO depth | 256 |
| `COAL_QUEUE_DEPTH` | 256 |

## Phase-5 Evidence

| status | Case | Evidence |
|:---:|---|---|
| ✅ | `P03_wire_burst_absorb` | Non-backpressured over-depth burst characterized drops and reached active FIFO depth. |
| ✅ | `P04_all_channel_injection_frame` | 8 ports x 32 channels produced 256 accepted hits, 0 dropped hits, 0 coalescing overflow, and all 256 bins matched after bank swap. |
| ✅ | `P02_all_ports_soak` | 800 accepted hits across all ports, all 256 bins matched. |

## Notes

The testbench checks statistics before interval reset or bank-swap clear can
hide a counter state. The Phase-5 all-channel case is the pre-gate for MuTRiG
rate testing because it proves one injected 8-ASIC frame can be represented
without histogram queue loss.

_Regenerate with `python3 histogram_statistics/tb/scripts/generate_dv_report.py` when a generated dashboard exists; this Phase-5 note is the temporary source of truth._
