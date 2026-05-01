# DV Report - histogram_statistics_v2 Phase-6 Gate

**DUT:** `histogram_statistics_v2` &nbsp; **Date:** `2026-05-01` &nbsp; **Scope:** `Phase-6 ping-pong and delay-observability gate`

This dashboard records the hand-authored Phase-5 evidence slice. Broader
generated per-case dashboards are not yet available for this IP.

## Result

PASS. Standalone regression completed with `50 PASS, 0 FAIL`.

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

## Phase-6 Evidence

| status | Case | Evidence |
|:---:|---|---|
| PASS | `P05_pending_read_frozen_bank` | A host read deferred while active-bank updates are in flight still returns the frozen snapshot bin; this catches the Phase-6 live-rate readout bug. |
| PASS | `P04_all_channel_injection_frame` | 8 ports x 32 channels produced 256 accepted hits, 0 dropped hits, 0 coalescing overflow, and all 256 bins matched after bank swap. |
| PASS | `P03_wire_burst_absorb` | Non-backpressured over-depth burst characterized drops and reached active FIFO depth. |
| PASS | `P02_all_ports_soak` | 800 accepted hits across all ports, all 256 bins matched. |
| PASS | `B12_normal_hit_delay_t` | Positive `mode=+1` keeps normal hit_type1 filtering, bins a +512 cycle delay, rejects a wrong-ASIC hit, and sign-extends a wrapped -32 cycle delay. |
| PASS | `B04_version` | `META[VERSION]=0x1a0191f5`, matching release `26.1.9.0501`. |

## Notes

The testbench checks statistics before interval reset or bank-swap clear can
hide a counter state. The all-channel case remains the pre-gate for MuTRiG rate
testing because it proves one injected 8-ASIC frame can be represented without
histogram queue loss. The P05 case is the pre-gate for trusting live 1-second
per-channel rate plots during active MuTRiG traffic. The B12 case is the
pre-gate for the FE SciFi `delay_hit_t` preset, because it verifies the
normal-hit delay key without bypassing ASIC/channel filters.

_Regenerate with `python3 histogram_statistics/tb/scripts/generate_dv_report.py` when a generated dashboard exists; this Phase-5 note is the temporary source of truth._
