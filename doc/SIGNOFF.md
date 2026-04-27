# ✅ Signoff — histogram_statistics_v2 Phase-5 Queue Depth

**DUT:** `histogram_statistics_v2` &nbsp; **Date:** `2026-04-27` &nbsp; **Release under check:** `Phase-5 active system`

This page is the master signoff dashboard for the Phase-5 histogram statistics
queue-depth update.

## Legend

✅ pass / closed &middot; ⚠️ partial / known limitation &middot; ❌ failed / blocked &middot; ❓ pending &middot; ℹ️ informational

## Decision

✅ The Phase-5 histogram statistics IP is ready to use as the MuTRiG rate and
delay observation surface in `system_20260427_testplanphase5`.

## Evidence

| status | Layer | Evidence |
|:---:|---|---|
| ✅ | Model | `histogram_statistics/model/scripts/run_queue_tlm_rtl.sh`: TLM/RTL queue comparison passed with 1190 cycles and 0 mismatches. |
| ✅ | Standalone DV | [DV_REPORT.md](../tb/DV_REPORT.md): `27 PASS, 0 FAIL`. |
| ✅ | Standalone synthesis | [SYN_REPORT.md](../syn/SYN_REPORT.md): slow-85 setup slack +0.129 ns at 7.273 ns. |
| ⚠️ | Active Qsys generation | `qsys-generate scifi_datapath_system_v3_pipe.qsys --synthesis=VHDL --simulation=VHDL` returned warning code 3, but the report completed with `qsys-generate succeeded`. |
| ✅ | Integration simulation | DP E2E: `7 PASSED, 0 FAILED`, histogram total 506, dropped 0. |

## Phase-5 Contract

The delivered configuration uses:

```text
FIFO_ADDR_WIDTH = 8
per-port FIFO depth = 256
COAL_QUEUE_DEPTH = 256
N_PORTS = 8
CHANNELS_PER_PORT = 32
```

This covers the all-channel MuTRiG injection bound of one 32-channel hit frame
from each of eight MuTRiG links. Any future build reducing FIFO or coalescing
queue depth must rerun the model, standalone DV, synthesis, and active-system
integration gates before rate data is trusted.
