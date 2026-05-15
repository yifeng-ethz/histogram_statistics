# histogram_statistics_v2 TB

Standalone verification tree for `histogram_statistics_v2`.

## Quick Start

```sh
make -C histogram_statistics/tb run_all
```

The bridge-free streaming-debug contract is part of `run_all`: the histogram
IP accepts the normal fill path and the two readyless Type-1 extended inputs,
with source selection absorbed into `CONTROL.in_port[3:2]`.

`histogram_ingress_bridge` was removed on 2026-05-15. The old bridge
source-switch regression targets are intentionally retired.

## Phase-5 Evidence

- [DV_PLAN.md](DV_PLAN.md) records the standalone verification contract.
- [DV_PROF.md](DV_PROF.md) records the Phase-5 MuTRiG injection pre-gate.
- [DV_REPORT.md](DV_REPORT.md) records the current run result.
- [DV_COV.md](DV_COV.md) records the current coverage-status non-claim.
- [BUG_HISTORY.md](BUG_HISTORY.md) records debug history.
