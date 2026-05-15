# histogram_statistics_v2 TB

Standalone verification tree for `histogram_statistics_v2`.

## Quick Start

```sh
make -C histogram_statistics/tb run_all
```

Histogram ingress bridge source-switch regression:

```sh
make -C histogram_statistics/tb run_ingress_bridge_switch_observed
make -C histogram_statistics/tb run_ingress_bridge_switch_contract
```

The observed target records the current pending-switch behavior. The contract
target is expected to fail until the bridge source switch no longer treats
FEB pre-rbCAM run-level packet activity as a beat-level switch hazard.

## Phase-5 Evidence

- [DV_PLAN.md](DV_PLAN.md) records the standalone verification contract.
- [DV_PROF.md](DV_PROF.md) records the Phase-5 MuTRiG injection pre-gate.
- [DV_REPORT.md](DV_REPORT.md) records the current run result.
- [DV_COV.md](DV_COV.md) records the current coverage-status non-claim.
- [BUG_HISTORY.md](BUG_HISTORY.md) records debug history.
