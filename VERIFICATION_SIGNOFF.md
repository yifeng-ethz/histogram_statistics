# Verification Signoff: histogram_statistics_v2

Status: `PASS AGAINST DV_PLAN WITH WORKFLOW GAPS`

This signoff uses the current-tree `v7` regression baseline plus a promoted
long-run coverage farm. The DUT correctness and code-coverage gates are closed
against `tb/DV_PLAN.md`. The remaining open items are workflow/environment
gaps: exact non-Starter simulator-tier closure on this host, and Phase-4
waveform publication.

## Scope

- DUT: `histogram_statistics_v2`
- UVM harness: [tb/uvm](tb/uvm)
- Baseline regression driver:
  [tb/uvm/scripts/run_hist_cov_regression.py](tb/uvm/scripts/run_hist_cov_regression.py)
- Long-run farm drivers:
  - [tb/uvm/scripts/run_hist_cov_trend.py](tb/uvm/scripts/run_hist_cov_trend.py)
  - [tb/uvm/scripts/merge_hist_cov_suite.py](tb/uvm/scripts/merge_hist_cov_suite.py)
  - [tb/uvm/scripts/run_hist_cov_farm.py](tb/uvm/scripts/run_hist_cov_farm.py)
- Primary artifacts:
  - [tb/uvm/cov_runs/signoff_run_v7_fullquesta/summary.md](tb/uvm/cov_runs/signoff_run_v7_fullquesta/summary.md)
  - [tb/uvm/cov_runs/signoff_run_v7_fullquesta/summary.json](tb/uvm/cov_runs/signoff_run_v7_fullquesta/summary.json)
  - [tb/uvm/cov_runs/signoff_longrun_v1/summary.md](tb/uvm/cov_runs/signoff_longrun_v1/summary.md)
  - [tb/uvm/cov_runs/signoff_longrun_v1/suite/suite_trend.csv](tb/uvm/cov_runs/signoff_longrun_v1/suite/suite_trend.csv)
  - [tb/uvm/cov_runs/signoff_longrun_v1/suite/suite_trend.png](tb/uvm/cov_runs/signoff_longrun_v1/suite/suite_trend.png)

## Environment

- Installed simulators on this host:
  - `/data1/intelFPGA_pro/23.1/questa_fse`
  - `/data1/intelFPGA_pro/23.1/questa_fe`
- Default rerunnable path in [tb/uvm/Makefile](tb/uvm/Makefile) is still
  `questa_fse`, because that is the verified working runtime on this host.
- `make full_questa_probe` passes on the default path and proves working
  `rand`, `constraint`, and `covergroup` execution.
- `make run TEST=hist_smoke_test SEED=111` passes on the default path after the
  FE-aware Makefile/script cleanup.
- `questa_fe` is installed and reports the correct Edition banner, and
  `mtiverification` is check-outable through `lmutil lmdiag`, but live `vsim`
  startup still fails with `Invalid license environment` on this host.

Practical interpretation:

- the measured signoff results below are real and rerunnable on the current host
- they are based on the working `questa_fse` runtime path
- exact compliance with the stricter external DV workflow remains open until the
  `questa_fe` runtime checkout problem is resolved

## Baseline Result

Official current-tree baseline:
[signoff_run_v7_fullquesta](tb/uvm/cov_runs/signoff_run_v7_fullquesta)

- `54 / 54` executed cases pass
- `0` simulator failures
- `0` `UVM_ERROR`
- `0` `UVM_FATAL`

Runtime from `summary.json`:

| Bucket | Wall Time |
|--------|----------:|
| Deterministic baseline | `359.80 s` |
| Full `v7` regression | `387.98 s` |

Deterministic merged DUT coverage:

| Metric | Result |
|--------|-------:|
| Statement | `98.36%` |
| Branch | `91.46%` |
| Toggle | `66.45%` |
| Covergroups | `99.25%` |
| Total filtered view | `71.57%` |

This baseline already closed statement and branch. Toggle closure still required
promotion of at least one long-run case.

## Long-Run Farm

Promoted coverage-farm plan is documented in [tb/DV_PROF.md](tb/DV_PROF.md)
under `P141-P148`.

Executed farm: [signoff_longrun_v1](tb/uvm/cov_runs/signoff_longrun_v1)

- baseline input: `signoff_run_v7_fullquesta` deterministic merged UCDB
- farm mode: `jobs=2`
- trend cases:
  - `P141_real_triad`
  - `P142_real_boundary`
  - `P143_real_queue`
  - `P144_real_interval`

The farm uses `delta_total_pct_per_s` as an engineering proxy for incremental
observability gain per wall-clock second. It is not literal Fisher information,
but it is the right closure metric here: once merged structural coverage stops
moving, extra runtime has hit a plateau.

Per-case standalone trend endpoints:

| Case | Final Point | Cum Wall s | Total | Toggle | Final Gain %/s | Standalone Assessment |
|------|------------:|-----------:|------:|-------:|---------------:|-----------------------|
| `P141_real_triad` | `2048` | `43.59` | `58.15%` | `51.79%` | `0.014` | plateau |
| `P142_real_boundary` | `2048` | `20.32` | `33.34%` | `26.83%` | `0.007` | plateau |
| `P143_real_queue` | `2048` | `21.70` | `28.65%` | `17.92%` | `0.007` | plateau |
| `P144_real_interval` | `1024` | `20.46` | `55.42%` | `52.17%` | `0.148` | productive standalone |

Most important standalone observations:

- `P141_real_triad` keeps adding useful toggle coverage through `1024` hits and
  clearly plateaus by `2048`
- `P142_real_boundary` and `P143_real_queue` give an early burst of value, then
  flatten quickly
- `P144_real_interval` remains productive in isolation, but that does not mean
  it stays useful after earlier promoted cases are merged in

## Promotion Decision

Merged suite trend from
[suite_trend.csv](tb/uvm/cov_runs/signoff_longrun_v1/suite/suite_trend.csv):

| Step | Case | Cum Wall s | Total | Toggle | Delta Total | Delta Toggle | Gain %/s |
|-----:|------|-----------:|------:|-------:|------------:|-------------:|---------:|
| `0` | baseline deterministic | `359.80` | `71.57%` | `66.45%` | `0.00` | `0.00` | `0.000` |
| `1` | `P141_real_triad` | `403.40` | `73.97%` | `80.41%` | `2.40` | `13.96` | `0.055` |
| `2` | `P142_real_boundary` | `423.71` | `74.30%` | `81.55%` | `0.33` | `1.14` | `0.016` |
| `3` | `P143_real_queue` | `445.41` | `74.56%` | `82.21%` | `0.26` | `0.66` | `0.012` |
| `4` | `P144_real_interval` | `465.87` | `74.66%` | `82.21%` | `0.10` | `0.00` | `0.005` |

Promotion conclusion:

- `P141_real_triad` is the only long-run case required for routine closure. It
  alone closes the toggle target, taking toggle coverage from `66.45%` to
  `80.41%`.
- `P142_real_boundary` and `P143_real_queue` are valid publication/signoff
  extension cases, but their merged gain-per-second is already below the level
  that justifies putting them into the routine suite.
- `P144_real_interval` is not promoted into the routine long-run suite. It is
  still useful as a targeted interaction/debug case, but it adds effectively no
  new merged toggle coverage after the earlier promoted cases.

Routine promoted closure suite:

1. `signoff_run_v7_fullquesta` deterministic baseline
2. `P141_real_triad` at `2048` hits

Extended publication-only suite:

1. routine promoted closure suite
2. `P142_real_boundary`
3. `P143_real_queue`

Stop condition:

- the farm is on a clear merged plateau after `P143`
- `P144` confirms that more runtime without a new stimulus idea is no longer
  buying new structural information

## Final Coverage Statement

Minimal promoted closure point:

- statement `98.36%`
- branch `91.46%`
- toggle `80.41%`
- covergroups `100.00%`

Extended publication point:

- statement `98.36%`
- branch `91.46%`
- toggle `82.21%`
- covergroups `100.00%`

The `tb/DV_PLAN.md` gates are therefore closed at the minimal promoted closure
point.

## Residual Uncovered Space

Remaining misses are concentrated in configuration-locked or mode-locked space:

- generic-disabled branches such as alternate snoop/packet modes
- condition combinations around mutually exclusive config/apply timing
- no FSM coverage objects reported for this DUT configuration

These misses do not block signoff against the project coverage targets. Future
work, if needed, should be UNR/exclusion review or alternate-generic harnesses,
not blind runtime growth.

## Remaining Workflow Gaps

The earlier major verification blockers are closed:

- current-tree baseline regression passes
- statement, branch, toggle, and functional coverage targets are met
- long-run promotion is based on measured coverage-vs-wall-clock data
- the promoted routine closure suite is defined and justified

Still open:

- exact non-Starter simulator-tier closure on this host
  - `questa_fe` is installed but live runtime checkout still fails
- Phase-4 waveform publication
  - VCD/GTKWave/WaveDrom handoff is not yet produced

## Signoff Conclusion

What can be claimed now:

- `histogram_statistics_v2` is signed off against the project DV plan
- the measured regression and coverage closure are real and reproducible on this
  host

What should not be overstated:

- this is not yet exact closure of the stricter external DV workflow
- the blocking gaps are the `questa_fe` runtime checkout issue and missing
  waveform-publication collateral
