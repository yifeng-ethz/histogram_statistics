# Verification Signoff: histogram_statistics_v2

Status: `HISTORICAL SIGNOFF BASELINE PASS; ACTIVE QUESTAONE 2026 FULL TOOLCHAIN VALIDATION PASS`

This signoff records the historical `v7` regression baseline plus a promoted
long-run coverage farm. Those signoff artifacts remain useful evidence for DUT
correctness against `tb/DV_PLAN.md`. The active simulator-tier status on this
host is now simpler: the supported QuestaOne 2026 runtime is in place, and the
current tree has been rerun through a fresh `54 / 54` toolchain-validation pass
on that runtime. A follow-up smoke rerun also passes after the local
`modelsim.ini` write-path fix, so the current flow no longer relies on editing
the shared simulator installation tree.

## Current Refresh: 26.1.7.0501

The 2026-05-01 Phase-6 timing checkpoint reran the deterministic standalone
suite and focused UVM smokes after the debug-source pipeline and release
metadata changes.

| Flow | Command | Result |
|------|---------|--------|
| Standalone deterministic suite | `make -C tb run_all` | `45 PASS, 0 FAIL` |
| Version metadata smoke | `make -C tb run TEST=B04_version SEED=42` | PASS with date `20260501` |
| UVM debug smoke | `make -C tb/uvm run TEST=hist_debug_test SEED=42` | PASS, 0 UVM errors/fatals |
| UVM queue-error smoke | `make -C tb/uvm run TEST=hist_error_queue_test SEED=42` | PASS, 0 UVM errors/fatals |
| UVM QST profile smoke | `make -C tb/uvm run TEST=hist_prof_qst_test SEED=42` | PASS, 0 UVM errors/fatals |

This refresh does not replace the historical full coverage closure below. It
proves that the current timing patch and version metadata remain compatible with
the deterministic and focused simulator gates that protect the Phase-6
regeneration step.

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
  - [tb/uvm/cov_runs/toolchain_validation_2026_04_21/summary.md](tb/uvm/cov_runs/toolchain_validation_2026_04_21/summary.md)
  - [tb/uvm/cov_runs/toolchain_validation_2026_04_21/summary.json](tb/uvm/cov_runs/toolchain_validation_2026_04_21/summary.json)
  - [tb/uvm/cov_runs/signoff_run_v7_fullquesta/summary.md](tb/uvm/cov_runs/signoff_run_v7_fullquesta/summary.md)
  - [tb/uvm/cov_runs/signoff_run_v7_fullquesta/summary.json](tb/uvm/cov_runs/signoff_run_v7_fullquesta/summary.json)
  - [tb/uvm/cov_runs/signoff_longrun_v1/summary.md](tb/uvm/cov_runs/signoff_longrun_v1/summary.md)
  - [tb/uvm/cov_runs/signoff_longrun_v1/suite/suite_trend.csv](tb/uvm/cov_runs/signoff_longrun_v1/suite/suite_trend.csv)
  - [tb/uvm/cov_runs/signoff_longrun_v1/suite/suite_trend.png](tb/uvm/cov_runs/signoff_longrun_v1/suite/suite_trend.png)
  - [tb/waves/README.md](tb/waves/README.md)
  - [tb/waves/index.html](tb/waves/index.html)
  - [tb/waves/manifest.json](tb/waves/manifest.json)

## Environment

- Supported simulator runtime on this host:
  - `/data1/questaone_sim/questasim`
- `make full_questa_probe` passes on the default path and proves working
  `rand`, `constraint`, and `covergroup` execution.
- `tb/uvm/cov_runs/toolchain_validation_2026_04_21` reruns the full `54 / 54`
  suite on the supported QuestaOne 2026 runtime and passes on `2026-04-21`.
- `make -C tb/uvm clean run TEST=hist_smoke_test SEED=1` passes after the local
  writable-`modelsim.ini` fix on `2026-04-21`.
- Intel simulation libraries are now sourced from Quartus `sim_lib` or the
  repo helper flow rather than from the deprecated FE/FSE runtime trees.

Practical interpretation:

- the measured signoff results below are real and rerunnable on the current host
- they are based on the supported QuestaOne 2026 runtime path
- there is no remaining FE/FSE runtime dependency in the active rerun flow
- the active migration validation in this turn is a fresh `54 / 54`
  toolchain-validation rerun, not just a smoke probe
- the post-fix local-`modelsim.ini` smoke confirms the writable work-dir flow
  that the new runtime expects

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

Active toolchain-validation refresh:
[toolchain_validation_2026_04_21](tb/uvm/cov_runs/toolchain_validation_2026_04_21)

- `54 / 54` executed cases pass on the supported QuestaOne 2026 runtime
- merged DUT coverage is `statement=98.36%`, `branch=91.46%`,
  `condition=56.25%`, `expression=84.61%`, `toggle=80.04%`,
  `assertions=69.38%`, `covergroups=100.00%`
- runtime from `summary.md`:
  - total wall `133.56 s`
  - deterministic wall `122.85 s`
  - random wall `10.71 s`

This refreshed run is the current host-validation evidence for the supported
toolchain. The historical `signoff_run_v7_fullquesta` plus long-run promotion
data remain the signoff basis for the promoted coverage-closure argument below.

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

## Waveform Publication

Phase-4 publication collateral is now present in-tree:

- static browser index at [tb/waves/index.html](tb/waves/index.html)
- manifest automation through `make -C tb/uvm wave_index`
- case registration through `tb/uvm/scripts/publish_wave_case.py`
- reproducible VCD capture through `tb/uvm/scripts/run_vcd_case.sh`
- rendered WaveDrom summaries for the published seed cases

Published seed cases:

- `HIST-SMOKE`: deterministic bring-up anchor
- `P141-REAL-TRIAD`: promoted long-run coverage anchor

The VCD files themselves live under `tb/waves/generated/` and are intentionally
git-ignored, but the publication package, summaries, metadata, and GTKWave
template are checked in.

## Remaining Workflow Gaps

The earlier major verification blockers are closed:

- current-tree baseline regression passes
- statement, branch, toggle, and functional coverage targets are met
- long-run promotion is based on measured coverage-vs-wall-clock data
- the promoted routine closure suite is defined and justified
- Phase-4 waveform publication collateral now exists in-tree

Still open:

- no active simulator-tier blocker remains after the QuestaOne migration

## Signoff Conclusion

What can be claimed now:

- `histogram_statistics_v2` is signed off against the project DV plan
- the measured regression and coverage closure are real and reproducible on this
  host

What should not be overstated:

- this does not claim any extra RTL or coverage beyond the recorded evidence
- the simulator/runtime migration is closed on the supported QuestaOne path
