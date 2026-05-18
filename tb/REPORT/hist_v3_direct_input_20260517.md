# hist_v3_direct_input_20260517

## Command

```sh
make -C histogram_statistics/tb/uvm run TEST=hist_v3_direct_input_test SEED=7
```

## Result

Status: FAIL as standalone DV closure evidence.

The V3 direct-input test observes payload hits on all required direct sources,
but the simulator reports assertion errors from `hist_pipeline_sva`.

Key payload observations from the rerun:

| Source | Mode | Observation |
|---|---|---|
| Type0 | rate | `last_interval_total=1600`, `live_total=8`, `dropped=0`, `coal_status=0x00000100` |
| Type1 up | rate | `delta_total=256`, `dropped=0`, `coal_status=0x00000100` |
| Type1 up | delay | `delta_total=256`, `dropped=0`, `coal_status=0x00000100` |
| Type1 down | rate | `delta_total=256`, `dropped=0`, `coal_status=0x00000100` |
| Type1 down | delay | `delta_total=256`, `dropped=0`, `coal_status=0x00000100` |

Simulator errors:

```text
hist_pipeline_sva: measure_clear_pulse did not lead to flushing
Errors: 4, Warnings: 1
```

The make target now fails on the nonzero simulator error count:

```text
ERROR: simulator transcript contains nonzero errors; failing run
make: *** [Makefile:173: run] Error 1
```

## Bug Links

- `BUG-009-H`: fixed harness issue where `make run` returned success despite
  nonzero Questa SVA errors.
- `BUG-010-H`: open V3 direct-input measure-clear/flushing SVA failure.
