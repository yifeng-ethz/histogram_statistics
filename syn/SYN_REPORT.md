# Synthesis Report: histogram_statistics_v2 bridge-free streaming debug plane

Date: 2026-05-15

## Result

Current standalone Quartus timing signoff was not rerun after the 26.3.0
bridge-removal change. The previous PASS below was produced by the retired
bridge-plus-histogram harness and must not be used as current bridge-free
signoff evidence.

Current bridge-free functional evidence:

```sh
make -C histogram_statistics/tb run_all
```

Result:

```text
47 PASS, 0 FAIL
```

FEB v3 integration evidence:

```text
firmware_builds/systems/v3_pretest-260511/syn/feb_system_v3_qsys_generate_20260515_stream_debug_bridgefree_retry1_isolated.status
exit_code=0
error_count=0
```

The generated FEB v3 synthesis tree wires:

- `mts_preprocessor_0.hit_type1_out` as the 39-bit upper-bank main Type-1 path;
- `mts_preprocessor_1.hit_type1_out` as the 39-bit lower-bank main Type-1 path;
- `mts_preprocessor_0.hit_type1_extended_0` directly to `histogram_statistics_0.hit_type1_extended_0`;
- `mts_preprocessor_1.hit_type1_extended_1` directly to `histogram_statistics_0.hit_type1_extended_1`.

## Archived Previous Evidence

The archived 2026-05-15 bridge-plus-histogram standalone compile used:

```sh
cd histogram_statistics/syn/quartus
quartus_sh --flow compile hs0_board_instance -c hs0_board_instance
```

Tool:

```text
Quartus Prime 18.1.0 Build 625 Standard Edition
```

Project context:

| Item | Value |
|---|---|
| Project | `syn/quartus/hs0_board_instance.qpf` |
| Revision | `hs0_board_instance` |
| Top | `hs0_board_instance_top` |
| Device | Arria V `5AGXBA7D4F31C5` |
| Nominal F_target | `125 MHz` |
| 1.1x signoff target | `137.5 MHz` |
| Period | `7.273 ns` |

Archived timing summary from the retired harness:

| Corner | Setup Slack | Setup TNS | Hold Slack | MPW Slack |
|---|---:|---:|---:|---:|
| Slow 1100mV 85C | `+0.401 ns` | `0.000 ns` | `+0.264 ns` | `+2.685 ns` |
| Slow 1100mV 0C | `+0.595 ns` | `0.000 ns` | `+0.247 ns` | `+2.665 ns` |
| Fast 1100mV 85C | `+2.844 ns` | `0.000 ns` | `+0.160 ns` | `+2.841 ns` |
| Fast 1100mV 0C | `+3.328 ns` | `0.000 ns` | `+0.150 ns` | `+2.836 ns` |

Archived resource summary from the retired harness:

| Resource | Usage |
|---|---:|
| Logic utilization | `7,639 / 91,680 ALMs (8%)` |
| Registers | `4,717` |
| Virtual pins | `665` |
| Block memory bits | `16,384 / 13,987,840 (<1%)` |
| RAM blocks | `2 / 1,366 (<1%)` |
| DSP blocks | `0 / 800 (0%)` |

## Current Closure Gap

Run a fresh standalone Quartus compile for the bridge-free
`hs0_board_instance_top` before claiming 26.3.0 standalone timing/resource
closure. The archived bridge-plus-hist numbers are useful only as a historical
baseline.
