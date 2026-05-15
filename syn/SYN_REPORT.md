# Synthesis Report: histogram bridge + histogram delay sideband

Date: 2026-05-15

## Result

PASS. The standalone bridge-plus-histogram harness meets the 1.1x signoff
target after locking the fixed-format key slice in `histogram_statistics_v2`
and registering the queue-overflow diagnostic event before its saturating
counter.

Command:

```sh
cd histogram_statistics/syn/quartus
quartus_sh --flow compile hs0_board_instance -c hs0_board_instance
```

Latest evidence:

```text
histogram_statistics/syn/quartus/output_files/hs0_board_instance.flow.rpt
histogram_statistics/syn/quartus/output_files/hs0_board_instance.sta.summary
histogram_statistics/syn/quartus/output_files/hs0_board_instance.fit.summary
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

The harness instantiates `histogram_post_ts_sideband`,
`histogram_ingress_bridge`, and `histogram_statistics_v2` with
`AVST_DATA_WIDTH=87`, `UPDATE_KEY_BIT_HI=86`, `UPDATE_KEY_BIT_LO=39`,
`SAR_TICK_WIDTH=32`, and `LOCK_KEY_RANGES=true`.

## Timing Summary

From `syn/quartus/output_files/hs0_board_instance.sta.summary`:

| Corner | Setup Slack | Setup TNS | Hold Slack | MPW Slack |
|---|---:|---:|---:|---:|
| Slow 1100mV 85C | `+0.401 ns` | `0.000 ns` | `+0.264 ns` | `+2.685 ns` |
| Slow 1100mV 0C | `+0.595 ns` | `0.000 ns` | `+0.247 ns` | `+2.665 ns` |
| Fast 1100mV 85C | `+2.844 ns` | `0.000 ns` | `+0.160 ns` | `+2.841 ns` |
| Fast 1100mV 0C | `+3.328 ns` | `0.000 ns` | `+0.150 ns` | `+2.836 ns` |

Worst setup slack is `+0.401 ns` at Slow 1100mV 85C.

## Resource Summary

From `syn/quartus/output_files/hs0_board_instance.fit.summary`:

| Resource | Usage |
|---|---:|
| Logic utilization | `7,639 / 91,680 ALMs (8%)` |
| Registers | `4,717` |
| Virtual pins | `665` |
| Block memory bits | `16,384 / 13,987,840 (<1%)` |
| RAM blocks | `2 / 1,366 (<1%)` |
| DSP blocks | `0 / 800 (0%)` |

## Validation

```sh
make -C histogram_statistics/tb run_all
make -C histogram_statistics/tb run_post_ts_sideband
make -C histogram_statistics/tb run_pre_ts_trim
make -C histogram_statistics/tb run_ingress_bridge_switch_contract
```

Result:

- `run_all`: `47 PASS, 0 FAIL`
- `tb_histogram_post_ts_sideband`: PASS
- `tb_histogram_pre_ts_trim`: PASS
- `tb_histogram_ingress_bridge_switch`: PASS

## Closure Note

The initial bridge-plus-hist compile failed timing through the runtime
CSR-programmable key-range extractor. `LOCK_KEY_RANGES=true` makes fixed-format
integrations use the generic `UPDATE_KEY_BIT_*` and `FILTER_KEY_BIT_*` ranges
directly while CSR mode/filter/value controls remain runtime-controlled. This
keeps delay mode configurable through CSR and removes the programmable
bit-slice mux from the hot ingress path.
