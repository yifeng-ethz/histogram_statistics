# Signoff - histogram_statistics_v2 standalone STA refresh

**DUT:** `histogram_statistics_v2`
**VERSION:** `26.1.6.0429`
**RTL HEAD before this documentation commit:** `c035c35a96478387e21d692c2a17554bcff20806`
**Refresh date:** `2026-05-11`
**Decision:** FAIL / REGRESSION

This page records the standalone Quartus signoff refresh requested for the
latest committed IP. The newest VERSION is treated as authentic; no downgrade
or fallback build was used.

## Standalone Target

| Item | Value |
|---|---|
| Project | `syn/quartus/histogram_statistics_v2_signoff.qpf` |
| Revision | `histogram_statistics_v2_standalone` |
| Top | `histogram_statistics_v2` |
| Device | Arria V `5AGXBA7D4F31C5` |
| Quartus | `18.1.0 Build 625 09/12/2018 SJ Standard Edition` |
| Nominal F_target | `125 MHz` |
| 1.1x signoff target | `137.5 MHz` |
| Constrained period | `7.273 ns` |
| SDC | `syn/quartus/histogram_statistics_v2_standalone.sdc` |

The requested corner names were Slow/Fast 900mV 100C/0C. For this Arria V
Quartus 18.1 standalone project, TimeQuest reports the four available setup
timing models as Slow/Fast 1100mV 85C/0C. The table below records those actual
report corners.

## Evidence

| Artifact | Path |
|---|---|
| Compile log | `syn/quartus/compile_histogram_statistics_v2_standalone_20260511.log` |
| Flow report | `syn/quartus/output_files/histogram_statistics_v2_standalone.flow.rpt` |
| Fit summary | `syn/quartus/output_files/histogram_statistics_v2_standalone.fit.summary` |
| STA summary | `syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary` |
| STA full report | `syn/quartus/output_files/histogram_statistics_v2_standalone.sta.rpt` |
| Worst-path report | `syn/quartus/output_files/histogram_statistics_v2_standalone.worst_setup_paths.rpt` |
| Worst-path STA log | `syn/quartus/sta_worst_setup_paths_20260511.log` |
| Static screen log | `tb/static_screen_20260511_v2cone/questa_static_screen.log` |

The full compile finished successfully with `0 errors, 10022 warnings`.
`histogram_statistics_v2_standalone.flow.rpt` records `Flow Status :
Successful - Mon May 11 10:59:18 2026`; the compile log ended at
`Mon May 11 10:59:44 2026`. The detailed worst-path STA report was regenerated
at `Mon May 11 11:04:51 2026`.

## Timing Result

| Corner | Setup slack | Setup TNS | Hold slack | MPW slack | Fmax | Worst setup path |
|---|---:|---:|---:|---:|---:|---|
| Slow 1100mV 85C | `-0.234 ns` | `-6.359 ns` | `0.274 ns` | `2.698 ns` | `133.21 MHz` | `queue_hit_bin[3]` to `coalescing_queue:queue_inst|overflow_count_q[0]` |
| Slow 1100mV 0C | `0.036 ns` | `0.000 ns` | `0.255 ns` | `2.667 ns` | `138.18 MHz` | `cfg_mode[3]` to `ingress_stage_write_req[1]` |
| Fast 1100mV 85C | `2.459 ns` | `0.000 ns` | `0.162 ns` | `2.849 ns` | n/a | `rr_arbiter:arb_inst|select_idx_q[2]` to `hit_fifo:\fifo_gen:4:fifo_inst|read_data_q[20]` |
| Fast 1100mV 0C | `2.950 ns` | `0.000 ns` | `0.148 ns` | `2.839 ns` | n/a | `rr_arbiter:arb_inst|select_idx_q[2]` to `hit_fifo:\fifo_gen:4:fifo_inst|read_data_q[20]` |

Worst overall clock: `i_clk`. Worst overall setup slack: `-0.234 ns` at
Slow 1100mV 85C.

## Failing Path Detail

| Corner | From | To | Data delay | Clock skew | Logic levels | Slack |
|---|---|---|---:|---:|---:|---:|
| Slow 1100mV 85C | `queue_hit_bin[3]` | `coalescing_queue:queue_inst|overflow_count_q[0]` | `7.429 ns` | `-0.078 ns` | `7` | `-0.234 ns` |
| Slow 1100mV 0C | `cfg_mode[3]` | `ingress_stage_write_req[1]` | `7.135 ns` | `-0.102 ns` | `8` | `0.036 ns` |
| Fast 1100mV 85C | `rr_arbiter:arb_inst|select_idx_q[2]` | `hit_fifo:\fifo_gen:4:fifo_inst|read_data_q[20]` | `4.763 ns` | `-0.051 ns` | `5` | `2.459 ns` |
| Fast 1100mV 0C | `rr_arbiter:arb_inst|select_idx_q[2]` | `hit_fifo:\fifo_gen:4:fifo_inst|read_data_q[20]` | `4.279 ns` | `-0.044 ns` | `5` | `2.950 ns` |

## Static Screen

The current v2 standalone cone was screened with the Questa static wrapper using
`tb/histogram_statistics_v2_static.f`.

| Check | Result | Report |
|---|---|---|
| Lint errors | `0` | `tb/static_screen_20260511_v2cone/qverify_db/lint.rpt` |
| CDC violations | `0` | `tb/static_screen_20260511_v2cone/reports/cdc.rpt` |
| RDC violations | `0` | `tb/static_screen_20260511_v2cone/qverify_db/rdc.rpt` |

Lint still reports warnings, but no lint errors. CDC and RDC report zero checks
with zero violations because the standalone cone has one inferred clock domain
and one synchronous reset domain.

## Signoff Decision

Standalone IP signoff requires setup slack `>= 0 ns` at every available
standalone signoff corner. VERSION `26.1.6.0429` fails that rule at Slow
1100mV 85C with `-0.234 ns` setup slack on `i_clk`.

Status: REGRESSION. Newest version fails standalone signoff; integration must
apply an SDC false_path only if the arc is architecturally false, or the IP must
register-split the path.
