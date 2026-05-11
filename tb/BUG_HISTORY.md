# BUG_HISTORY.md - histogram_statistics_v2 bug ledger

This ledger records Phase-5 debug issues for `histogram_statistics_v2`.

## 2026-05-11

### `BUG-003-R` _hw.tcl BOOLEAN vs VHDL NATURAL type mismatch on ENABLE_PINGPONG / SNOOP_EN / ENABLE_PACKET

- Status: fixed (this commit).
- First seen in: FEB v3_pretest-260511 Quartus full compile attempt 2026-05-11 (file `firmware_builds/systems/v3_pretest-260511/syn/board_projects/fe_scifi_feb_v3/quartus_compile_top_20260511_120148.console.log`).
- Symptom: VHDL error 10476 at `histogram_statistics_v2.vhd` lines 22, 35, 36 - "type of identifier `true` does not agree with its usage as `natural` type", followed by error 12152 inability to elaborate the IP inside `feb_system_v3_data_path_subsystem`. Quartus Analysis & Synthesis aborts with 4 errors, 257 warnings.
- Root cause: `histogram_statistics_v2_hw.tcl` declared `ENABLE_PINGPONG`, `SNOOP_EN`, `ENABLE_PACKET` as `BOOLEAN true`, but `histogram_statistics_v2.vhd` entity declares the matching generics as `natural := 1`. The qsys-generated wrapper passed the boolean literal through to the natural-typed generic, which Quartus refused to elaborate. Standalone IP build did not see this path because the standalone harness instantiates the entity directly with natural literals rather than via the qsys-generated wrapper.
- Fix: `histogram_statistics_v2_hw.tcl` lines 591, 606, 612 now declare `add_parameter ENABLE_PINGPONG NATURAL 1`, `add_parameter SNOOP_EN NATURAL 1`, `add_parameter ENABLE_PACKET NATURAL 1`. Defaults preserved (was `true`, now `1`; FALSE branch was already covered as `0`).
- Evidence: post-fix the 3 lines read `add_parameter <name> NATURAL 1` per `grep -nE "add_parameter (ENABLE_PINGPONG|SNOOP_EN|ENABLE_PACKET)" histogram_statistics_v2_hw.tcl`. Quartus recompile from the v3_pretest-260511 board project is launched after this commit; expected outcome is A&S succeeds past the prior abort.

### `BUG-002-R` Standalone STA regression at VERSION 26.1.6.0429

- Status: REGRESSION.
- First seen in: fresh standalone Quartus signoff from HEAD `c035c35a96478387e21d692c2a17554bcff20806` at the 1.1x F_target corner.
- Symptom: Slow 1100mV 85C setup slack is `-0.234 ns` on `i_clk` at the `7.273 ns` standalone signoff period.
- Note: Failed standalone signoff at corner Slow 1100mV 85C after VERSION `26.1.6.0429` - integration must apply SDC false_path only if the arc is architecturally false, or the IP must register-split the path.
- Failing path: from `queue_hit_bin[3]` to `coalescing_queue:queue_inst|overflow_count_q[0]`; data delay `7.429 ns`; clock skew `-0.078 ns`; logic depth `7`.
- Corner summary: Slow 1100mV 85C setup `-0.234 ns`; Slow 1100mV 0C setup `0.036 ns`; Fast 1100mV 85C setup `2.459 ns`; Fast 1100mV 0C setup `2.950 ns`.
- Evidence: `syn/quartus/compile_histogram_statistics_v2_standalone_20260511.log`; `syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary`; `syn/quartus/output_files/histogram_statistics_v2_standalone.worst_setup_paths.rpt`.
- Fix status: open. Newest version fails standalone signoff; no RTL fix, false-path exception, VERSION downgrade, or older-build fallback was applied in this record.

## 2026-04-27

### `BUG-001-R` Phase-5 queue-depth timing miss after FIFO expansion

- First seen in: Phase-5 standalone Quartus signoff after raising the ingress FIFO default to 256 entries.
- Symptom: slow-85 standalone setup slack missed timing on the histogram critical path.
- Root cause: the expanded FIFO/filter hot path and coalescing peak-level accounting left too much logic between registers.
- Fix status: fixed; registered the FIFO head in `hit_fifo`, replicated filter fields per ingress port, and updated `coalescing_queue` peak occupancy from the registered queue level.
- Evidence: standalone Quartus closes at slow-85 setup slack +0.129 ns; standalone DV remains `27 PASS, 0 FAIL`.
