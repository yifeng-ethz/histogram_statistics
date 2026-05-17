# BUG_HISTORY.md - histogram_statistics_v2 DV bug ledger

This ledger records Phase-5 debug issues for `histogram_statistics_v2`.

Class legend:
- `R` = RTL / DUT bug
- `H` = harness / testcase / reporting bug

Severity legend:
- `soft error` = the bad data is observable but the datapath does not remain stuck
- `hard stuck error` = the bug can poison later datapath behavior until reset or restart
- `non-datapath-refactor` = packaging, observability, reporting, harness, or metadata work with no direct packet-contract effect

Encounterability legend:
- practical severity is `severity x encounterability`, so the index must say how likely a reader is to hit the bug in normal use rather than only when it first appeared in one simulation log
- nominal datapath operation = legal traffic, about `50%` link load, iid per-lane behavior, and no forced error injection or artificially pathological stalls
- nominal control-path operation = routine bring-up / CSR program / readback / clear-counter sequences
- `common (...)` = readily hit in nominal operation
- `occasional (...)` = hit in nominal operation without heroic setup, but not in every short run
- `rare (...)` = legal in nominal operation, but usually needs long runtime or unlucky alignment
- `corner-only (...)` = requires a legal but non-nominal stress or corner profile
- `directed-only (...)` = requires targeted error injection, formal/probe flow, reporting-only flow, or another non-operational stimulus

Fix status detail contract for active entries and future updates:
- `state` = fixed / open / partial plus the current verification gate
- `mechanism` = how the implemented repair changes the RTL, package metadata, or harness behavior
- `before_fix_outcome` and `after_fix_outcome` = concise evidence showing what changed
- `potential_hazard` = whether the fix looks permanent or is still provisional / profile-limited
- `Claude Opus 4.7 xhigh review decision` = explicit review state; use `pending / not run` until that review has actually happened

Historical formal note:
- This IP currently tracks Quartus and directed simulation evidence in this ledger.
- Future formal evidence should name the tool, version, and archived report path.

## Index

| bug_id | class | severity | encounterability | status | first seen | commit | summary |
|---|---|---|---|---|---|---|---|
| [BUG-001-R](#bug-001-r-phase-5-queue-depth-timing-miss-after-fifo-expansion) | R | soft error | `corner-only (standalone timing signoff)` | fixed | Phase-5 standalone Quartus signoff | `pending` | FIFO expansion exposed a histogram critical path until the FIFO head and filter fields were registered. |
| [BUG-002-R](#bug-002-r-standalone-sta-regression-at-version-26160429) | R | soft error | `corner-only (standalone timing signoff)` | fixed | standalone Quartus signoff from HEAD `c035c35` | `5c94fad` | VERSION 26.1.6.0429 had a standalone slow-85 setup miss; V3 direct profile now closes timing at 7.273 ns. |
| [BUG-003-R](#bug-003-r-qsys-generated-boolean-vs-natural-shell-split-on-enable_pingpong--snoop_en--enable_packet) | R | non-datapath-refactor | `common (FEB Qsys generation)` | fixed | FEB v3_pretest-260511 Quartus full compile attempt 2026-05-11 | `5c94fad` | Platform Designer package metadata briefly generated a NATURAL shell around BOOLEAN RTL generics. |
| [BUG-004-R](#bug-004-r-ctrl-sink-still-declared-asi-ctrl-ready-against-the-rc-network-readyless-contract) | R | non-datapath-refactor | `directed-only (Qsys auto-inserts timing_adapter on rc fan-out)` | fixed | FEB v3 integration audit `tb_int_run_emulator_directed` | this commit | The `ctrl` sink still declared `asi_ctrl_ready` so Qsys auto-inserted `altera_avalon_st_timing_adapter` on the rc fan-out, carrying the B002 ready-default hazard on silicon. |
| [BUG-005-R](#bug-005-r-histogram-ingress-bridge-source-switch-stalls-on-pre-rbcam-run-level-packet-active) | R | hard stuck error | `common (routine source selection after FEB pre run starts)` | open | FEB/SWB board hist status `0x105`, IP switch TB 2026-05-14 | `pending` | `histogram_ingress_bridge` treats pre-rbCAM run-level packet activity as a source-switch hazard, leaving pre/post source requests pending through RUNNING. |
| [BUG-006-R](#bug-006-r-feb-v3-direct-hist-topology-left-hist-inputs-undriven-in-tb_int) | R | hard stuck error | `common (FEB V3 direct source operation)` | fixed | FEB V3 direct-hist `tb_int` zero-hit debug, 2026-05-17 | `5c94fad` | Active Qsys topology did not yet feed the new Type0/Type1 direct hist inputs, so hist bins stayed at zero until direct taps and Type1 TS sidebands were wired. |
| [BUG-007-R](#bug-007-r-v3-direct-profile-standalone-resource-exceeds-2k-alm-target) | R | non-datapath-refactor | `corner-only (standalone resource signoff)` | open | standalone Quartus signoff 2026-05-17 | `pending` | V3 direct-input profile closes timing but still fits at 2,745 ALMs, above the requested <2k ALM target. |
| [BUG-008-H](#bug-008-h-three-instance-signaltap-debug-file-trips-quartus-181-sld-hub-generation) | H | non-datapath-refactor | `directed-only (board debug STP compile)` | partial | FEB V3 direct-hist debug compile 2026-05-17 | `pending` | A three-clock SignalTap file triggers Quartus 18.1 SLD hub generation failure; single-domain STP generation is available but multi-instance STP remains open. |
| [BUG-009-H](#bug-009-h-uvm-make-run-returned-success-despite-questa-sva-errors) | H | non-datapath-refactor | `common (SVA-enabled standalone DV run)` | fixed | focused V3 direct-input rerun 2026-05-17 | `5c94fad` | `make run` returned success even when the Questa transcript reported nonzero SVA errors. |
| [BUG-010-H](#bug-010-h-v3-direct-input-test-trips-measure_clear_pulse-flushing-sva) | H | soft error | `common (V3 source-switch clear between cases)` | open | focused V3 direct-input rerun 2026-05-17 | `pending` | `hist_v3_direct_input_test` triggers four `measure_clear_pulse did not lead to flushing` SVA errors; standalone DV is not closed until this is explained. |

## 2026-05-17

### BUG-010-H: V3 direct input test trips measure_clear_pulse flushing SVA

- First seen:
  - Focused standalone V3 direct-input rerun:
    `make -C histogram_statistics/tb/uvm run TEST=hist_v3_direct_input_test SEED=7`.
- Symptom:
  - The payload scoreboards report nonzero Type0/Type1 hit counts and zero
    UVM errors/fatals, but the simulator reports four assertion errors:
    `hist_pipeline_sva: measure_clear_pulse did not lead to flushing`.
  - The failing assertion is
    `histogram_statistics/tb/uvm/sva/hist_pipeline_sva.sv:61`.
- Root cause:
  - Open. The evidence points to a contract mismatch around measure-clear:
    `hist_pipeline_sva` requires `probe_if.flushing` on the cycle after
    `measure_clear_pulse`, while `pingpong_sram` handles `i_clear` by clearing
    internal state/valid maps and forcing `clear_active <= '0'`.
  - This entry is classed as harness until the intended clear contract is
    reviewed; promote to RTL if the design really must assert `flushing` after
    every measure clear.
- Fix status:
  - state:
    - open
  - mechanism:
    - No SVA or RTL semantic fix is claimed in this entry.
  - before_fix_outcome:
    - `histogram_statistics/tb/REPORT/hist_v3_direct_input_20260517_seed7.log`
      contains four SVA errors but the old make target returned success.
  - after_fix_outcome:
    - After BUG-009-H, the same run fails the make target and preserves the
      failing transcript at
      `histogram_statistics/tb/REPORT/hist_v3_direct_input_20260517_seed7_post_makefix.log`.
  - potential_hazard:
    - Standalone V3 DV can be falsely called closed if only the UVM report
      summary is checked and simulator assertion errors are ignored.
  - Claude Opus 4.7 xhigh review decision:
    - pending / not run in this turn
- Commit:
  - pending

### BUG-009-H: UVM make run returned success despite Questa SVA errors

- First seen:
  - Focused standalone V3 direct-input rerun on 2026-05-17.
- Symptom:
  - Questa printed `Errors: 4` from SVA `$error` calls, but the make target
    exited successfully because `vsim` returned zero after `quit -f`.
- Root cause:
  - The `run`, `run_cov`, and `run_vcd` targets trusted the `vsim` process
    return code and did not scan the transcript for simulator error counts or
    `# ** Error:` records.
- Fix status:
  - state:
    - fixed
  - mechanism:
    - Added `RUN_LOG` transcript capture and `CHECK_RUN_LOG` post-processing in
      `tb/uvm/Makefile`.
    - `run`, `run_cov`, and `run_vcd` now fail when the transcript contains
      nonzero simulator errors, even if `vsim` exits zero.
  - before_fix_outcome:
    - `histogram_statistics/tb/REPORT/hist_v3_direct_input_20260517_seed7.log`
      shows four SVA errors with no make failure.
  - after_fix_outcome:
    - Re-running `hist_v3_direct_input_test` now exits with make error and the
      explicit message `ERROR: simulator transcript contains nonzero errors;
      failing run`.
  - potential_hazard:
    - Low after the makefile fix; future report generators still need to use
      the make target exit code and not just UVM error counts.
  - Claude Opus 4.7 xhigh review decision:
    - pending / not run in this turn
- Commit:
  - fixed by `5c94fad`

### BUG-008-H: three-instance SignalTap debug file trips Quartus 18.1 SLD hub generation

- First seen:
  - FEB V3 direct-hist debug compile after importing a three-instance STP
    containing `direct_hist_v3_lvds`, `direct_hist_v3_mclk125`, and
    `direct_hist_v3_cclk156`.
- Symptom:
  - Analysis reaches debug fabric generation, then Quartus 18.1 aborts with
    `Error (265075): The megafunction, pzdyqx, required for Intel FPGA IP
    Evaluation Mode does not match the one supplied in this release of Quartus
    Prime software`.
  - The same failure reports `Error (12154): Can't elaborate inferred hierarchy
    "sld_hub:auto_hub"`.
- Root cause:
  - Not identified as DUT RTL. The failure is tied to the debug-fabric shape
    emitted by the generated multi-instance STP under Quartus 18.1.
- Fix status:
  - state:
    - partial
  - mechanism:
    - `generate_direct_hist_v3_stp.py` can now emit selected single-clock
      domains with `--domains`.
    - The `cclk156` single-domain STP validates 19/19 probes and imports with
      zero `quartus_stp` errors, exposing the SC upload boundary without a
      cross-clock SignalTap acquisition path.
    - The three-instance STP shape remains unsupported in this flow until a
      Quartus-compatible multi-clock debug-fabric recipe is found.
  - before_fix_outcome:
    - Clean compile of the three-instance STP reproduced the `sld_hub:auto_hub`
      elaboration failure.
  - after_fix_outcome:
    - `direct_hist_v3_cclk156.stp` Node Finder check: 19 probes found, 0
      missing, 0 errors.
    - `quartus_stp` import of the single-domain STP: 0 errors, 0 warnings.
    - Full FEB fit was interrupted before final STA/SOF evidence was produced;
      do not use the stale `status=running` file as closure evidence.
  - potential_hazard:
    - Board debug can be blocked if multi-clock STP is regenerated as one
      three-instance file. Use one same-clock debug image per compile until the
      multi-instance Quartus issue is closed.
  - Claude Opus 4.7 xhigh review decision:
    - pending / not run in this turn
- Commit:
  - pending

### BUG-007-R: V3 direct profile standalone resource exceeds 2k ALM target

- First seen:
  - Standalone Quartus signoff for `histogram_statistics_v2_standalone` on
    2026-05-17.
- Symptom:
  - Timing closes at the tightened 7.273 ns period, but fitter resource usage
    is `2,745` ALMs, above the requested `<2k ALM` target.
- Root cause:
  - The largest fitted hierarchy is still the top-level CSR/source-select/
    readback cone plus `pingpong_sram` valid/pending/read-arbitration logic.
    The V3 reductions are present, including 20-bit counters, 4-entry ingress
    FIFOs, 4 live coalescer cells, 4-bit kick counters, and power-of-two bin
    mapping, but they are not sufficient to reach `<2k ALM`.
- Fix status:
  - state:
    - open
  - mechanism:
    - No resource-reduction RTL fix is claimed in this entry.
  - before_fix_outcome:
    - `syn/quartus/output_files/histogram_statistics_v2_standalone.fit.summary`
      reports logic utilization `2,745 / 91,680 ALMs`.
  - after_fix_outcome:
    - open; no after-fix compile exists yet below `<2k ALM`.
  - potential_hazard:
    - This is not a functional datapath bug, but it blocks the requested V3
      standalone synthesis/resource closure.
  - Claude Opus 4.7 xhigh review decision:
    - pending / not run in this turn
- Commit:
  - pending

### BUG-006-R: FEB V3 direct hist topology left hist inputs undriven in tb_int

- First seen:
  - FEB V3 direct-hist `tb_int` zero-hit debug on 2026-05-17.
- Symptom:
  - `tb_int` stimulus delivered legal Type0/Type1 hits, but histogram readback
    stayed at zero because the active system topology was not yet feeding the
    new direct Type0/Type1 histogram interfaces.
- Root cause:
  - The active FEB V3 Qsys topology still needed explicit wiring for the new
    direct hist contract:
    - eight Type0 lane taps into `histogram_statistics_0.type0_lane0..7`,
    - Type1 up/down split taps into `histogram_statistics_0.type1_up/down`,
    - direct 48-bit Type1 timestamp sidebands into
      `histogram_statistics_0.type1_up/down_ts`.
  - The old `histogram_ingress_bridge` path is not valid evidence for FEB V3
    direct Type0/Type1 operation.
- Fix status:
  - state:
    - fixed
  - mechanism:
    - Added the reusable `hit_type0_tap2` Platform Designer IP.
    - Updated the FEB V3 histogram topology Tcl so Type0/Type1 streams feed
      hist directly while the primary hit path is preserved through the tap
      outputs.
    - Removed live `histogram_ingress_bridge_0/1` instances from the active
      generated direct-hist `.qsys` systems.
  - before_fix_outcome:
    - Direct hist `tb_int` showed zero histogram hits under legal stimulus.
  - after_fix_outcome:
    - `hist_direct_v3_matrix_20260517_090444_summary.csv`: 30/30 PASS, zero
      simulator errors, zero drops, `bin_sum == total` for every case.
    - The matrix covers Type0 rate and Type1 up/down rate+latency modes at
      100 kHz, 500 kHz, and 1 MHz per ASIC, with both one-random-channel and
      all-channel/all-ASIC patterns.
  - potential_hazard:
    - The fix is topology/source-of-truth sensitive. If future Qsys regeneration
      bypasses `update_dualport_histogram_topology.tcl`, the zero-hit symptom
      can return.
  - Claude Opus 4.7 xhigh review decision:
    - pending / not run in this turn
- Commit:
  - fixed by `5c94fad`

## 2026-05-14

### BUG-005-R: histogram ingress bridge source switch stalls on pre-rbCAM run-level packet active

- First seen:
  - SWB board guarded run reported histogram bridge source status `0x00000105`: live post, requested pre, switch pending, `pre_packet_active=1`.
  - FEB IP-level switch regression added under `tb/histogram_ingress_bridge` reproduces the same mechanism with post-hit filtering enabled:
    - post-to-pre request during a FEB pre run reports `0x00000505`.
    - pre-to-post request during a FEB pre run reports `0x00000506`.
- Symptom:
  - Once the FEB pre-rbCAM stream emits its run-opening SOP, source-selection writes can remain pending until the terminating pre EOP marker.
  - In RUNNING, the histogram input can therefore stay connected to the stale source even when both stream inputs are beat-idle.
- Root cause:
  - `histogram_ingress_bridge.switch_safe` requires `pre_packet_active='0'`.
  - FEB MTS/rbCAM semantics use pre stream SOP/EOP as run-level markers, not per-hit packet markers, so `pre_packet_active` remains high for the whole run after the first pre hit.
- Fix status:
  - state:
    - open
  - mechanism:
    - No RTL fix applied in this change.
    - New IP-local TB target `run_ingress_bridge_switch_observed` records the current stuck/pending behavior.
    - New IP-local TB target `run_ingress_bridge_switch_contract` encodes the intended contract and fails on current RTL.
  - before_fix_outcome:
    - `run_ingress_bridge_switch_observed` passes with observed-bug statuses `0x00000505` and `0x00000506`.
    - `run_ingress_bridge_switch_contract` fails with two contract errors because source requests remain pending while streams are beat-idle.
  - potential_hazard:
    - This is operationally common if source selection is performed after a FEB pre run has started, and it can make RUNNING histogram evidence come from the wrong rbCAM boundary.
  - Claude Opus 4.7 xhigh review decision:
    - pending / not run in this turn
- Commit:
  - pending

## 2026-05-11

### BUG-004-R: ctrl sink still declared asi_ctrl_ready against the rc-network readyless contract

- First seen:
  - FEB v3 integration audit during the rc-readyless rollout
  - Hub `runctl_mgmt_host._hw.tcl` advertises `USE_READY=0` for the broadcast `runctl` source; every sink that still declared `asi_*_ready` caused Qsys to silently auto-insert `altera_avalon_st_timing_adapter` on the rc fan-out
  - The timing_adapter is the structural carrier of the B002 ready-default hazard already documented for the FEB SC plane
- Symptom:
  - `histogram_statistics_v2.ctrl` still exposed `asi_ctrl_ready` on the entity boundary even though the hub source has no ready signal
  - Qsys-generated `feb_system_v3.vhd` wired the `histogram_statistics_0` instance through an adapter even though the local body only drove `asi_ctrl_ready <= '1';` as a constant
- Root cause:
  - The Avalon-ST sink interface contract is "readyless" only when both ends declare `USE_READY=0`. `histogram_statistics_v2` was still on the legacy backpressured-rc form, and the constant `'1'` driver was the only consumer.
- Fix status:
  - state:
    - fixed
  - mechanism:
    - Removed the `asi_ctrl_ready` entity port from `rtl/histogram_statistics_v2.vhd` and the matching `add_interface_port` line from `histogram_statistics_v2_hw.tcl`
    - Removed the constant assignment `asi_ctrl_ready <= '1';` from the architecture body
    - Bumped `VERSION` 26.1.6.0429 -> 26.2.0.0511
  - after_fix_outcome:
    - FEB v3 Qsys regeneration produced `feb_system_v3.vhd` with the `histogram_statistics_0` instance wired with `asi_ctrl_valid => avalon_st_adapter_018_out_0_valid` and no paired ready wire
    - `tb_int` regression passed: `B065`, `B066`, `B067`, `B068`, `B069`, and the directed `RC_EMUL` run all reported `*** TEST PASSED ***` with zero UVM errors and zero UVM fatals
  - potential_hazard:
    - The change is interface-contract only; no internal logic was modified.
  - Claude Opus 4.7 xhigh review decision:
    - pending / not run in this turn
- Commit:
  - this commit (`[FIX] HW: Drop ctrl ready output (rc-network readyless contract)`)

### BUG-003-R: Qsys-generated BOOLEAN vs NATURAL shell split on ENABLE_PINGPONG / SNOOP_EN / ENABLE_PACKET

- Status: fixed by follow-up packaging rollback after the first NATURAL attempt.
- First seen in: FEB v3_pretest-260511 Quartus full compile attempt 2026-05-11 (file `firmware_builds/systems/v3_pretest-260511/syn/board_projects/fe_scifi_feb_v3/quartus_compile_top_20260511_120148.console.log`).
- Symptom: VHDL error 10476 at `histogram_statistics_v2.vhd` lines 22, 35, 36 - "type of identifier `true` does not agree with its usage as `natural` type", followed by error 12152 inability to elaborate the IP inside `feb_system_v3_data_path_subsystem`. Quartus Analysis & Synthesis aborts with 4 errors, 257 warnings.
- Root cause: the source RTL entity `rtl/histogram_statistics_v2.vhd` declares `ENABLE_PINGPONG`, `SNOOP_EN`, and `ENABLE_PACKET` as BOOLEAN generics. The attempted fix in `c17ce73` changed only `histogram_statistics_v2_hw.tcl` to NATURAL, causing Platform Designer to generate a NATURAL top shell for the IP while the enclosing FEB data-path wrapper still held BOOLEAN component metadata and BOOLEAN actuals. That split made A&S see BOOLEAN literals at a NATURAL shell boundary.
- Fix status:
  - state:
    - fixed
  - mechanism:
    - Keep the IP type contract BOOLEAN on both sides.
    - `histogram_statistics_v2_hw.tcl` declares
      `add_parameter ENABLE_PINGPONG BOOLEAN true`,
      `add_parameter SNOOP_EN BOOLEAN true`, and
      `add_parameter ENABLE_PACKET BOOLEAN true`; the V3 Qsys recipes set the
      FEB instance values to `true`, `false`, and `false`.
  - after_fix_outcome:
    - Post-fix packaging and Qsys regeneration show the generated
      `histogram_statistics_v2.vhd` entity and the generated
      `feb_system_v3_data_path_subsystem.vhd` component declaration both use
      BOOLEAN for these three generics.
  - potential_hazard:
    - Future metadata-only edits can reintroduce a shell/entity type split if
      the `_hw.tcl` parameter type and RTL generic type are not kept identical.
  - Claude Opus 4.7 xhigh review decision:
    - pending / not run in this turn
- Commit:
  - fixed by `5c94fad`

### BUG-002-R: Standalone STA regression at VERSION 26.1.6.0429

- Status: fixed by the V3 direct-input timing/resource-profile update.
- First seen in: fresh standalone Quartus signoff from HEAD `c035c35a96478387e21d692c2a17554bcff20806` at the 1.1x F_target corner.
- Symptom: Slow 1100mV 85C setup slack is `-0.234 ns` on `i_clk` at the `7.273 ns` standalone signoff period.
- Root cause:
  - The pre-V3 queue accounting path let `queue_hit_bin` drive
    `coalescing_queue.overflow_count_q` through too much same-cycle compare and
    queue/update logic for the tightened standalone 1.1x signoff period.
- Note: Failed standalone signoff at corner Slow 1100mV 85C after VERSION `26.1.6.0429` - integration must apply SDC false_path only if the arc is architecturally false, or the IP must register-split the path.
- Failing path: from `queue_hit_bin[3]` to `coalescing_queue:queue_inst|overflow_count_q[0]`; data delay `7.429 ns`; clock skew `-0.078 ns`; logic depth `7`.
- Corner summary: Slow 1100mV 85C setup `-0.234 ns`; Slow 1100mV 0C setup `0.036 ns`; Fast 1100mV 85C setup `2.459 ns`; Fast 1100mV 0C setup `2.950 ns`.
- Evidence: `syn/quartus/compile_histogram_statistics_v2_standalone_20260511.log`; `syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary`; `syn/quartus/output_files/histogram_statistics_v2_standalone.worst_setup_paths.rpt`.
- Fix status:
  - state:
    - fixed for standalone timing
  - mechanism:
    - The V3 direct-input profile reduces and registers the hot queue/update
      path using 4-entry ingress FIFOs, 4 live coalescer cells, 4-bit kick
      counters, power-of-two bin mapping, and the direct Type0/Type1 source
      selection profile.
  - before_fix_outcome:
    - Slow 1100mV 85C setup slack was `-0.234 ns` at the `7.273 ns`
      standalone signoff period.
  - after_fix_outcome:
    - `syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary`
      reports setup and hold closed at all available corners.
    - Worst Slow 1100mV 85C setup slack is `+0.809 ns`; hold slack is
      `+0.271 ns`; TNS is `0.000 ns`.
  - potential_hazard:
    - Timing is closed, but BUG-007-R remains open because the same fitted V3
      profile is still above the requested `<2k ALM` resource target.
  - Claude Opus 4.7 xhigh review decision:
    - pending / not run in this turn
- Commit:
  - fixed by `5c94fad`

## 2026-04-27

### BUG-001-R: Phase-5 queue-depth timing miss after FIFO expansion

- First seen in: Phase-5 standalone Quartus signoff after raising the ingress FIFO default to 256 entries.
- Symptom: slow-85 standalone setup slack missed timing on the histogram critical path.
- Root cause: the expanded FIFO/filter hot path and coalescing peak-level accounting left too much logic between registers.
- Fix status: fixed; registered the FIFO head in `hit_fifo`, replicated filter fields per ingress port, and updated `coalescing_queue` peak occupancy from the registered queue level.
- Evidence: standalone Quartus closes at slow-85 setup slack +0.129 ns; standalone DV remains `27 PASS, 0 FAIL`.
