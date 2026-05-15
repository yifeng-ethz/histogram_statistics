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
| [BUG-002-R](#bug-002-r-standalone-sta-regression-at-version-26160429) | R | soft error | `corner-only (standalone timing signoff)` | open | standalone Quartus signoff from HEAD `c035c35` | `pending` | VERSION 26.1.6.0429 has a standalone slow-85 setup miss on a queue accounting path. |
| [BUG-003-R](#bug-003-r-qsys-generated-boolean-vs-natural-shell-split-on-enable_pingpong--snoop_en--enable_packet) | R | non-datapath-refactor | `common (FEB Qsys generation)` | fixed | FEB v3_pretest-260511 Quartus full compile attempt 2026-05-11 | `pending` | Platform Designer package metadata briefly generated a NATURAL shell around BOOLEAN RTL generics. |
| [BUG-004-R](#bug-004-r-ctrl-sink-still-declared-asi-ctrl-ready-against-the-rc-network-readyless-contract) | R | non-datapath-refactor | `directed-only (Qsys auto-inserts timing_adapter on rc fan-out)` | fixed | FEB v3 integration audit `tb_int_run_emulator_directed` | this commit | The `ctrl` sink still declared `asi_ctrl_ready` so Qsys auto-inserted `altera_avalon_st_timing_adapter` on the rc fan-out, carrying the B002 ready-default hazard on silicon. |
| [BUG-005-R](#bug-005-r-histogram-ingress-bridge-source-switch-stalls-on-pre-rbcam-run-level-packet-active) | R | hard stuck error | `common (routine source selection after FEB pre run starts)` | open | FEB/SWB board hist status `0x105`, IP switch TB 2026-05-14 | `pending` | `histogram_ingress_bridge` treats pre-rbCAM run-level packet activity as a source-switch hazard, leaving pre/post source requests pending through RUNNING. |

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
- Fix: keep the IP type contract BOOLEAN on both sides. `histogram_statistics_v2_hw.tcl` lines 591, 606, 612 again declare `add_parameter ENABLE_PINGPONG BOOLEAN true`, `add_parameter SNOOP_EN BOOLEAN true`, and `add_parameter ENABLE_PACKET BOOLEAN true`; the v3 Qsys recipes set the FEB instance values to `true`, `false`, and `false`.
- Evidence: post-fix packaging and Qsys regeneration must show the generated `histogram_statistics_v2.vhd` entity and the generated `feb_system_v3_data_path_subsystem.vhd` component declaration both use BOOLEAN for these three generics.

### BUG-002-R: Standalone STA regression at VERSION 26.1.6.0429

- Status: REGRESSION.
- First seen in: fresh standalone Quartus signoff from HEAD `c035c35a96478387e21d692c2a17554bcff20806` at the 1.1x F_target corner.
- Symptom: Slow 1100mV 85C setup slack is `-0.234 ns` on `i_clk` at the `7.273 ns` standalone signoff period.
- Note: Failed standalone signoff at corner Slow 1100mV 85C after VERSION `26.1.6.0429` - integration must apply SDC false_path only if the arc is architecturally false, or the IP must register-split the path.
- Failing path: from `queue_hit_bin[3]` to `coalescing_queue:queue_inst|overflow_count_q[0]`; data delay `7.429 ns`; clock skew `-0.078 ns`; logic depth `7`.
- Corner summary: Slow 1100mV 85C setup `-0.234 ns`; Slow 1100mV 0C setup `0.036 ns`; Fast 1100mV 85C setup `2.459 ns`; Fast 1100mV 0C setup `2.950 ns`.
- Evidence: `syn/quartus/compile_histogram_statistics_v2_standalone_20260511.log`; `syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary`; `syn/quartus/output_files/histogram_statistics_v2_standalone.worst_setup_paths.rpt`.
- Fix status: open. Newest version fails standalone signoff; no RTL fix, false-path exception, VERSION downgrade, or older-build fallback was applied in this record.

## 2026-04-27

### BUG-001-R: Phase-5 queue-depth timing miss after FIFO expansion

- First seen in: Phase-5 standalone Quartus signoff after raising the ingress FIFO default to 256 entries.
- Symptom: slow-85 standalone setup slack missed timing on the histogram critical path.
- Root cause: the expanded FIFO/filter hot path and coalescing peak-level accounting left too much logic between registers.
- Fix status: fixed; registered the FIFO head in `hit_fifo`, replicated filter fields per ingress port, and updated `coalescing_queue` peak occupancy from the registered queue level.
- Evidence: standalone Quartus closes at slow-85 setup slack +0.129 ns; standalone DV remains `27 PASS, 0 FAIL`.
