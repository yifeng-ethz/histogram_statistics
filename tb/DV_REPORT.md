# DV Report - histogram_statistics_v2 GTS Clear Regression Fix

Date: 2026-05-19

Scope: focused regression fix for the reset-contract patch.  The RTL restores
the SYNC-entry `gts_counter_clear` pulse while preserving the RESET self-exit
contract and keeping the removed `gts_reset_reg` latch out of the design.

## Result

NOT CLOSED.  The focused RTL fix compiles and preserves the reset-state
contract, but the requested UVM soak still fails in the current tree.

The project Makefiles were not invoked directly because the `compile` targets in
`tb/Makefile` and `tb/uvm/Makefile` run `rm -rf` on simulator work libraries.
The 2026-05-19 run below used a fresh work directory and equivalent Questa
compile/run commands without deleting files.

## Evidence

| Flow | Result | Evidence |
|---|---|---|
| UVM compile | PASS | `tb/uvm/codex_uvm_run_20260519_gtsfix_gw9dbf/logs/compile.log`: VHDL/SV compile completed with 0 errors. |
| Requested UVM soak subset | FAIL | `tb/uvm/codex_uvm_run_20260519_gtsfix_gw9dbf/uvm_soak_summary.txt`: 1 passing test, 7 failing tests. |
| Questa static screen | PASS | `.questa_static_screen_gtsfix/questa_static_screen.log`: lint 0 errors, CDC 0 violations, RDC 0 violations. |

UVM result summary:

| Test | Result |
|---|---|
| `hist_bank_test` | FAIL |
| `hist_bin_test` | FAIL |
| `hist_burst_test` | FAIL |
| `hist_cfg_test` | FAIL |
| `hist_coalesce_test` | FAIL |
| `hist_debug_test` | FAIL |
| `hist_csr_test` | PASS |
| `hist_cross_test` | FAIL |

`hist_cross_test` was stopped after UVM errors were already present in the log;
it is counted as a failure in this report, not as passing evidence.

Representative failing signatures:

```text
hist_scoreboard.sv(451): divider bin mismatch dut=0 ref=1
hist_scoreboard.sv(451): divider bin mismatch dut=0 ref=2
hist_scoreboard.sv(451): divider bin mismatch dut=0 ref=3
hist_scoreboard.sv(451): divider bin mismatch dut=2 ref=42
hist_scoreboard.sv(451): divider bin mismatch dut=96 ref=50
```

## RTL Fix Coverage

The old output-value behavior of `gts_counter_rst` is restored without
reintroducing a latched reset-equivalent state:

- `runctl_sync_start` asserts for the SYNC-entry command pulse when
  `run_state_cmd /= SYNC`;
- `gts_counter_clear` now asserts on hardware reset, active run-control RESET,
  SYNC entry, or RUNNING entry;
- no `gts_reset_reg` process or `gts_counter_rst` register is present;
- the only `run_state_cmd <= RESET` assignment remains under the active
  `RUNCTL_RESET_CMD_CONST` branch;
- when the run-control RESET hold deasserts, the FSM self-exits to `IDLE`.

## Open DV Item

The requested soak failures are not closed by the GTS clear restoration.  In
particular, `hist_bank_test` programs CSRs and injects fill words but does not
drive the run-control SYNC/RUNNING path, so it does not exercise the restored
`gts_counter_clear` pulse.  The current failure pattern aligns with a separate
Type0 key-layout mismatch: UVM fill-word defaults build the update key at bits
`17:29`, while the RTL Type0 extraction constants select bits `21:35`.
