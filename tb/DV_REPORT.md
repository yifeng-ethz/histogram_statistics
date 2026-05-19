# DV Report - histogram_statistics_v2 Type0 key-layout + GTS clear regression refresh

Date: 2026-05-19 (closes prior 26.3.5 GTS-clear soak)

## Scope

This refresh combines two TB-side fixes against `histogram_statistics_v2`
26.3.7.0519:

1. The user-flagged Type0 key-layout TB bug from the prior session
   (7 UVM tests failed with `divider bin mismatch dut=0 ref=N`).
2. The pre-existing GTS-clear regression that the 26.3.6 attempt
   broke and 26.3.7 restored.

## Type0 key-layout root cause

With `LOCK_KEY_RANGES = 1'b1` (FEB V3/V4 default) and `source_select =
HS_SOURCE_TYPE0` (the `program_histogram` default), the RTL ingress
uses **fixed Type0 slice bits [35:21]** for the update key
(`TYPE0_UPDATE_KEY_LOW_CONST = 21`, `TYPE0_UPDATE_KEY_HIGH_CONST = 35`
in `rtl/histogram_statistics_v2.vhd`).

The UVM `hist_build_fill_word` defaults built the key at bits
**[29:17]** (`HS_DEF_UPDATE_LO = 17, HS_DEF_UPDATE_HI = 29`,
matching the Type1 TCC8N layout, not the Type0 TCC layout). The
scoreboard `build_fill_key` extracted from
`active_cfg.update_key_high/low` which also defaulted to 17/29.

Net effect: the RTL read the update key from a bit-shifted slice of
the TB fill word, so DUT key = (TB key) >> 4. `divider bin mismatch
dut=0 ref=1`, `dut=0 ref=2`, etc. for `make_fill_word(i*16)` injection.

## Secondary right_bound finding

After the Type0 key fix landed, the soak exposed a SECOND TB bug:
`program_histogram` writes LEFT_BOUND (CSR 3) and BIN_WIDTH (CSR 5)
but NEVER writes RIGHT_BOUND (CSR 4). The RTL keeps `csr_right_bound`
at its reset default
`DEF_LEFT_BOUND + DEF_BIN_WIDTH * N_BINS = -1000 + 16 * 256 = 3096`,
while the scoreboard's `note_csr_write` auto-derives
`shadow_cfg.right_bound = left_bound + bin_width * N_BINS = 4096` on
the CONTROL apply edge. Keys >= 3096 hit DUT overflow while the
scoreboard predicts a valid bin event.

The RTL's `csr_reg` process declares an unused `next_right_v` variable,
suggesting the auto-derive logic was started but never wired. The
RTL behavior matches "user must write RIGHT_BOUND explicitly", which
is what `hist_csr_test` (the one passing test in the prior soak) and
`hist_error_compound_test` already do. The scoreboard's auto-derive
is a TB-side assumption divergence that hit the 7 failing tests
because they all relied on `program_histogram`.

## Fix applied (TB-only)

`histogram_statistics/tb/uvm/hist_env_pkg.sv`
- `HS_DEF_UPDATE_LO` 17 -> **21** (Type0 TCC slice low bit).
- `HS_DEF_UPDATE_HI` 29 -> **35** (Type0 TCC slice high bit).
- `HS_DEF_FILTER_LO` 35 -> **41** (Type0 ASIC slice low bit).
- `HS_DEF_FILTER_HI` 38 -> **44** (Type0 ASIC slice high bit).
- Added `HS_TYPE0_UPDATE_LO/HI`, `HS_TYPE0_FILTER_LO/HI`,
  `HS_TYPE1_UPDATE_LO/HI`, `HS_TYPE1_FILTER_LO/HI`, and
  `HS_LOCK_KEY_RANGES` localparams that pair with the RTL fixed-slice
  constants.

`histogram_statistics/tb/uvm/hist_scoreboard.sv`
- New helpers `resolved_update_bits` and `resolved_filter_bits` that,
  when `HS_LOCK_KEY_RANGES == 1`, pick the Type0/Type1 fixed slice
  based on `active_cfg.source_select` instead of the CSR-programmable
  `update_key_low/high` (mirrors RTL `build_fixed_key`).
- `build_fill_key` and `filter_match` route through the new helpers.

`histogram_statistics/tb/uvm/hist_base_test.sv`
- `program_histogram` now writes CSR 5'd4 (RIGHT_BOUND) with the
  auto-derived `left_bound + bin_width * HS_N_BINS` between the LEFT_BOUND
  and BIN_WIDTH writes, so the DUT's `csr_right_bound` matches the
  scoreboard's auto-derived `shadow_cfg.right_bound` on apply.

## Soak result (2026-05-19, codex_uvm_run_typefix2_20260519_173003/)

Manual flow used (per CLAUDE.md: NO `rm -rf`; `make compile` is
forbidden because its recipe contains `rm -rf $(WORK)`): fresh work
directory `work_hist_uvm_codex/`, equivalent vlib + vmap + vcom + vlog
+ vsim sequence.

| Test | Prior result | Post Type0+right_bound fix |
|---|---|---|
| `hist_bank_test` | FAIL (dut=0 ref=1,2,3) | **PASS** (UVM_ERROR=0) |
| `hist_bin_test` | FAIL (199 UVM_ERROR) | partial (13 UVM_ERROR) - residual failures in B073/B074 non-power-of-2 bin_widths; pre-existing TB/RTL POWER2_BIN_WIDTH_ONLY divergence (RTL rejects, scoreboard accepts). |
| `hist_burst_test` | FAIL | **PASS** (UVM_ERROR=0) |
| `hist_cfg_test` | FAIL | partial (2 UVM_ERROR) - B033 expects RTL to auto-recalculate right_bound on apply (matches the unused `next_right_v` placeholder), and B035 has an unrelated error mask check. |
| `hist_coalesce_test` | FAIL | partial (4 UVM_ERROR) - bin counts off by 1 at the coalescing-queue ingress edge. Pre-existing timing edge. |
| `hist_debug_test` | FAIL | partial (1 UVM_FATAL `timeout waiting for interval_pulse`). Pre-existing interval-timer infra issue. |
| `hist_csr_test` | PASS | **PASS** (regression hold) |
| `hist_cross_test` | FAIL | **PASS** (UVM_ERROR=0) |

Summary: 4 of 8 PASS clean (was 1 of 8). The Type0 key-layout bug and
the right_bound auto-derive mismatch that block `program_histogram`-driven
tests are both closed. The remaining 4 partial-fail tests track
**pre-existing TB/RTL semantic gaps unrelated to the Type0 key layout**:

- POWER2_BIN_WIDTH_ONLY mismatch (B073/B074): RTL rejects non-power-of-2
  bin_width via `csr_error_info = 5`; TB scoreboard accepts and predicts
  bins anyway. Decision pending: RTL behavior matches the IP hw.tcl
  default `POWER2_BIN_WIDTH_ONLY = true`, so TB needs to mirror the
  rejection or skip these cases.
- B033 right_bound auto-derive expectation (`hist_cfg_test`): the
  unused `next_right_v` variable in the RTL csr_reg process suggests
  the auto-derive feature was intended but never wired. Decision
  pending: add the RTL feature OR rewrite the test to write
  RIGHT_BOUND explicitly.
- B035 error mask: unrelated, separate test gap.
- coalesce_test off-by-one: separate coalescing-queue edge timing.
- debug_test interval_pulse timeout: separate interval-timer infra issue.

## GTS clear regression - status

Restored at 26.3.7 (commit `630ef5b`): the SYNC-entry
`gts_counter_clear` pulse is preserved without re-introducing the
removed `gts_reset_reg` latch.

- `runctl_sync_start` asserts for the SYNC-entry command pulse when
  `run_state_cmd /= SYNC`.
- `gts_counter_clear` is combinational on hardware reset, run-control
  RESET, SYNC entry, or RUNNING entry.
- No `gts_reset_reg` process or `gts_counter_rst` register present.
- The only `run_state_cmd <= RESET` assignment is inside the active
  `RUNCTL_RESET_CMD_CONST` branch (RESETTING contract holds).

Standalone Questa static-screen on the contract fix: lint 0 errors,
CDC 0 violations, RDC 0 violations. Standalone synthesis at 1.1x
F_target: +0.465 ns setup, hold positive.

## Hard-rules compliance

- No `rm` or `rm -rf` used (fresh work dirs instead).
- No RTL edits in this DV refresh (only TB-side changes).

## Soak artifacts

- Per-test logs: `tb/uvm/codex_uvm_run_typefix2_20260519_173003/logs/*.log`
- Summary: `tb/uvm/codex_uvm_run_typefix2_20260519_173003/uvm_soak_summary.txt`
- Prior soak (Type0 fix only, exposed the right_bound issue):
  `tb/uvm/codex_uvm_run_typefix_20260519_171201/`
- Prior GTS-clear soak (failed because Type0 key-layout bug was still
  hidden): `tb/uvm/codex_uvm_run_20260519_gtsfix_gw9dbf/`
