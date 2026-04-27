# DV Coverage Summary — histogram_statistics_v2 Phase-5 Gate

This page records the current coverage status for the Phase-5 queue-depth gate.
The present evidence is directed functional closure plus synthesis closure;
merged UCDB coverage is not claimed yet.

## Legend

✅ pass / closed &middot; ⚠️ partial / below target / known limitation &middot; ❌ failed / missing evidence &middot; ❓ pending &middot; ℹ️ informational

## Coverage Category Status

| status | metric | result |
|:---:|---|---|
| ⚠️ | statement | not merged for this Phase-5 hand-run gate |
| ⚠️ | branch | not merged for this Phase-5 hand-run gate |
| ⚠️ | toggle | not merged for this Phase-5 hand-run gate |
| ✅ | directed functional evidence | `27 PASS, 0 FAIL` |

## Non-Claims

This page does not claim structural coverage closure. The Phase-5 acceptance
claim is limited to model agreement, directed standalone DV, standalone
synthesis timing closure, active Qsys regeneration, and DP E2E integration sim.

_Regenerate with `python3 histogram_statistics/tb/scripts/generate_dv_report.py` when a generated coverage dashboard exists._
