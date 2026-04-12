#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import json
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path


DEFAULT_CASES = [
    {
        "case_id": "P141_real_triad",
        "test": "hist_real_hit_rand_test",
        "points": "256,512,1024,2048",
        "plusargs_template": "+HIST_REAL_PROFILE=triad +HIST_REAL_HITS={hits}",
    },
    {
        "case_id": "P142_real_boundary",
        "test": "hist_real_hit_rand_test",
        "points": "256,512,1024,2048",
        "plusargs_template": "+HIST_REAL_PROFILE=boundary +HIST_REAL_HITS={hits}",
    },
    {
        "case_id": "P143_real_queue",
        "test": "hist_real_hit_rand_test",
        "points": "256,512,1024,2048",
        "plusargs_template": "+HIST_REAL_PROFILE=queue +HIST_REAL_HITS={hits}",
    },
    {
        "case_id": "P144_real_interval",
        "test": "hist_real_hit_rand_test",
        "points": "128,256,512,1024",
        "plusargs_template": "+HIST_REAL_PROFILE=interval +HIST_REAL_HITS={hits}",
    },
]


def run_logged(cmd: list[str], cwd: Path, log_path: Path | None = None) -> None:
    proc = subprocess.run(cmd, cwd=str(cwd), text=True, capture_output=True, check=False)
    if log_path is not None:
        log_path.write_text(proc.stdout + proc.stderr)
    if proc.returncode != 0:
        print(proc.stdout)
        print(proc.stderr, file=sys.stderr)
        raise subprocess.CalledProcessError(proc.returncode, cmd)


def load_rows(csv_path: Path) -> list[dict[str, str]]:
    with csv_path.open() as fh:
        return list(csv.DictReader(fh))


def write_summary(
    out_path: Path,
    baseline_label: str,
    baseline_wall: float,
    baseline_total: float,
    cases: list[dict[str, str]],
    suite_rows: list[dict[str, str]],
) -> None:
    lines: list[str] = []
    lines.append("# Histogram Long-Run Coverage Farm")
    lines.append("")
    lines.append("This report uses `delta_total_pct_per_s` as an engineering proxy for incremental observability gain per wall-clock second.")
    lines.append("It is not literal Fisher information, but it is the right signoff proxy here: if structural coverage stops moving, added runtime is no longer buying new observable state-space information.")
    lines.append("")
    lines.append("## Baseline")
    lines.append("")
    lines.append(f"- Baseline: `{baseline_label}`")
    lines.append(f"- Baseline wall time: `{baseline_wall:.2f} s`")
    lines.append(f"- Baseline DUT total: `{baseline_total:.2f}%`")
    lines.append("")
    lines.append("## Final Per-Case Trend Points")
    lines.append("")
    lines.append("| Case | Hits | Wall s | Total | Stmt | Branch | Cond | Expr | Toggle | Covergroups | Final Gain %/s | Assessment |")
    lines.append("|------|-----:|-------:|------:|-----:|-------:|-----:|-----:|-------:|------------:|---------------:|------------|")
    for case in cases:
        lines.append(
            "| {case_id} | {txn_count} | {cumulative_wall_s} | {total_pct} | {statements_pct} | {branches_pct} | "
            "{conditions_pct} | {expressions_pct} | {toggles_pct} | {covergroups_pct} | {delta_total_pct_per_s} | {assessment} |".format(**case)
        )
    lines.append("")
    lines.append("## Suite Merge")
    lines.append("")
    lines.append("| Step | Case | Wall s | Cum Wall s | Total | Stmt | Branch | Cond | Expr | Toggle | Delta Total | Delta Toggle | Gain %/s |")
    lines.append("|-----:|------|-------:|-----------:|------:|-----:|-------:|-----:|-----:|-------:|------------:|-------------:|---------:|")
    for row in suite_rows:
        lines.append(
            "| {step} | {case_id} | {wall_s} | {cumulative_wall_s} | {total_pct} | {statements_pct} | {branches_pct} | "
            "{conditions_pct} | {expressions_pct} | {toggles_pct} | {delta_total_pct} | {delta_toggle_pct} | {delta_total_pct_per_s} |".format(**row)
        )
    lines.append("")
    lines.append("## Promotion Guidance")
    lines.append("")
    for idx, row in enumerate(suite_rows[1:], start=1):
        gain = float(row["delta_total_pct_per_s"])
        delta_toggle = float(row["delta_toggle_pct"])
        prev_toggle = float(suite_rows[idx - 1]["toggles_pct"])
        curr_toggle = float(row["toggles_pct"])
        if prev_toggle < 80.0 <= curr_toggle:
            verdict = "required in the routine promoted long-run suite because it closes toggle coverage"
        elif gain < 0.02 and delta_toggle <= 0.10:
            verdict = "do not deepen further without a new stimulus idea"
        elif gain < 0.10:
            verdict = "keep only for publication/signoff, not for routine regression"
        else:
            verdict = "productive enough to keep in the promoted long-run suite"
        lines.append(f"- `{row['case_id']}`: {verdict}.")
    out_path.write_text("\n".join(lines) + "\n")


def main() -> int:
    parser = argparse.ArgumentParser(description="Run histogram long-run coverage-trend cases in parallel and merge the suite result")
    parser.add_argument("--uvm-dir", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--outdir", type=Path, default=None)
    parser.add_argument("--baseline-run", type=Path, required=True)
    parser.add_argument("--jobs", type=int, default=2)
    parser.add_argument("--seed", type=int, default=101)
    parser.add_argument("--compile", action="store_true")
    args = parser.parse_args()

    uvm_dir = args.uvm_dir.resolve()
    scripts_dir = Path(__file__).resolve().parent
    outdir = args.outdir.resolve() if args.outdir else (uvm_dir / "cov_runs" / "signoff_longrun_farm")
    trends_dir = outdir / "trends"
    suite_dir = outdir / "suite"
    logs_dir = outdir / "logs"
    for path in [trends_dir, suite_dir, logs_dir]:
        path.mkdir(parents=True, exist_ok=True)

    baseline_run = args.baseline_run.resolve()
    baseline_summary = json.loads((baseline_run / "summary.json").read_text())
    baseline_ucdb = baseline_run / "ucdb" / "merged_baseline.ucdb"
    if not baseline_ucdb.exists():
        raise SystemExit(f"missing baseline UCDB: {baseline_ucdb}")
    baseline_wall = float(baseline_summary.get("deterministic_wall_seconds", 0.0))
    if args.compile:
        run_logged(["make", "compile_cov"], uvm_dir, logs_dir / "compile.log")

    def run_case(case: dict[str, str]) -> Path:
        cmd = [
            sys.executable,
            str(scripts_dir / "run_hist_cov_trend.py"),
            case["case_id"],
            "--uvm-dir",
            str(uvm_dir),
            "--test",
            case["test"],
            "--seed",
            str(args.seed),
            "--points",
            case["points"],
            "--outdir",
            str(trends_dir / case["case_id"]),
            "--plusargs-template",
            case["plusargs_template"],
        ]
        run_logged(cmd, uvm_dir, logs_dir / f"{case['case_id']}.log")
        return trends_dir / case["case_id"] / f"{case['case_id']}_trend.csv"

    trend_csvs: list[Path] = []
    with ThreadPoolExecutor(max_workers=max(1, args.jobs)) as executor:
        future_map = {executor.submit(run_case, case): case for case in DEFAULT_CASES}
        for future in as_completed(future_map):
            case = future_map[future]
            csv_path = future.result()
            trend_csvs.append(csv_path)
            print(f"completed {case['case_id']}: {csv_path}")

    ordered_csvs = [trends_dir / case["case_id"] / f"{case['case_id']}_trend.csv" for case in DEFAULT_CASES]
    merge_cmd = [
        sys.executable,
        str(scripts_dir / "merge_hist_cov_suite.py"),
        "--outdir",
        str(suite_dir),
        "--base-ucdb",
        str(baseline_ucdb),
        "--base-wall-s",
        str(baseline_wall),
        "--base-label",
        f"{baseline_run.name}_deterministic",
    ] + [str(path) for path in ordered_csvs]
    run_logged(merge_cmd, uvm_dir, logs_dir / "suite_merge.log")

    final_case_rows = [load_rows(path)[-1] for path in ordered_csvs]
    suite_rows = load_rows(suite_dir / "suite_trend.csv")
    baseline_total = float(suite_rows[0]["total_pct"])
    write_summary(
        outdir / "summary.md",
        f"{baseline_run.name}_deterministic",
        baseline_wall,
        baseline_total,
        final_case_rows,
        suite_rows,
    )
    print(f"summary={outdir / 'summary.md'}")
    print(f"suite_csv={suite_dir / 'suite_trend.csv'}")
    print(f"suite_png={suite_dir / 'suite_trend.png'}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
