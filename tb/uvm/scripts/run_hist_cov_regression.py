#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import subprocess
import sys
import time
from dataclasses import dataclass, asdict
from pathlib import Path
from typing import Dict, List, Optional


DEFAULT_RANDOM_SEEDS = [101, 202, 303, 404, 505]
EXCLUDED_DETERMINISTIC_TESTS = {
    "hist_real_hit_rand_test",
    "test_codex",
}
DEFAULT_QUESTA = (
    "/data1/intelFPGA_pro/23.1/questa_fse"
    if Path("/data1/intelFPGA_pro/23.1/questa_fse").exists()
    else "/data1/intelFPGA_pro/23.1/questa_fe"
)
QUESTA_HOME = Path(os.environ.get("QUESTA_HOME", DEFAULT_QUESTA))
VCOVER_BIN = next(
    (path for path in [QUESTA_HOME / "bin" / "vcover", QUESTA_HOME / "linux_x86_64" / "vcover"] if path.exists()),
    QUESTA_HOME / "bin" / "vcover",
)
MAKE_BIN = shutil.which("make") or "make"

TOTAL_LINE_RE = re.compile(
    r"^\s*(Assertions|Branches|Conditions|Covergroups|Expressions|Statements|Toggles|FSM States)"
    r"\s+(\d+|na)\s+(\d+|na)\s+(\d+|na)\s+(\d+|na)\s+([0-9.]+)%\s*$"
)
DETAIL_SECTION_RE = re.compile(r"^(Branch|Condition|Expression|Statement|Toggle|FSM State) Coverage:$")
DETAIL_SUMMARY_RE = re.compile(
    r"^\s*(Branches|Conditions|Expressions|Statements|Toggles|FSM States)\s+(\d+)\s+(\d+)\s+(\d+)\s+([0-9.]+)%\s*$"
)
SIM_ERRORS_RE = re.compile(r"^\# Errors:\s+(\d+)\b", re.M)
UVM_ERROR_RE = re.compile(r"^\# UVM_ERROR\s*:\s*(\d+)\b", re.M)
UVM_FATAL_RE = re.compile(r"^\# UVM_FATAL\s*:\s*(\d+)\b", re.M)


@dataclass
class CaseResult:
    test: str
    seed: int
    kind: str
    status: str
    wall_seconds: float
    ucdb: str
    dut_code: Dict[str, float]
    totals: Dict[str, float]
    log: str
    sim_errors: int
    uvm_errors: int
    uvm_fatals: int


def case_key(test: str, seed: int, kind: str) -> tuple[str, int, str]:
    return (test, seed, kind)


def run_cmd(cmd: List[str], cwd: Path, log_path: Optional[Path] = None) -> None:
    with subprocess.Popen(
        cmd,
        cwd=str(cwd),
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        bufsize=1,
    ) as proc:
        collected: List[str] = []
        assert proc.stdout is not None
        for line in proc.stdout:
            sys.stdout.write(line)
            collected.append(line)
        ret = proc.wait()
    if log_path is not None:
        log_path.write_text("".join(collected))
    if ret != 0:
        raise subprocess.CalledProcessError(ret, cmd)


def parse_total_report(report_path: Path) -> Dict[str, float]:
    metrics: Dict[str, float] = {}
    for line in report_path.read_text().splitlines():
      m = TOTAL_LINE_RE.match(line)
      if not m:
        continue
      name = m.group(1).lower().replace(" ", "_")
      metrics[name] = float(m.group(6))
    return metrics


def parse_dut_detail_report(report_path: Path) -> Dict[str, float]:
    metrics: Dict[str, float] = {}
    for line in report_path.read_text().splitlines():
        m = DETAIL_SUMMARY_RE.match(line)
        if not m:
            continue
        name = m.group(1).lower().replace(" ", "_")
        metrics[name] = float(m.group(5))
    if "fsm_states" not in metrics:
        metrics["fsm_states"] = 0.0
    return metrics


def classify_log(log_path: Path) -> tuple[str, int, int, int]:
    text = log_path.read_text()
    sim_matches = SIM_ERRORS_RE.findall(text)
    uvm_error_matches = UVM_ERROR_RE.findall(text)
    uvm_fatal_matches = UVM_FATAL_RE.findall(text)

    sim_errors = int(sim_matches[-1]) if sim_matches else 0
    uvm_errors = int(uvm_error_matches[-1]) if uvm_error_matches else 0
    uvm_fatals = int(uvm_fatal_matches[-1]) if uvm_fatal_matches else 0

    status = "PASS" if sim_errors == 0 and uvm_errors == 0 and uvm_fatals == 0 else "FAIL"
    return status, sim_errors, uvm_errors, uvm_fatals


def compute_incremental_steps(
    items: List[CaseResult],
    ucdb_dir: Path,
    reports_dir: Path,
    cwd: Path,
    prefix: str,
    base_inputs: Optional[List[Path]] = None,
    base_metrics: Optional[Dict[str, float]] = None,
) -> List[Dict[str, object]]:
    steps: List[Dict[str, object]] = []
    previous_metrics = dict(base_metrics or {
        "statements": 0.0,
        "branches": 0.0,
        "conditions": 0.0,
        "expressions": 0.0,
        "toggles": 0.0,
        "fsm_states": 0.0,
    })
    merged_inputs: List[Path] = list(base_inputs or [])

    for idx, item in enumerate(items, start=1):
        merged_inputs.append(cwd / item.ucdb)
        merged_step = ucdb_dir / f"{prefix}_{idx}.ucdb"
        merge_ucdbs(merged_step, merged_inputs, cwd)
        step_metrics, _ = build_reports(merged_step, reports_dir, f"{prefix}_{idx}", cwd)
        delta = {
            key: step_metrics.get(key, 0.0) - previous_metrics.get(key, 0.0)
            for key in ["statements", "branches", "conditions", "expressions", "toggles", "fsm_states"]
        }
        gap_gain = coverage_gap(previous_metrics) - coverage_gap(step_metrics)
        steps.append(
            {
                "test": item.test,
                "seed": item.seed,
                "kind": item.kind,
                "delta": delta,
                "gap_gain": gap_gain,
                "merged_metrics": step_metrics,
            }
        )
        previous_metrics = step_metrics

    return steps


def load_existing_wall_seconds(summary_path: Path) -> Dict[tuple[str, int, str], float]:
    if not summary_path.exists():
        return {}
    try:
        data = json.loads(summary_path.read_text())
    except json.JSONDecodeError:
        return {}

    results = data.get("results", [])
    wall_seconds: Dict[tuple[str, int, str], float] = {}
    for item in results:
        try:
            key = case_key(item["test"], int(item["seed"]), item["kind"])
            wall_seconds[key] = float(item.get("wall_seconds", 0.0))
        except (KeyError, TypeError, ValueError):
            continue
    return wall_seconds


def write_markdown_summary(
    out_path: Path,
    results: List[CaseResult],
    merged_metrics: Dict[str, float],
    merged_totals: Dict[str, float],
    deterministic_steps: List[Dict[str, object]],
    plateau_steps: List[Dict[str, object]],
) -> None:
    deterministic_wall = sum(item.wall_seconds for item in results if item.kind == "deterministic")
    random_wall = sum(item.wall_seconds for item in results if item.kind == "random")
    total_wall = sum(item.wall_seconds for item in results)

    lines: List[str] = []
    lines.append("# Histogram Coverage Regression")
    lines.append("")
    lines.append("## Runtime")
    lines.append("")
    lines.append("| Total Wall s | Deterministic Wall s | Random Wall s | Cases |")
    lines.append("|-------------:|---------------------:|--------------:|------:|")
    lines.append(
        "| {total:.2f} | {deterministic:.2f} | {random:.2f} | {cases} |".format(
            total=total_wall,
            deterministic=deterministic_wall,
            random=random_wall,
            cases=len(results),
        )
    )
    lines.append("")
    lines.append("## Per-Case Results")
    lines.append("")
    lines.append("| Test | Seed | Kind | Status | Wall s | Statement | Branch | Condition | Expression | Toggle | FSM |")
    lines.append("|------|------|------|--------|-------:|----------:|-------:|----------:|-----------:|-------:|----:|")
    for item in results:
        lines.append(
            "| {test} | {seed} | {kind} | {status} | {wall:.2f} | {statement:.2f} | {branch:.2f} | {condition:.2f} | {expression:.2f} | {toggle:.2f} | {fsm:.2f} |".format(
                test=item.test,
                seed=item.seed,
                kind=item.kind,
                status=item.status,
                wall=item.wall_seconds,
                statement=item.dut_code.get("statements", 0.0),
                branch=item.dut_code.get("branches", 0.0),
                condition=item.dut_code.get("conditions", 0.0),
                expression=item.dut_code.get("expressions", 0.0),
                toggle=item.dut_code.get("toggles", 0.0),
                fsm=item.dut_code.get("fsm_states", 0.0),
            )
        )
    lines.append("")
    lines.append("## Merged DUT Coverage")
    lines.append("")
    lines.append("| Statement | Branch | Condition | Expression | Toggle | FSM | Assertions | Covergroups |")
    lines.append("|----------:|-------:|----------:|-----------:|-------:|----:|-----------:|------------:|")
    lines.append(
        "| {statement:.2f} | {branch:.2f} | {condition:.2f} | {expression:.2f} | {toggle:.2f} | {fsm:.2f} | {assertions:.2f} | {covergroups:.2f} |".format(
            statement=merged_metrics.get("statements", 0.0),
            branch=merged_metrics.get("branches", 0.0),
            condition=merged_metrics.get("conditions", 0.0),
            expression=merged_metrics.get("expressions", 0.0),
            toggle=merged_metrics.get("toggles", 0.0),
            fsm=merged_metrics.get("fsm_states", 0.0),
            assertions=merged_totals.get("assertions", 0.0),
            covergroups=merged_totals.get("covergroups", 0.0),
        )
    )
    lines.append("")
    lines.append("## Deterministic Incremental Gain")
    lines.append("")
    lines.append("| Test | Seed | Statement Δ | Branch Δ | Condition Δ | Expression Δ | Toggle Δ | Gap Gain |")
    lines.append("|------|------|------------:|---------:|------------:|-------------:|---------:|---------:|")
    for step in deterministic_steps:
        delta = step["delta"]
        lines.append(
            "| {test} | {seed} | {statement:.2f} | {branch:.2f} | {condition:.2f} | {expression:.2f} | {toggle:.2f} | {gap_gain:.2f} |".format(
                test=step["test"],
                seed=step["seed"],
                statement=delta.get("statements", 0.0),
                branch=delta.get("branches", 0.0),
                condition=delta.get("conditions", 0.0),
                expression=delta.get("expressions", 0.0),
                toggle=delta.get("toggles", 0.0),
                gap_gain=step["gap_gain"],
            )
        )
    lines.append("")
    lines.append("## Random-Seed Incremental Gain")
    lines.append("")
    lines.append("| Seed | Statement Δ | Branch Δ | Condition Δ | Expression Δ | Toggle Δ | Gap Gain |")
    lines.append("|------|------------:|---------:|------------:|-------------:|---------:|---------:|")
    for step in plateau_steps:
        delta = step["delta"]
        lines.append(
            "| {seed} | {statement:.2f} | {branch:.2f} | {condition:.2f} | {expression:.2f} | {toggle:.2f} | {gap_gain:.2f} |".format(
                seed=step["seed"],
                statement=delta.get("statements", 0.0),
                branch=delta.get("branches", 0.0),
                condition=delta.get("conditions", 0.0),
                expression=delta.get("expressions", 0.0),
                toggle=delta.get("toggles", 0.0),
                gap_gain=step["gap_gain"],
            )
        )
    out_path.write_text("\n".join(lines) + "\n")


def merge_ucdbs(out_ucdb: Path, ucdbs: List[Path], cwd: Path) -> None:
    cmd = [str(VCOVER_BIN), "merge", str(out_ucdb)] + [str(path) for path in ucdbs]
    run_cmd(cmd, cwd)


def build_reports(ucdb: Path, reports_dir: Path, stem: str, cwd: Path) -> tuple[Dict[str, float], Dict[str, float]]:
    total_report = reports_dir / f"{stem}_totals.txt"
    dut_report = reports_dir / f"{stem}_dut.txt"

    run_cmd(
        [
            str(VCOVER_BIN),
            "report",
            "-summary",
            "-codeAll",
            "-cvg",
            "-assert",
            "-output",
            str(total_report),
            str(ucdb),
        ],
        cwd,
    )
    run_cmd(
        [
            str(VCOVER_BIN),
            "report",
            "-details",
            "-codeAll",
            "-instance=/tb_top/dut",
            "-output",
            str(dut_report),
            str(ucdb),
        ],
        cwd,
    )
    return parse_dut_detail_report(dut_report), parse_total_report(total_report)


def coverage_gap(metrics: Dict[str, float]) -> float:
    keys = ["statements", "branches", "conditions", "expressions", "toggles"]
    return sum(max(0.0, 100.0 - metrics.get(key, 0.0)) for key in keys)


def discover_deterministic_tests(uvm_dir: Path) -> List[str]:
    tests_dir = uvm_dir / "tests"
    test_names = sorted(path.stem for path in tests_dir.glob("*_test.sv"))
    return [name for name in test_names if name not in EXCLUDED_DETERMINISTIC_TESTS]


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--uvm-dir", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--label", default=time.strftime("%Y%m%d_%H%M%S"))
    parser.add_argument("--real-hits", type=int, default=256)
    parser.add_argument("--random-seeds", type=int, nargs="*", default=DEFAULT_RANDOM_SEEDS)
    parser.add_argument(
        "--reuse-run",
        type=Path,
        help="Reuse an existing cov_runs/<label> directory, rebuild reports, and rewrite the summary without rerunning simulation.",
    )
    args = parser.parse_args()

    uvm_dir = args.uvm_dir.resolve()
    run_dir = args.reuse_run.resolve() if args.reuse_run else (uvm_dir / "cov_runs" / args.label)
    label = run_dir.name if args.reuse_run else args.label
    logs_dir = run_dir / "logs"
    reports_dir = run_dir / "reports"
    ucdb_dir = run_dir / "ucdb"
    for path in [logs_dir, reports_dir, ucdb_dir]:
        path.mkdir(parents=True, exist_ok=True)

    deterministic_tests = discover_deterministic_tests(uvm_dir)
    deterministic_cases = [{"test": test, "seed": 101, "kind": "deterministic"} for test in deterministic_tests]
    random_cases = [{"test": "hist_real_hit_rand_test", "seed": seed, "kind": "random"} for seed in args.random_seeds]
    all_cases = deterministic_cases + random_cases

    if not args.reuse_run:
        compile_log = logs_dir / "compile_cov.log"
        run_cmd([MAKE_BIN, "clean", "compile_cov"], uvm_dir, compile_log)

    existing_wall_seconds = load_existing_wall_seconds(run_dir / "summary.json")

    results: List[CaseResult] = []
    had_failures = False
    for case in all_cases:
        stem = f"{case['test']}_{case['seed']}"
        ucdb = ucdb_dir / f"{stem}.ucdb"
        log = logs_dir / f"{stem}.log"
        if args.reuse_run:
            if not ucdb.exists():
                raise FileNotFoundError(f"Missing UCDB for reuse mode: {ucdb}")
            if not log.exists():
                raise FileNotFoundError(f"Missing log for reuse mode: {log}")
            wall_seconds = existing_wall_seconds.get(case_key(case["test"], case["seed"], case["kind"]), 0.0)
        else:
            cmd = [
                MAKE_BIN,
                "run_cov",
                f"TEST={case['test']}",
                f"SEED={case['seed']}",
                f"UCDB={ucdb}",
            ]
            start = time.perf_counter()
            if case["test"] == "hist_real_hit_rand_test":
                cmd.append(f"PLUSARGS=+HIST_REAL_HITS={args.real_hits}")
            run_cmd(cmd, uvm_dir, log)
            wall_seconds = time.perf_counter() - start
        status, sim_errors, uvm_errors, uvm_fatals = classify_log(log)
        had_failures |= status != "PASS"
        dut_code, totals = build_reports(ucdb, reports_dir, stem, uvm_dir)
        results.append(
            CaseResult(
                test=case["test"],
                seed=case["seed"],
                kind=case["kind"],
                status=status,
                wall_seconds=wall_seconds,
                ucdb=str(ucdb.relative_to(uvm_dir)),
                dut_code=dut_code,
                totals=totals,
                log=str(log.relative_to(uvm_dir)),
                sim_errors=sim_errors,
                uvm_errors=uvm_errors,
                uvm_fatals=uvm_fatals,
            )
        )

    merged_ucdb = ucdb_dir / "merged_all.ucdb"
    merge_ucdbs(merged_ucdb, [uvm_dir / item.ucdb for item in results], uvm_dir)
    merged_code, merged_totals = build_reports(merged_ucdb, reports_dir, "merged_all", uvm_dir)

    deterministic_items = [item for item in results if item.kind == "deterministic"]
    deterministic_steps = compute_incremental_steps(deterministic_items, ucdb_dir, reports_dir, uvm_dir, "merged_deterministic")

    baseline_ucdb = ucdb_dir / "merged_baseline.ucdb"
    baseline_inputs = [uvm_dir / item.ucdb for item in results if item.kind != "random"]
    merge_ucdbs(baseline_ucdb, baseline_inputs, uvm_dir)
    baseline_metrics, _ = build_reports(baseline_ucdb, reports_dir, "merged_baseline", uvm_dir)
    plateau_steps = compute_incremental_steps(
        [item for item in results if item.kind == "random"],
        ucdb_dir,
        reports_dir,
        uvm_dir,
        "merged_random",
        base_inputs=baseline_inputs,
        base_metrics=baseline_metrics,
    )

    summary = {
        "label": label,
        "uvm_dir": str(uvm_dir),
        "real_hits": args.real_hits,
        "deterministic_tests": deterministic_tests,
        "results": [asdict(item) for item in results],
        "merged_code": merged_code,
        "merged_totals": merged_totals,
        "deterministic_steps": deterministic_steps,
        "plateau_steps": plateau_steps,
        "aggregate_wall_seconds": sum(item.wall_seconds for item in results),
        "deterministic_wall_seconds": sum(item.wall_seconds for item in results if item.kind == "deterministic"),
        "random_wall_seconds": sum(item.wall_seconds for item in results if item.kind == "random"),
        "overall_status": "PASS" if not had_failures else "FAIL",
    }
    (run_dir / "summary.json").write_text(json.dumps(summary, indent=2, sort_keys=True) + "\n")
    write_markdown_summary(run_dir / "summary.md", results, merged_code, merged_totals, deterministic_steps, plateau_steps)
    print(f"\nWrote regression summary to {run_dir / 'summary.json'}")
    print(f"Wrote regression markdown to {run_dir / 'summary.md'}")
    return 1 if had_failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
