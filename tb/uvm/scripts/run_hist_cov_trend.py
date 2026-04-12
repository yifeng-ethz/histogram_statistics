#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import os
import re
import subprocess
import sys
import time
from pathlib import Path


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
MAKE_BIN = os.environ.get("MAKE", "make")

TOTAL_LINE_RE = re.compile(
    r"^\s*(Assertions|Branches|Conditions|Covergroups|Expressions|Statements|Toggles|FSM States)"
    r"\s+(\d+|na)\s+(\d+|na)\s+(\d+|na)\s+(\d+|na)\s+([0-9.]+)%\s*$"
)
TOTAL_COVERAGE_RE = re.compile(r"^Total coverage \((?:Code Coverage Only, )?filtered view\):\s+([0-9]+\.[0-9]+)%\s*$")
DETAIL_SUMMARY_RE = re.compile(
    r"^\s*(Branches|Conditions|Expressions|Statements|Toggles|FSM States)\s+(\d+)\s+(\d+)\s+(\d+)\s+([0-9.]+)%\s*$"
)


def run_cmd(cmd: list[str], cwd: Path, log_path: Path | None = None) -> None:
    with subprocess.Popen(
        cmd,
        cwd=str(cwd),
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        bufsize=1,
    ) as proc:
        collected: list[str] = []
        assert proc.stdout is not None
        for line in proc.stdout:
            sys.stdout.write(line)
            collected.append(line)
        ret = proc.wait()
    if log_path is not None:
        log_path.write_text("".join(collected))
    if ret != 0:
        raise subprocess.CalledProcessError(ret, cmd)


def parse_total_report(report_path: Path) -> dict[str, float]:
    metrics: dict[str, float] = {}
    for line in report_path.read_text().splitlines():
        total_match = TOTAL_COVERAGE_RE.match(line)
        if total_match:
            metrics["total"] = float(total_match.group(1))
            continue
        match = TOTAL_LINE_RE.match(line)
        if not match:
            continue
        metrics[match.group(1).lower().replace(" ", "_")] = float(match.group(6))
    return metrics


def parse_dut_detail_report(report_path: Path) -> dict[str, float]:
    metrics: dict[str, float] = {}
    for line in report_path.read_text().splitlines():
        match = DETAIL_SUMMARY_RE.match(line)
        if not match:
            continue
        metrics[match.group(1).lower().replace(" ", "_")] = float(match.group(5))
    if "fsm_states" not in metrics:
        metrics["fsm_states"] = 0.0
    return metrics


def build_reports(ucdb: Path, reports_dir: Path, stem: str, cwd: Path) -> tuple[dict[str, float], dict[str, float]]:
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


def coverage_gap(metrics: dict[str, float]) -> float:
    keys = ["statements", "branches", "conditions", "expressions", "toggles"]
    return sum(max(0.0, 100.0 - metrics.get(key, 0.0)) for key in keys)


def maybe_plot(rows: list[dict[str, str]], out_png: Path) -> None:
    try:
        import matplotlib.pyplot as plt
    except Exception as exc:
        print(f"plot skipped: matplotlib unavailable ({exc})")
        return

    x = [float(row["cumulative_wall_s"]) for row in rows]
    fig, ax = plt.subplots(figsize=(10, 5))
    ax.plot(x, [float(row["total_pct"]) for row in rows], marker="o", label="dut total")
    ax.plot(x, [float(row["statements_pct"]) for row in rows], marker="o", label="stmt")
    ax.plot(x, [float(row["branches_pct"]) for row in rows], marker="o", label="branch")
    ax.plot(x, [float(row["toggles_pct"]) for row in rows], marker="o", label="toggle")
    ax.plot(x, [float(row["covergroups_pct"]) for row in rows], marker="o", label="cvg")
    ax.set_xlabel("cumulative wall time [s]")
    ax.set_ylabel("coverage [%]")
    ax.set_title("Histogram long-run coverage trend")
    ax.grid(True, alpha=0.3)
    ax.legend(loc="best")
    fig.tight_layout()
    fig.savefig(out_png, dpi=160)


def plateau_assessment(delta_total: float, delta_toggle: float, gain_per_s: float) -> str:
    if delta_total <= 0.05 and delta_toggle <= 0.10:
        return "hard plateau"
    if gain_per_s >= 0.50:
        return "very high value"
    if gain_per_s >= 0.10:
        return "productive"
    if gain_per_s >= 0.02:
        return "slowing"
    return "plateau"


def write_markdown_summary(
    out_path: Path,
    case_id: str,
    test: str,
    seed: int,
    points: list[int],
    plusargs_template: str,
    rows: list[dict[str, str]],
) -> None:
    lines: list[str] = []
    lines.append(f"# {case_id} Coverage Trend")
    lines.append("")
    lines.append(f"- Test: `{test}`")
    lines.append(f"- Seed: `{seed}`")
    lines.append(f"- Points: `{', '.join(str(point) for point in points)}`")
    lines.append(f"- Plusargs template: `{plusargs_template}`")
    lines.append("")
    lines.append("| Hits | Wall s | Cum Wall s | Total | Stmt | Branch | Cond | Expr | Toggle | Covergroups | Delta Total | Delta Toggle | Gap Gain | Gain %/s | Assessment |")
    lines.append("|-----:|-------:|-----------:|------:|-----:|-------:|-----:|-----:|-------:|------------:|------------:|-------------:|---------:|---------:|------------|")
    for row in rows:
        lines.append(
            "| {txn_count} | {wall_s} | {cumulative_wall_s} | {total_pct} | {statements_pct} | {branches_pct} | "
            "{conditions_pct} | {expressions_pct} | {toggles_pct} | {covergroups_pct} | {delta_total_pct} | "
            "{delta_toggle_pct} | {gap_gain} | {delta_total_pct_per_s} | {assessment} |".format(**row)
        )
    out_path.write_text("\n".join(lines) + "\n")


def main() -> int:
    parser = argparse.ArgumentParser(description="Run one histogram long-run coverage case across hit-count points")
    parser.add_argument("case_id")
    parser.add_argument("--uvm-dir", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--test", default="hist_real_hit_rand_test")
    parser.add_argument("--seed", type=int, default=101)
    parser.add_argument("--points", default="256,512,1024,2048")
    parser.add_argument("--outdir", type=Path)
    parser.add_argument("--plusargs-template", default="+HIST_REAL_HITS={hits}")
    parser.add_argument("--compile", action="store_true")
    args = parser.parse_args()

    uvm_dir = args.uvm_dir.resolve()
    outdir = args.outdir.resolve() if args.outdir else (uvm_dir / "cov_runs" / "coverage_trend" / args.case_id)
    logs_dir = outdir / "logs"
    reports_dir = outdir / "reports"
    ucdb_dir = outdir / "ucdb"
    for path in [logs_dir, reports_dir, ucdb_dir]:
        path.mkdir(parents=True, exist_ok=True)

    if args.compile:
        run_cmd([MAKE_BIN, "compile_cov"], uvm_dir, logs_dir / "compile.log")

    points = [int(part.strip()) for part in args.points.split(",") if part.strip()]
    if not points:
        raise SystemExit("no points provided")

    rows: list[dict[str, str]] = []
    cumulative_wall = 0.0
    previous_metrics = {
        "statements": 0.0,
        "branches": 0.0,
        "conditions": 0.0,
        "expressions": 0.0,
        "toggles": 0.0,
        "fsm_states": 0.0,
    }

    for point in points:
        stem = f"{args.case_id}_{point}"
        ucdb = ucdb_dir / f"{stem}.ucdb"
        log_path = logs_dir / f"{stem}.log"
        plusargs = args.plusargs_template.format(hits=point)

        cmd = [
            MAKE_BIN,
            "run_cov",
            f"TEST={args.test}",
            f"SEED={args.seed}",
            f"UCDB={ucdb}",
            f"PLUSARGS={plusargs}",
        ]
        start = time.perf_counter()
        run_cmd(cmd, uvm_dir, log_path)
        wall_s = time.perf_counter() - start
        cumulative_wall += wall_s

        dut_metrics, totals = build_reports(ucdb, reports_dir, stem, uvm_dir)
        current_total = totals.get("total", 0.0)
        previous_total = float(rows[-1]["total_pct"]) if rows else 0.0
        delta_total = current_total - previous_total
        delta_toggle = dut_metrics.get("toggles", 0.0) - previous_metrics.get("toggles", 0.0)
        gap_gain = coverage_gap(previous_metrics) - coverage_gap(dut_metrics)
        gain_per_s = 0.0 if wall_s == 0.0 else delta_total / wall_s
        row = {
            "case_id": args.case_id,
            "test": args.test,
            "seed": str(args.seed),
            "txn_count": str(point),
            "wall_s": f"{wall_s:.3f}",
            "cumulative_wall_s": f"{cumulative_wall:.3f}",
            "total_pct": f"{current_total:.2f}",
            "statements_pct": f"{dut_metrics.get('statements', 0.0):.2f}",
            "branches_pct": f"{dut_metrics.get('branches', 0.0):.2f}",
            "conditions_pct": f"{dut_metrics.get('conditions', 0.0):.2f}",
            "expressions_pct": f"{dut_metrics.get('expressions', 0.0):.2f}",
            "toggles_pct": f"{dut_metrics.get('toggles', 0.0):.2f}",
            "fsm_states_pct": f"{dut_metrics.get('fsm_states', 0.0):.2f}",
            "covergroups_pct": f"{totals.get('covergroups', 0.0):.2f}",
            "assertions_pct": f"{totals.get('assertions', 0.0):.2f}",
            "delta_total_pct": f"{delta_total:.2f}",
            "delta_toggle_pct": f"{delta_toggle:.2f}",
            "gap_gain": f"{gap_gain:.2f}",
            "delta_total_pct_per_s": f"{gain_per_s:.3f}",
            "assessment": plateau_assessment(delta_total, delta_toggle, gain_per_s),
            "ucdb": str(ucdb),
            "log": str(log_path),
        }
        rows.append(row)
        previous_metrics = dut_metrics
        print(
            f"case={args.case_id} hits={point} wall={wall_s:.3f}s total={row['total_pct']} "
            f"stmt={row['statements_pct']} branch={row['branches_pct']} toggle={row['toggles_pct']} "
            f"cvg={row['covergroups_pct']} gain/s={row['delta_total_pct_per_s']}"
        )

    csv_path = outdir / f"{args.case_id}_trend.csv"
    with csv_path.open("w", newline="") as fh:
        writer = csv.DictWriter(
            fh,
            fieldnames=[
                "case_id",
                "test",
                "seed",
                "txn_count",
                "wall_s",
                "cumulative_wall_s",
                "total_pct",
                "statements_pct",
                "branches_pct",
                "conditions_pct",
                "expressions_pct",
                "toggles_pct",
                "fsm_states_pct",
                "covergroups_pct",
                "assertions_pct",
                "delta_total_pct",
                "delta_toggle_pct",
                "gap_gain",
                "delta_total_pct_per_s",
                "assessment",
                "ucdb",
                "log",
            ],
        )
        writer.writeheader()
        writer.writerows(rows)

    maybe_plot(rows, outdir / f"{args.case_id}_trend.png")
    write_markdown_summary(outdir / f"{args.case_id}_summary.md", args.case_id, args.test, args.seed, points, args.plusargs_template, rows)
    print(f"csv={csv_path}")
    print(f"png={outdir / f'{args.case_id}_trend.png'}")
    print(f"summary={outdir / f'{args.case_id}_summary.md'}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
