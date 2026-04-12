#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import os
import subprocess
import sys
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


def run(cmd: list[str]) -> None:
    proc = subprocess.run(cmd, text=True, capture_output=True, check=False)
    if proc.returncode != 0:
        print(proc.stdout)
        print(proc.stderr, file=sys.stderr)
        raise subprocess.CalledProcessError(proc.returncode, cmd)


def parse_total_report(report_path: Path) -> dict[str, float]:
    import re

    line_re = re.compile(
        r"^\s*(Assertions|Branches|Conditions|Covergroups|Expressions|Statements|Toggles|FSM States)"
        r"\s+(\d+|na)\s+(\d+|na)\s+(\d+|na)\s+(\d+|na)\s+([0-9.]+)%\s*$"
    )
    total_re = re.compile(r"^Total coverage \((?:Code Coverage Only, )?filtered view\):\s+([0-9]+\.[0-9]+)%\s*$")
    metrics: dict[str, float] = {}
    for line in report_path.read_text().splitlines():
        total_match = total_re.match(line)
        if total_match:
            metrics["total"] = float(total_match.group(1))
            continue
        match = line_re.match(line)
        if not match:
            continue
        metrics[match.group(1).lower().replace(" ", "_")] = float(match.group(6))
    return metrics


def parse_dut_report(report_path: Path) -> dict[str, float]:
    import re

    line_re = re.compile(
        r"^\s*(Branches|Conditions|Expressions|Statements|Toggles|FSM States)\s+(\d+)\s+(\d+)\s+(\d+)\s+([0-9.]+)%\s*$"
    )
    metrics: dict[str, float] = {}
    for line in report_path.read_text().splitlines():
        match = line_re.match(line)
        if not match:
            continue
        metrics[match.group(1).lower().replace(" ", "_")] = float(match.group(5))
    if "fsm_states" not in metrics:
        metrics["fsm_states"] = 0.0
    return metrics


def build_reports(ucdb: Path, reports_dir: Path, stem: str) -> tuple[dict[str, float], dict[str, float]]:
    total_report = reports_dir / f"{stem}_totals.txt"
    dut_report = reports_dir / f"{stem}_dut.txt"
    run(
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
        ]
    )
    run(
        [
            str(VCOVER_BIN),
            "report",
            "-details",
            "-codeAll",
            "-instance=/tb_top/dut",
            "-output",
            str(dut_report),
            str(ucdb),
        ]
    )
    return parse_dut_report(dut_report), parse_total_report(total_report)


def coverage_gap(metrics: dict[str, float]) -> float:
    keys = ["statements", "branches", "conditions", "expressions", "toggles"]
    return sum(max(0.0, 100.0 - metrics.get(key, 0.0)) for key in keys)


def load_rows(csv_path: Path) -> list[dict[str, str]]:
    with csv_path.open() as fh:
        return list(csv.DictReader(fh))


def read_final_row(csv_path: Path) -> dict[str, str]:
    rows = load_rows(csv_path)
    if not rows:
        raise SystemExit(f"empty trend csv: {csv_path}")
    rows.sort(key=lambda row: int(row["txn_count"]))
    return rows[-1]


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
    ax.set_title("Histogram suite coverage trend")
    ax.grid(True, alpha=0.3)
    ax.legend(loc="best")
    fig.tight_layout()
    fig.savefig(out_png, dpi=160)


def main() -> int:
    parser = argparse.ArgumentParser(description="Merge histogram coverage-trend final UCDBs into a suite trend")
    parser.add_argument("trend_csv", nargs="+")
    parser.add_argument("--outdir", required=True, type=Path)
    parser.add_argument("--base-ucdb", type=Path)
    parser.add_argument("--base-wall-s", type=float, default=0.0)
    parser.add_argument("--base-label", default="baseline")
    args = parser.parse_args()

    outdir = args.outdir.resolve()
    outdir.mkdir(parents=True, exist_ok=True)

    merged_inputs: list[Path] = []
    rows: list[dict[str, str]] = []
    previous_metrics = {
        "statements": 0.0,
        "branches": 0.0,
        "conditions": 0.0,
        "expressions": 0.0,
        "toggles": 0.0,
        "fsm_states": 0.0,
    }
    cumulative_wall = args.base_wall_s

    if args.base_ucdb:
        base_ucdb = args.base_ucdb.resolve()
        merged_inputs.append(base_ucdb)
        dut_metrics, totals = build_reports(base_ucdb, outdir, "suite_step00_baseline")
        previous_metrics = dut_metrics
        current_total = totals.get("total", 0.0)
        rows.append(
            {
                "step": "0",
                "case_id": args.base_label,
                "txn_count": "0",
                "wall_s": f"{args.base_wall_s:.3f}",
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
                "delta_total_pct": "0.00",
                "delta_toggle_pct": "0.00",
                "gap_gain": "0.00",
                "delta_total_pct_per_s": "0.000",
                "merged_ucdb": str(base_ucdb),
            }
        )

    for idx, csv_arg in enumerate(args.trend_csv, start=1):
        final_row = read_final_row(Path(csv_arg))
        case_wall = float(final_row["cumulative_wall_s"])
        cumulative_wall += case_wall
        merged_inputs.append(Path(final_row["ucdb"]).resolve())
        merged_ucdb = outdir / f"suite_step{idx:02d}_{final_row['case_id']}.ucdb"
        run([str(VCOVER_BIN), "merge", str(merged_ucdb)] + [str(path) for path in merged_inputs])
        dut_metrics, totals = build_reports(merged_ucdb, outdir, f"suite_step{idx:02d}_{final_row['case_id']}")
        current_total = totals.get("total", 0.0)
        previous_total = float(rows[-1]["total_pct"]) if rows else 0.0
        delta_total = current_total - previous_total
        delta_toggle = dut_metrics.get("toggles", 0.0) - previous_metrics.get("toggles", 0.0)
        gap_gain = coverage_gap(previous_metrics) - coverage_gap(dut_metrics)
        gain_per_s = 0.0 if case_wall == 0.0 else delta_total / case_wall
        rows.append(
            {
                "step": str(idx),
                "case_id": final_row["case_id"],
                "txn_count": final_row["txn_count"],
                "wall_s": f"{case_wall:.3f}",
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
                "merged_ucdb": str(merged_ucdb),
            }
        )
        previous_metrics = dut_metrics
        print(
            f"step={idx} case={final_row['case_id']} total={current_total:.2f} delta={delta_total:.2f} "
            f"toggle_delta={delta_toggle:.2f} cum_wall={cumulative_wall:.3f}s"
        )

    csv_path = outdir / "suite_trend.csv"
    with csv_path.open("w", newline="") as fh:
        writer = csv.DictWriter(
            fh,
            fieldnames=[
                "step",
                "case_id",
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
                "merged_ucdb",
            ],
        )
        writer.writeheader()
        writer.writerows(rows)

    maybe_plot(rows, outdir / "suite_trend.png")
    print(f"csv={csv_path}")
    print(f"png={outdir / 'suite_trend.png'}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
