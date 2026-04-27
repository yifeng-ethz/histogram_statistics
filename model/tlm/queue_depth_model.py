#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import math
import random
from dataclasses import dataclass
from pathlib import Path


N_BINS = 256
KICK_WIDTH = 8
KICK_MAX = (1 << KICK_WIDTH) - 1
OVERFLOW_MAX = (1 << 16) - 1
DEFAULT_QUEUE_DEPTH = 256
REALISTIC_LINK_RATE_MHIT = 200.0
REALISTIC_STALL_CYCLES = 256

RATES = [0.10, 0.25, 0.50, 0.75, 1.00]
STALL_CYCLES = [16, 64, 128, 256, 512]
THRESHOLDS = [5.0e-2, 1.0e-2, 1.0e-3, 1.0e-6]
DEPTHS = [1, 2, 4, 8, 16, 32, 64, 96, 128, 144, 160, 192, 224, 240, 248, 256]

TRAFFIC_PROFILES = [
    ("single_bin_hotspot", 1, "iid over one bin; queue depth cannot fix kick saturation"),
    ("cluster8_local", 8, "iid over one physical 8-channel cluster"),
    ("port32_uniform", 32, "iid over one MuTRiG channel slice"),
    ("half_detector128", 128, "iid over four MuTRiG channel slices"),
    ("uniform256", 256, "iid over all histogram bins"),
]

SIM_CYCLES = 4_096
STALL_PERIOD = 1024
SWEEP_SEEDS = 4


def sat_inc(value: int) -> int:
    return min(OVERFLOW_MAX, value + 1)


@dataclass
class QueueOutputs:
    drain_valid: int
    drain_bin: int
    drain_count: int
    occupancy: int
    occupancy_max: int
    overflow_count: int


class CoalescingQueueTlm:
    """Cycle model matching rtl/coalescing_queue.vhd state update order."""

    def __init__(self, *, n_bins: int = N_BINS, depth: int = DEFAULT_QUEUE_DEPTH, kick_max: int = KICK_MAX) -> None:
        self.n_bins = n_bins
        self.depth = depth
        self.kick_max = kick_max
        self.kicks = [0] * n_bins
        self.queued = [False] * n_bins
        self.queue_mem = [0] * depth
        self.rd_ptr = 0
        self.wr_ptr = 0
        self.level = 0
        self.level_max = 0
        self.overflow_count = 0
        self.head_bin = 0
        self.head_valid = False

    def next_ptr(self, ptr: int) -> int:
        return 0 if ptr == self.depth - 1 else ptr + 1

    def step(self, hit_valid: int, hit_bin: int, drain_ready: int) -> QueueOutputs:
        old_level = self.level
        old_head_bin = self.head_bin
        old_head_valid = self.head_valid
        drain_fire = bool(drain_ready and old_head_valid)
        drain_bin = old_head_bin
        drain_count = self.kicks[old_head_bin]
        queue_room = (old_level < self.depth) or drain_fire

        level = self.level
        rd_ptr = self.rd_ptr
        wr_ptr = self.wr_ptr
        next_head_bin = self.head_bin
        next_head_valid = self.head_valid

        if (not next_head_valid) and old_level != 0:
            next_head_bin = self.queue_mem[self.rd_ptr]
            next_head_valid = True

        if drain_fire:
            rd_ptr = self.next_ptr(rd_ptr)
            level -= 1
            if old_level > 1:
                next_head_bin = self.queue_mem[rd_ptr]
                next_head_valid = True
            else:
                next_head_valid = False

        if drain_fire:
            self.kicks[old_head_bin] = 0
            self.queued[old_head_bin] = False

        if hit_valid:
            if self.queued[hit_bin]:
                if self.kicks[hit_bin] < self.kick_max:
                    self.kicks[hit_bin] += 1
                else:
                    self.overflow_count = sat_inc(self.overflow_count)
            elif queue_room:
                self.queue_mem[wr_ptr] = hit_bin
                wr_ptr = self.next_ptr(wr_ptr)
                level += 1
                self.kicks[hit_bin] = 1
                self.queued[hit_bin] = True
            else:
                self.overflow_count = sat_inc(self.overflow_count)

        self.rd_ptr = rd_ptr
        self.wr_ptr = wr_ptr
        self.level = level
        self.head_bin = next_head_bin
        self.head_valid = next_head_valid
        if level > self.level_max:
            self.level_max = level

        return QueueOutputs(
            drain_valid=1 if drain_fire else 0,
            drain_bin=drain_bin,
            drain_count=drain_count,
            occupancy=self.level,
            occupancy_max=self.level_max,
            overflow_count=self.overflow_count,
        )


def distinct_pmf_iid_bernoulli(active_bins: int, cycles: int, hit_probability: float) -> list[float]:
    """Exact P(number of distinct tags queued during a no-drain window)."""

    pmf = [0.0] * (active_bins + 1)
    pmf[0] = 1.0
    for _ in range(cycles):
        nxt = [0.0] * (active_bins + 1)
        for distinct, prob in enumerate(pmf):
            if prob == 0.0:
                continue
            nxt[distinct] += prob * (1.0 - hit_probability)
            if distinct == 0:
                nxt[1] += prob * hit_probability
            else:
                nxt[distinct] += prob * hit_probability * (distinct / active_bins)
                if distinct < active_bins:
                    nxt[distinct + 1] += prob * hit_probability * ((active_bins - distinct) / active_bins)
        pmf = nxt
    return pmf


def tail_probability(pmf: list[float], depth: int) -> float:
    if depth >= len(pmf) - 1:
        return 0.0
    return sum(pmf[depth + 1 :])


def depth_for_threshold(pmf: list[float], threshold: float) -> int:
    for depth in range(len(pmf)):
        if tail_probability(pmf, depth) <= threshold:
            return depth
    return len(pmf) - 1


def binomial_tail_greater_than(n: int, p: float, threshold_count: int) -> float:
    pmf = [0.0] * (n + 1)
    pmf[0] = 1.0
    for _ in range(n):
        nxt = [0.0] * (n + 1)
        for k, prob in enumerate(pmf):
            if prob == 0.0:
                continue
            nxt[k] += prob * (1.0 - p)
            nxt[k + 1] += prob * p
        pmf = nxt
    return sum(pmf[threshold_count + 1 :])


def write_exact_regions(out_dir: Path) -> None:
    with (out_dir / "queue_depth_exact_regions.csv").open("w", newline="", encoding="utf-8") as f:
        fieldnames = [
            "traffic",
            "active_bins",
            "stall_cycles",
            "hit_probability_per_cycle",
            "no_drop_worst_case_depth",
            "depth_for_drop_le_5pct",
            "depth_for_drop_le_1pct",
            "depth_for_drop_le_0p1pct",
            "depth_for_drop_le_1ppm",
            "configured_depth_drop_probability",
            "kick_saturation_probability_exact_when_active_bins_1",
            "definition",
        ]
        writer = csv.DictWriter(f, fieldnames=fieldnames, lineterminator="\n")
        writer.writeheader()
        for traffic, active_bins, definition in TRAFFIC_PROFILES:
            for stall in STALL_CYCLES:
                for rate in RATES:
                    pmf = distinct_pmf_iid_bernoulli(active_bins, stall, rate)
                    worst_depth = min(active_bins, stall) if rate > 0.0 else 0
                    depths = [depth_for_threshold(pmf, threshold) for threshold in THRESHOLDS]
                    kick_prob = ""
                    if active_bins == 1:
                        kick_prob = f"{binomial_tail_greater_than(stall, rate, KICK_MAX):.12g}"
                    writer.writerow(
                        {
                            "traffic": traffic,
                            "active_bins": active_bins,
                            "stall_cycles": stall,
                            "hit_probability_per_cycle": f"{rate:.2f}",
                            "no_drop_worst_case_depth": worst_depth,
                            "depth_for_drop_le_5pct": depths[0],
                            "depth_for_drop_le_1pct": depths[1],
                            "depth_for_drop_le_0p1pct": depths[2],
                            "depth_for_drop_le_1ppm": depths[3],
                            "configured_depth_drop_probability": f"{tail_probability(pmf, DEFAULT_QUEUE_DEPTH):.12g}",
                            "kick_saturation_probability_exact_when_active_bins_1": kick_prob,
                            "definition": definition,
                        }
                    )


def tag_for_profile(profile: str, seq: int, rng: random.Random) -> int:
    if profile == "single_bin_hotspot":
        return 0
    if profile == "cluster8_local":
        return rng.randrange(8)
    if profile == "port32_uniform":
        return rng.randrange(32)
    if profile == "half_detector128":
        return rng.randrange(128)
    if profile == "uniform256":
        return rng.randrange(256)
    if profile == "sequential256":
        return seq % 256
    raise ValueError(f"unknown profile {profile}")


def drain_ready_for_cycle(cycle: int, stall_cycles: int) -> int:
    return 0 if (cycle % STALL_PERIOD) < stall_cycles else 1


def simulate_profile(profile: str, depth: int, rate: float, stall_cycles: int, seed: int) -> tuple[int, int, int, int]:
    rng = random.Random(seed)
    q = CoalescingQueueTlm(depth=depth)
    offered = 0
    seq = 0
    acc = 0.0
    for cycle in range(SIM_CYCLES):
        ready = drain_ready_for_cycle(cycle, stall_cycles)
        acc += rate
        hit_valid = 0
        tag = 0
        if acc >= 1.0:
            hit_valid = 1
            acc -= 1.0
            tag = tag_for_profile(profile, seq, rng)
            seq += 1
            offered += 1
        q.step(hit_valid, tag, ready)
    return offered, q.overflow_count, q.level_max, q.level


def write_tlm_sweep(out_dir: Path) -> None:
    profiles = [p[0] for p in TRAFFIC_PROFILES] + ["sequential256"]
    with (out_dir / "queue_depth_sweep_tlm.csv").open("w", newline="", encoding="utf-8") as f:
        fieldnames = [
            "traffic",
            "queue_depth",
            "hit_probability_per_cycle",
            "stall_cycles_per_1024",
            "seeds",
            "offered_hits",
            "drops",
            "drop_probability",
            "occupancy_max_max",
            "occupancy_end_max",
        ]
        writer = csv.DictWriter(f, fieldnames=fieldnames, lineterminator="\n")
        writer.writeheader()
        for profile in profiles:
            for stall in [0, 64, 128, 256]:
                for rate in RATES:
                    for depth in DEPTHS:
                        offered_total = 0
                        drops_total = 0
                        occ_max = 0
                        occ_end = 0
                        for seed_idx in range(SWEEP_SEEDS):
                            offered, drops, level_max, level_end = simulate_profile(
                                profile,
                                depth,
                                rate,
                                stall,
                                0xC0A1E5CE ^ (seed_idx * 0x1F123BB5) ^ int(rate * 1000) ^ (stall << 4),
                            )
                            offered_total += offered
                            drops_total += drops
                            occ_max = max(occ_max, level_max)
                            occ_end = max(occ_end, level_end)
                        writer.writerow(
                            {
                                "traffic": profile,
                                "queue_depth": depth,
                                "hit_probability_per_cycle": f"{rate:.2f}",
                                "stall_cycles_per_1024": stall,
                                "seeds": SWEEP_SEEDS,
                                "offered_hits": offered_total,
                                "drops": drops_total,
                                "drop_probability": f"{(drops_total / offered_total) if offered_total else 0.0:.12g}",
                                "occupancy_max_max": occ_max,
                                "occupancy_end_max": occ_end,
                            }
                        )


def expected_iid_loss_fraction(active_bins: int, depth: int, cycles: int, hit_probability: float) -> float:
    """Exact expected lost-hit fraction for iid tags during a no-drain window."""

    if depth >= active_bins:
        return 0.0
    prob = [0.0] * (active_bins + 1)
    loss_mass = [0.0] * (active_bins + 1)
    prob[0] = 1.0
    for _ in range(cycles):
        next_prob = [0.0] * (active_bins + 1)
        next_loss = [0.0] * (active_bins + 1)
        for distinct, p_state in enumerate(prob):
            if p_state == 0.0:
                continue
            l_state = loss_mass[distinct]

            # No offered hit this cycle.
            stay_p = p_state * (1.0 - hit_probability)
            next_prob[distinct] += stay_p
            next_loss[distinct] += l_state * (1.0 - hit_probability)

            if hit_probability <= 0.0:
                continue

            existing_p = distinct / active_bins if active_bins else 0.0
            new_p = 1.0 - existing_p

            # Hit to an already queued tag: accepted.
            next_prob[distinct] += p_state * hit_probability * existing_p
            next_loss[distinct] += l_state * hit_probability * existing_p

            # Hit to a new tag: allocate if depth remains, otherwise drop.
            if distinct < depth:
                next_prob[distinct + 1] += p_state * hit_probability * new_p
                next_loss[distinct + 1] += l_state * hit_probability * new_p
            else:
                next_prob[distinct] += p_state * hit_probability * new_p
                next_loss[distinct] += (l_state + p_state) * hit_probability * new_p
        prob = next_prob
        loss_mass = next_loss

    offered = cycles * hit_probability
    if offered == 0.0:
        return 0.0
    return sum(loss_mass) / offered


def write_loss_vs_depth_8mutrig(out_dir: Path) -> None:
    """Write the requested 1D loss-vs-queue-depth view at the 8-MuTRiG cap."""

    with (out_dir / "queue_loss_vs_depth_8mutrig.csv").open("w", newline="", encoding="utf-8") as f:
        fieldnames = [
            "traffic",
            "legend",
            "queue_depth",
            "loss_rate",
            "link_rate_mhit_s",
            "stall_cycles",
            "active_channels",
            "definition",
        ]
        writer = csv.DictWriter(f, fieldnames=fieldnames, lineterminator="\n")
        writer.writeheader()
        for depth in range(1, N_BINS + 1):
            rows = [
                (
                    "iid_256",
                    "iid 256-channel",
                    expected_iid_loss_fraction(N_BINS, depth, REALISTIC_STALL_CYCLES, 1.0),
                    N_BINS,
                    "exact expected lost-hit fraction for iid dark-count tags over all 256 channels",
                ),
                (
                    "physical_cluster8",
                    "physical 8-channel hit",
                    max(0.0, (8.0 - float(depth)) / 8.0),
                    8,
                    "localized physical hit crossing 8 channels; repeated cluster during the stall window",
                ),
                (
                    "injection_all256",
                    "injection all-channel",
                    max(0.0, (float(N_BINS) - float(depth)) / float(N_BINS)),
                    N_BINS,
                    "deterministic all-channel injection/superburst over all 256 channels",
                ),
            ]
            for traffic, legend, loss_rate, active_channels, definition in rows:
                writer.writerow(
                    {
                        "traffic": traffic,
                        "legend": legend,
                        "queue_depth": depth,
                        "loss_rate": f"{loss_rate:.12g}",
                        "link_rate_mhit_s": f"{REALISTIC_LINK_RATE_MHIT:.1f}",
                        "stall_cycles": REALISTIC_STALL_CYCLES,
                        "active_channels": active_channels,
                        "definition": definition,
                    }
                )


def write_depth_quantiles_8mutrig(out_dir: Path) -> None:
    """Write analytical queue-depth quantiles for the presentation report."""

    with (out_dir / "queue_depth_quantiles_8mutrig.csv").open("w", newline="", encoding="utf-8") as f:
        fieldnames = [
            "series",
            "legend",
            "active_channels",
            "required_depth",
            "stall_cycles",
            "hit_probability_per_cycle",
            "definition",
        ]
        writer = csv.DictWriter(f, fieldnames=fieldnames, lineterminator="\n")
        writer.writeheader()
        for traffic, active_bins, traffic_definition in TRAFFIC_PROFILES:
            pmf = distinct_pmf_iid_bernoulli(active_bins, REALISTIC_STALL_CYCLES, 1.0)
            rows = [
                (
                    "worst_case",
                    "no-drop worst case",
                    min(active_bins, REALISTIC_STALL_CYCLES),
                    "all active channels can appear during the stall",
                ),
                (
                    "drop_le_5pct",
                    "drop prob <= 5%",
                    depth_for_threshold(pmf, 5.0e-2),
                    "minimum depth where queue-full probability is at most 5%",
                ),
                (
                    "drop_le_1pct",
                    "drop prob <= 1%",
                    depth_for_threshold(pmf, 1.0e-2),
                    "minimum depth where queue-full probability is at most 1%",
                ),
                (
                    "drop_le_1ppm",
                    "drop prob <= 1 ppm",
                    depth_for_threshold(pmf, 1.0e-6),
                    "minimum depth where queue-full probability is at most 1 ppm",
                ),
            ]
            for series, legend, depth, definition in rows:
                writer.writerow(
                    {
                        "series": series,
                        "legend": legend,
                        "active_channels": active_bins,
                        "required_depth": depth,
                        "stall_cycles": REALISTIC_STALL_CYCLES,
                        "hit_probability_per_cycle": "1.00",
                        "definition": f"{definition}; traffic={traffic}: {traffic_definition}",
                    }
                )


def validation_rows() -> list[tuple[int, int, int, int]]:
    rows: list[tuple[int, int, int, int]] = []
    cycle = 0

    def add(hit_valid: int, hit_bin: int, ready: int) -> None:
        nonlocal cycle
        rows.append((cycle, hit_valid, hit_bin, ready))
        cycle += 1

    for _ in range(20):
        add(1, 10, 1)
    for _ in range(50):
        add(0, 0, 1)
    for idx in range(200):
        add(1, idx % N_BINS, 0)
    for _ in range(260):
        add(0, 0, 1)
    for _ in range(300):
        add(1, 7, 0)
    for _ in range(360):
        add(0, 0, 1)
    return rows


def write_validation_trace(out_dir: Path, depth: int) -> None:
    rows = validation_rows()
    with (out_dir / "queue_trace_stimulus.csv").open("w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f, lineterminator="\n")
        writer.writerow(["cycle", "hit_valid", "hit_bin", "drain_ready"])
        writer.writerows(rows)

    q = CoalescingQueueTlm(depth=depth)
    with (out_dir / "queue_trace_expected_tlm.csv").open("w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f, lineterminator="\n")
        writer.writerow(
            [
                "cycle",
                "hit_valid",
                "hit_bin",
                "drain_ready",
                "drain_valid",
                "drain_bin",
                "drain_count",
                "occupancy",
                "occupancy_max",
                "overflow_count",
            ]
        )
        for cycle, hit_valid, hit_bin, ready in rows:
            out = q.step(hit_valid, hit_bin, ready)
            writer.writerow(
                [
                    cycle,
                    hit_valid,
                    hit_bin,
                    ready,
                    out.drain_valid,
                    out.drain_bin,
                    out.drain_count,
                    out.occupancy,
                    out.occupancy_max,
                    out.overflow_count,
                ]
            )


def compare_rtl(expected_path: Path, observed_path: Path, compare_out: Path) -> int:
    with expected_path.open(newline="", encoding="utf-8") as f:
        expected = list(csv.DictReader(f))
    with observed_path.open(newline="", encoding="utf-8") as f:
        observed = list(csv.DictReader(f))

    fields = [
        "cycle",
        "hit_valid",
        "hit_bin",
        "drain_ready",
        "drain_valid",
        "drain_bin",
        "drain_count",
        "occupancy",
        "occupancy_max",
        "overflow_count",
    ]
    mismatches = 0
    with compare_out.open("w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f, lineterminator="\n")
        writer.writerow(["cycle", "field", "tlm", "rtl"])
        if len(expected) != len(observed):
            writer.writerow(["length", "row_count", len(expected), len(observed)])
            mismatches += 1
        for exp, obs in zip(expected, observed):
            for field in fields:
                if str(exp[field]) != str(obs[field]):
                    writer.writerow([exp["cycle"], field, exp[field], obs[field]])
                    mismatches += 1
    if mismatches:
        raise SystemExit(f"TLM/RTL mismatch count: {mismatches}")
    print(f"TLM/RTL comparison PASS: {len(expected)} cycles, 0 mismatches")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--out-dir", type=Path, default=Path("histogram_statistics/model/artifacts"))
    parser.add_argument("--queue-depth", type=int, default=DEFAULT_QUEUE_DEPTH)
    parser.add_argument("--compare-rtl", nargs=2, type=Path, metavar=("EXPECTED", "OBSERVED"))
    parser.add_argument("--compare-out", type=Path, default=Path("queue_trace_compare.csv"))
    args = parser.parse_args()

    if args.compare_rtl:
        return compare_rtl(args.compare_rtl[0], args.compare_rtl[1], args.compare_out)

    args.out_dir.mkdir(parents=True, exist_ok=True)
    write_exact_regions(args.out_dir)
    write_tlm_sweep(args.out_dir)
    write_loss_vs_depth_8mutrig(args.out_dir)
    write_depth_quantiles_8mutrig(args.out_dir)
    write_validation_trace(args.out_dir, args.queue_depth)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
