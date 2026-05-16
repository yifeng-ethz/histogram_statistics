#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import math
import random
from collections import deque
from dataclasses import dataclass
from pathlib import Path


N_BINS = 256
CLOCK_HZ = 125_000_000.0
PUSH_HITS_PER_CYCLE = 2
DRAIN_CELLS_PER_CYCLE = 0.5
KICK_MAX = 255
DEFAULT_MIN_PULSE_RATE_HZ = 10_000.0
DEFAULT_MAX_PULSE_RATE_HZ = 950_000.0
DEFAULT_RATE_POINTS = 96
DEFAULT_WARMUP_PULSES = 32
DEFAULT_SAMPLE_PULSES = 128
DEFAULT_BUNCHED_WARMUP_CYCLES = 20_000
DEFAULT_BUNCHED_SAMPLE_CYCLES = 200_000
DEFAULT_IID_WARMUP_CYCLES = 20_000
DEFAULT_IID_SAMPLE_CYCLES = 200_000
DEFAULT_IID_SEED = 0xC04E_1CE5
REFERENCE_PULSE_RATE_HZ = 100_000.0


@dataclass
class SampleStats:
    min_cells: int
    p50_cells: int
    max_cells: int
    mean_cells: float


class PerBinCounterCoalescer:
    """Fixed per-bin pending cells, each with an 8-bit saturating kick counter."""

    name = "per_bin_counter"
    legend = "per-bin counters"

    def __init__(self, n_bins: int, kick_max: int = KICK_MAX) -> None:
        self.n_bins = n_bins
        self.kick_max = kick_max
        self.kicks = [0] * n_bins
        self.kick_hist = [0] * (kick_max + 1)
        self.live_kick_values: set[int] = set()
        self.scan_ptr = 0
        self.nonzero_cells = 0
        self.pending_kicks = 0
        self.emitted_transactions = 0
        self.emitted_kicks = 0
        self.overflow_events = 0
        self.saturation_events = 0

    def _hist_inc(self, value: int) -> None:
        self.kick_hist[value] += 1
        self.live_kick_values.add(value)

    def _hist_dec(self, value: int) -> None:
        self.kick_hist[value] -= 1
        if self.kick_hist[value] == 0:
            self.live_kick_values.discard(value)

    def push(self, tag: int) -> None:
        old = self.kicks[tag]
        if old >= self.kick_max:
            self.saturation_events += 1
            return
        if old == 0:
            self.nonzero_cells += 1
            self._hist_inc(1)
        else:
            self._hist_dec(old)
            self._hist_inc(old + 1)
        self.kicks[tag] = old + 1
        self.pending_kicks += 1

    def service(self) -> None:
        if self.nonzero_cells == 0:
            return
        for offset in range(self.n_bins):
            idx = (self.scan_ptr + offset) % self.n_bins
            count = self.kicks[idx]
            if count == 0:
                continue
            self._hist_dec(count)
            self.kicks[idx] = 0
            self.scan_ptr = (idx + 1) % self.n_bins
            self.nonzero_cells -= 1
            self.pending_kicks -= count
            self.emitted_transactions += 1
            self.emitted_kicks += count
            return

    def occupancy(self) -> int:
        return self.nonzero_cells

    def capacity(self) -> int:
        return self.n_bins

    def active_kick_items(self) -> list[tuple[int, int]]:
        return [(value, self.kick_hist[value]) for value in sorted(self.live_kick_values)]


@dataclass
class CbCell:
    tag: int
    count: int


class CircularBufferCoalescer:
    """Tag-addressable circular buffer with one queued cell per live tag."""

    name = "tag_addressable_cb"
    legend = "tag-addressable CB"

    def __init__(self, n_bins: int, depth: int, kick_max: int = KICK_MAX) -> None:
        self.n_bins = n_bins
        self.depth = depth
        self.kick_max = kick_max
        self.queue: deque[CbCell] = deque()
        self.by_tag: dict[int, CbCell] = {}
        self.kick_hist = [0] * (kick_max + 1)
        self.live_kick_values: set[int] = set()
        self.pending_kicks = 0
        self.emitted_transactions = 0
        self.emitted_kicks = 0
        self.overflow_events = 0
        self.saturation_events = 0

    def _hist_inc(self, value: int) -> None:
        self.kick_hist[value] += 1
        self.live_kick_values.add(value)

    def _hist_dec(self, value: int) -> None:
        self.kick_hist[value] -= 1
        if self.kick_hist[value] == 0:
            self.live_kick_values.discard(value)

    def push(self, tag: int) -> None:
        cell = self.by_tag.get(tag)
        if cell is not None:
            if cell.count >= self.kick_max:
                self.saturation_events += 1
                return
            self._hist_dec(cell.count)
            cell.count += 1
            self._hist_inc(cell.count)
            self.pending_kicks += 1
            return

        if len(self.queue) >= self.depth:
            self.overflow_events += 1
            return

        cell = CbCell(tag=tag, count=1)
        self.queue.append(cell)
        self.by_tag[tag] = cell
        self._hist_inc(1)
        self.pending_kicks += 1

    def service(self) -> None:
        if not self.queue:
            return
        cell = self.queue.popleft()
        del self.by_tag[cell.tag]
        self._hist_dec(cell.count)
        self.pending_kicks -= cell.count
        self.emitted_transactions += 1
        self.emitted_kicks += cell.count

    def occupancy(self) -> int:
        return len(self.queue)

    def capacity(self) -> int:
        return self.depth

    def active_kick_items(self) -> list[tuple[int, int]]:
        return [(value, self.kick_hist[value]) for value in sorted(self.live_kick_values)]


def percentile_from_hist(hist: list[int], pct: float) -> int:
    total = sum(hist)
    if total <= 0:
        return 0
    target = max(1, math.ceil(total * pct / 100.0))
    accum = 0
    for value, count in enumerate(hist):
        accum += count
        if accum >= target:
            return value
    return len(hist) - 1


def stats_from_hist(hist: list[int]) -> SampleStats:
    total = sum(hist)
    if total <= 0:
        return SampleStats(0, 0, 0, 0.0)

    min_cells = next(idx for idx, count in enumerate(hist) if count)
    max_cells = len(hist) - 1 - next(idx for idx, count in enumerate(reversed(hist)) if count)
    weighted_sum = sum(idx * count for idx, count in enumerate(hist))
    return SampleStats(
        min_cells=min_cells,
        p50_cells=percentile_from_hist(hist, 50.0),
        max_cells=max_cells,
        mean_cells=weighted_sum / total,
    )


def stats_from_samples(samples: list[int]) -> SampleStats:
    if not samples:
        return SampleStats(0, 0, 0, 0.0)
    ordered = sorted(samples)
    return SampleStats(
        min_cells=ordered[0],
        p50_cells=ordered[len(ordered) // 2],
        max_cells=ordered[-1],
        mean_cells=sum(samples) / len(samples),
    )


def default_rate_list(
    min_rate_hz: float,
    max_rate_hz: float,
    points: int,
    n_bins: int,
    clock_hz: float,
    drain_cells_per_cycle: float,
) -> list[float]:
    if points < 2:
        rates = [min_rate_hz]
    else:
        step = (max_rate_hz - min_rate_hz) / float(points - 1)
        rates = [min_rate_hz + idx * step for idx in range(points)]

    service_limited_pulse_rate = clock_hz * drain_cells_per_cycle / float(n_bins)
    rates.extend([REFERENCE_PULSE_RATE_HZ, service_limited_pulse_rate])
    return sorted({round(rate, 6) for rate in rates if min_rate_hz <= rate <= max_rate_hz})


def build_hit_schedule(
    *,
    n_bins: int,
    push_hits_per_cycle: int,
    period_cycles: float,
    total_pulses: int,
) -> tuple[dict[int, list[int]], list[int], int]:
    source_burst_cycles = math.ceil(n_bins / push_hits_per_cycle)
    starts = [int(round(pulse_idx * period_cycles)) for pulse_idx in range(total_pulses + 1)]

    min_gap = min(starts[idx + 1] - starts[idx] for idx in range(total_pulses))
    if min_gap < source_burst_cycles:
        raise ValueError(
            f"pulse period is too short for the source serializer: "
            f"min_gap={min_gap} cycles, burst={source_burst_cycles} cycles"
        )

    schedule: dict[int, list[int]] = {}
    for pulse_idx in range(total_pulses):
        start = starts[pulse_idx]
        for burst_cycle in range(source_burst_cycles):
            cycle = start + burst_cycle
            tags = schedule.setdefault(cycle, [])
            for lane in range(push_hits_per_cycle):
                tag = burst_cycle * push_hits_per_cycle + lane
                if tag < n_bins:
                    tags.append(tag)
    return schedule, starts, source_burst_cycles


def poisson_sample(lam: float, rng: random.Random) -> int:
    limit = math.exp(-lam)
    k = 0
    product = 1.0
    while product > limit:
        k += 1
        product *= rng.random()
    return k - 1


def update_sample_histograms(
    *,
    models: list[PerBinCounterCoalescer | CircularBufferCoalescer],
    occupancy_hist: dict[str, list[int]],
    kick_hist: dict[str, list[int]],
    pending_kick_sum: dict[str, int],
    pending_kick_max: dict[str, int],
) -> None:
    for model in models:
        occ = model.occupancy()
        occupancy_hist[model.name][occ] += 1
        pending_kick_sum[model.name] += model.pending_kicks
        if model.pending_kicks > pending_kick_max[model.name]:
            pending_kick_max[model.name] = model.pending_kicks
        for kick_count, count in model.active_kick_items():
            kick_hist[model.name][kick_count] += count


def build_coalescer_rows(
    *,
    models: list[PerBinCounterCoalescer | CircularBufferCoalescer],
    baseline: dict[str, tuple[int, int, int, int]],
    occupancy_hist: dict[str, list[int]],
    kick_hist: dict[str, list[int]],
    pending_kick_sum: dict[str, int],
    pending_kick_max: dict[str, int],
    offered_hits: int,
    sample_cycles: int,
    pulse_rate_hz: float,
    n_bins: int,
    clock_hz: float,
    push_hits_per_cycle: int,
    drain_cells_per_cycle: float,
    source_burst_cycles: int,
    warmup_pulses: int,
    sample_pulses: int,
    traffic: str,
) -> list[dict[str, object]]:
    rows: list[dict[str, object]] = []
    for model in models:
        stats = stats_from_hist(occupancy_hist[model.name])
        kick_stats = stats_from_hist(kick_hist[model.name])
        base_transactions, base_kicks, base_overflow, base_saturation = baseline[model.name]
        rows.append(
            {
                "traffic": traffic,
                "model": model.name,
                "legend": model.legend,
                "pulse_rate_hz": pulse_rate_hz,
                "push_rate_per_s": pulse_rate_hz * n_bins,
                "push_rate_mps": pulse_rate_hz * n_bins / 1.0e6,
                "pulse_period_cycles": clock_hz / pulse_rate_hz,
                "source_burst_cycles": source_burst_cycles,
                "sample_cycles": sample_cycles,
                "min_cells": stats.min_cells,
                "p50_cells": stats.p50_cells,
                "max_cells": stats.max_cells,
                "mean_cells": stats.mean_cells,
                "max_pending_kicks": pending_kick_max[model.name],
                "mean_pending_kicks": pending_kick_sum[model.name] / sample_cycles if sample_cycles else 0.0,
                "offered_hits": offered_hits,
                "emitted_transactions": model.emitted_transactions - base_transactions,
                "emitted_kicks": model.emitted_kicks - base_kicks,
                "overflow_events": model.overflow_events - base_overflow,
                "saturation_events": model.saturation_events - base_saturation,
                "queue_capacity": model.capacity(),
                "n_bins": n_bins,
                "clock_hz": clock_hz,
                "push_hits_per_cycle": push_hits_per_cycle,
                "drain_cells_per_cycle": drain_cells_per_cycle,
                "warmup_pulses": warmup_pulses,
                "sample_pulses": sample_pulses,
                "min_kick_count": kick_stats.min_cells,
                "p50_kick_count": kick_stats.p50_cells,
                "max_kick_count": kick_stats.max_cells,
                "mean_kick_count": kick_stats.mean_cells,
            }
        )
    return rows


def simulate_rate(
    *,
    pulse_rate_hz: float,
    n_bins: int,
    cb_depth: int,
    clock_hz: float,
    push_hits_per_cycle: int,
    drain_cells_per_cycle: float,
    warmup_pulses: int,
    sample_pulses: int,
    warmup_cycles: int = DEFAULT_BUNCHED_WARMUP_CYCLES,
    sample_cycles_fixed: int = DEFAULT_BUNCHED_SAMPLE_CYCLES,
) -> list[dict[str, object]]:
    period_cycles = clock_hz / pulse_rate_hz
    total_cycles = warmup_cycles + sample_cycles_fixed
    total_pulses = int(math.ceil((total_cycles + n_bins / push_hits_per_cycle) / period_cycles)) + 1
    schedule, starts, source_burst_cycles = build_hit_schedule(
        n_bins=n_bins,
        push_hits_per_cycle=push_hits_per_cycle,
        period_cycles=period_cycles,
        total_pulses=total_pulses,
    )
    _ = starts
    sample_start = warmup_cycles
    sample_end = total_cycles
    sample_cycles = sample_end - sample_start

    models = [
        PerBinCounterCoalescer(n_bins=n_bins),
        CircularBufferCoalescer(n_bins=n_bins, depth=cb_depth),
    ]
    max_capacity = max(model.capacity() for model in models)
    occupancy_hist = {model.name: [0] * (max_capacity + 1) for model in models}
    kick_hist = {model.name: [0] * (KICK_MAX + 1) for model in models}
    pending_kick_sum = {model.name: 0 for model in models}
    pending_kick_max = {model.name: 0 for model in models}
    baseline = {}
    offered_hits = 0
    drain_credit = 0.0

    for cycle in range(sample_end):
        if cycle == sample_start:
            baseline = {
                model.name: (
                    model.emitted_transactions,
                    model.emitted_kicks,
                    model.overflow_events,
                    model.saturation_events,
                )
                for model in models
            }

        hits = schedule.get(cycle, ())
        if cycle >= sample_start:
            offered_hits += len(hits)
        for tag in hits:
            for model in models:
                model.push(tag)

        drain_credit += drain_cells_per_cycle
        service_ops = int(drain_credit)
        drain_credit -= service_ops
        for _ in range(service_ops):
            for model in models:
                model.service()

        if cycle >= sample_start:
            update_sample_histograms(
                models=models,
                occupancy_hist=occupancy_hist,
                kick_hist=kick_hist,
                pending_kick_sum=pending_kick_sum,
                pending_kick_max=pending_kick_max,
            )

    return build_coalescer_rows(
        models=models,
        baseline=baseline,
        occupancy_hist=occupancy_hist,
        kick_hist=kick_hist,
        pending_kick_sum=pending_kick_sum,
        pending_kick_max=pending_kick_max,
        offered_hits=offered_hits,
        sample_cycles=sample_cycles,
        pulse_rate_hz=pulse_rate_hz,
        n_bins=n_bins,
        clock_hz=clock_hz,
        push_hits_per_cycle=push_hits_per_cycle,
        drain_cells_per_cycle=drain_cells_per_cycle,
        source_burst_cycles=source_burst_cycles,
        warmup_pulses=warmup_pulses,
        sample_pulses=sample_pulses,
        traffic="bunched_all_bins",
    )


def simulate_iid_rate(
    *,
    pulse_rate_hz: float,
    n_bins: int,
    cb_depth: int,
    clock_hz: float,
    push_hits_per_cycle: int,
    drain_cells_per_cycle: float,
    warmup_pulses: int,
    sample_pulses: int,
    seed: int,
    warmup_cycles: int = DEFAULT_IID_WARMUP_CYCLES,
    sample_cycles_fixed: int = DEFAULT_IID_SAMPLE_CYCLES,
) -> list[dict[str, object]]:
    aggregate_rate_hz = pulse_rate_hz * n_bins
    lam_per_cycle = aggregate_rate_hz / clock_hz
    total_cycles = warmup_cycles + sample_cycles_fixed
    sample_start = warmup_cycles
    sample_cycles = total_cycles - sample_start
    rng = random.Random(seed)

    models = [
        PerBinCounterCoalescer(n_bins=n_bins),
        CircularBufferCoalescer(n_bins=n_bins, depth=cb_depth),
    ]
    max_capacity = max(model.capacity() for model in models)
    occupancy_hist = {model.name: [0] * (max_capacity + 1) for model in models}
    kick_hist = {model.name: [0] * (KICK_MAX + 1) for model in models}
    pending_kick_sum = {model.name: 0 for model in models}
    pending_kick_max = {model.name: 0 for model in models}
    baseline = {}
    offered_hits = 0
    source_arrivals = 0
    drain_credit = 0.0
    source_queue: deque[int] = deque()
    source_fifo_samples: list[int] = []

    for cycle in range(total_cycles):
        if cycle == sample_start:
            baseline = {
                model.name: (
                    model.emitted_transactions,
                    model.emitted_kicks,
                    model.overflow_events,
                    model.saturation_events,
                )
                for model in models
            }

        arrivals = poisson_sample(lam_per_cycle, rng)
        if cycle >= sample_start:
            source_arrivals += arrivals
        for _ in range(arrivals):
            source_queue.append(rng.randrange(n_bins))

        pushes = min(push_hits_per_cycle, len(source_queue))
        if cycle >= sample_start:
            offered_hits += pushes
        for _ in range(pushes):
            tag = source_queue.popleft()
            for model in models:
                model.push(tag)

        drain_credit += drain_cells_per_cycle
        service_ops = int(drain_credit)
        drain_credit -= service_ops
        for _ in range(service_ops):
            for model in models:
                model.service()

        if cycle >= sample_start:
            source_fifo_samples.append(len(source_queue))
            update_sample_histograms(
                models=models,
                occupancy_hist=occupancy_hist,
                kick_hist=kick_hist,
                pending_kick_sum=pending_kick_sum,
                pending_kick_max=pending_kick_max,
            )

    rows = build_coalescer_rows(
        models=models,
        baseline=baseline,
        occupancy_hist=occupancy_hist,
        kick_hist=kick_hist,
        pending_kick_sum=pending_kick_sum,
        pending_kick_max=pending_kick_max,
        offered_hits=offered_hits,
        sample_cycles=sample_cycles,
        pulse_rate_hz=pulse_rate_hz,
        n_bins=n_bins,
        clock_hz=clock_hz,
        push_hits_per_cycle=push_hits_per_cycle,
        drain_cells_per_cycle=drain_cells_per_cycle,
        source_burst_cycles=0,
        warmup_pulses=warmup_pulses,
        sample_pulses=sample_pulses,
        traffic="iid_poisson_256_sources",
    )
    source_fifo_stats = stats_from_samples(source_fifo_samples)
    for row in rows:
        row.update(
            {
                "source_arrivals": source_arrivals,
                "source_fifo_min_cells": source_fifo_stats.min_cells,
                "source_fifo_p50_cells": source_fifo_stats.p50_cells,
                "source_fifo_max_cells": source_fifo_stats.max_cells,
                "source_fifo_mean_cells": source_fifo_stats.mean_cells,
            }
        )
    return rows


def simulate_infinite_rate(
    *,
    pulse_rate_hz: float,
    n_bins: int,
    clock_hz: float,
    push_hits_per_cycle: int,
    drain_cells_per_cycle: float,
    warmup_pulses: int,
    sample_pulses: int,
) -> dict[str, object]:
    """Unbounded-capacity D/D/1 queue for update cells.

    This is the capacity-infinite reference: every offered bin update becomes
    one queued cell, and the update engine drains cells at the configured mean
    hardware service rate. It intentionally does not tag-coalesce cells.
    """

    period_cycles = clock_hz / pulse_rate_hz
    total_pulses = warmup_pulses + sample_pulses
    schedule, starts, source_burst_cycles = build_hit_schedule(
        n_bins=n_bins,
        push_hits_per_cycle=push_hits_per_cycle,
        period_cycles=period_cycles,
        total_pulses=total_pulses,
    )
    sample_start = starts[warmup_pulses]
    sample_end = starts[total_pulses]
    sample_cycles = sample_end - sample_start

    queue_birth_cycles: deque[int] = deque()
    occupancy_samples: list[int] = []
    latencies: list[int] = []
    offered_hits = 0
    emitted_transactions = 0
    drain_credit = 0.0

    for cycle in range(sample_end):
        hits = schedule.get(cycle, ())
        if cycle >= sample_start:
            offered_hits += len(hits)
        for _tag in hits:
            queue_birth_cycles.append(cycle)

        drain_credit += drain_cells_per_cycle
        service_ops = int(drain_credit)
        drain_credit -= service_ops
        for _ in range(service_ops):
            if queue_birth_cycles:
                birth_cycle = queue_birth_cycles.popleft()
                emitted_transactions += 1
                if cycle >= sample_start:
                    latencies.append(cycle - birth_cycle)

        if cycle >= sample_start:
            occupancy_samples.append(len(queue_birth_cycles))

    stats = stats_from_samples(occupancy_samples)
    max_latency_cycles = max(latencies) if latencies else 0
    p50_latency_cycles = sorted(latencies)[len(latencies) // 2] if latencies else 0
    return {
        "model": "infinite_update_queue",
        "legend": "D/D/1 infinite K",
        "pulse_rate_hz": pulse_rate_hz,
        "push_rate_per_s": pulse_rate_hz * n_bins,
        "push_rate_mps": pulse_rate_hz * n_bins / 1.0e6,
        "pulse_period_cycles": period_cycles,
        "source_burst_cycles": source_burst_cycles,
        "sample_cycles": sample_cycles,
        "min_cells": stats.min_cells,
        "p50_cells": stats.p50_cells,
        "max_cells": stats.max_cells,
        "mean_cells": stats.mean_cells,
        "offered_hits": offered_hits,
        "emitted_transactions": emitted_transactions,
        "max_latency_cycles": max_latency_cycles,
        "max_latency_us": max_latency_cycles / (clock_hz / 1.0e6),
        "p50_latency_cycles": p50_latency_cycles,
        "p50_latency_us": p50_latency_cycles / (clock_hz / 1.0e6),
        "n_bins": n_bins,
        "clock_hz": clock_hz,
        "push_hits_per_cycle": push_hits_per_cycle,
        "drain_cells_per_cycle": drain_cells_per_cycle,
        "warmup_pulses": warmup_pulses,
        "sample_pulses": sample_pulses,
    }


COALESCER_FIELDNAMES = [
    "traffic",
    "model",
    "legend",
    "pulse_rate_hz",
    "push_rate_per_s",
    "push_rate_mps",
    "pulse_period_cycles",
    "source_burst_cycles",
    "sample_cycles",
    "min_cells",
    "p50_cells",
    "max_cells",
    "mean_cells",
    "max_pending_kicks",
    "mean_pending_kicks",
    "offered_hits",
    "emitted_transactions",
    "emitted_kicks",
    "overflow_events",
    "saturation_events",
    "queue_capacity",
    "n_bins",
    "clock_hz",
    "push_hits_per_cycle",
    "drain_cells_per_cycle",
    "warmup_pulses",
    "sample_pulses",
    "min_kick_count",
    "p50_kick_count",
    "max_kick_count",
    "mean_kick_count",
    "source_arrivals",
    "source_fifo_min_cells",
    "source_fifo_p50_cells",
    "source_fifo_max_cells",
    "source_fifo_mean_cells",
]


def sweep_rates(args: argparse.Namespace) -> list[float]:
    rates = default_rate_list(
        args.min_pulse_rate_hz,
        args.max_pulse_rate_hz,
        args.rate_points,
        args.n_bins,
        args.clock_hz,
        args.drain_cells_per_cycle,
    )
    for include_rate in args.include_pulse_rate_hz:
        if args.min_pulse_rate_hz <= include_rate <= args.max_pulse_rate_hz:
            rates.append(include_rate)
    return sorted({round(rate, 6) for rate in rates})


def write_bunched_sweep(args: argparse.Namespace) -> Path:
    out_dir = args.out_dir
    out_dir.mkdir(parents=True, exist_ok=True)
    out_path = out_dir / "bunched_coalescing_rate_sweep.csv"
    max_source_pulse_rate = args.clock_hz * args.push_hits_per_cycle / float(args.n_bins)
    if args.max_pulse_rate_hz > max_source_pulse_rate:
        raise SystemExit(
            f"max pulse rate {args.max_pulse_rate_hz:g} Hz exceeds serialized source limit "
            f"{max_source_pulse_rate:g} Hz for {args.n_bins} bins at {args.push_hits_per_cycle} hits/cycle"
        )

    rates = sweep_rates(args)

    with out_path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=COALESCER_FIELDNAMES, lineterminator="\n")
        writer.writeheader()
        for pulse_rate_hz in rates:
            rows = simulate_rate(
                pulse_rate_hz=pulse_rate_hz,
                n_bins=args.n_bins,
                cb_depth=args.cb_depth,
                clock_hz=args.clock_hz,
                push_hits_per_cycle=args.push_hits_per_cycle,
                drain_cells_per_cycle=args.drain_cells_per_cycle,
                warmup_pulses=args.warmup_pulses,
                sample_pulses=args.sample_pulses,
                warmup_cycles=args.bunched_warmup_cycles,
                sample_cycles_fixed=args.bunched_sample_cycles,
            )
            for row in rows:
                writer.writerow(
                    {
                        key: f"{value:.12g}" if isinstance(value, float) else value
                        for key, value in row.items()
                    }
                )
    return out_path


def write_iid_sweep(args: argparse.Namespace) -> Path:
    out_dir = args.out_dir
    out_dir.mkdir(parents=True, exist_ok=True)
    out_path = out_dir / "iid_poisson_coalescing_rate_sweep.csv"
    rates = sweep_rates(args)

    with out_path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=COALESCER_FIELDNAMES, lineterminator="\n")
        writer.writeheader()
        for rate_idx, pulse_rate_hz in enumerate(rates):
            rows = simulate_iid_rate(
                pulse_rate_hz=pulse_rate_hz,
                n_bins=args.n_bins,
                cb_depth=args.cb_depth,
                clock_hz=args.clock_hz,
                push_hits_per_cycle=args.push_hits_per_cycle,
                drain_cells_per_cycle=args.drain_cells_per_cycle,
                warmup_pulses=args.warmup_pulses,
                sample_pulses=args.sample_pulses,
                seed=args.iid_seed ^ (rate_idx * 0x9E3779B1),
                warmup_cycles=args.iid_warmup_cycles,
                sample_cycles_fixed=args.iid_sample_cycles,
            )
            for row in rows:
                writer.writerow(
                    {
                        key: f"{value:.12g}" if isinstance(value, float) else value
                        for key, value in row.items()
                    }
                )
    return out_path


def write_infinite_sweep(args: argparse.Namespace) -> Path:
    out_dir = args.out_dir
    out_dir.mkdir(parents=True, exist_ok=True)
    out_path = out_dir / "bunched_infinite_queue_rate_sweep.csv"

    rates = sweep_rates(args)

    fieldnames = [
        "model",
        "legend",
        "pulse_rate_hz",
        "push_rate_per_s",
        "push_rate_mps",
        "pulse_period_cycles",
        "source_burst_cycles",
        "sample_cycles",
        "min_cells",
        "p50_cells",
        "max_cells",
        "mean_cells",
        "offered_hits",
        "emitted_transactions",
        "max_latency_cycles",
        "max_latency_us",
        "p50_latency_cycles",
        "p50_latency_us",
        "n_bins",
        "clock_hz",
        "push_hits_per_cycle",
        "drain_cells_per_cycle",
        "warmup_pulses",
        "sample_pulses",
    ]

    with out_path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames, lineterminator="\n")
        writer.writeheader()
        for pulse_rate_hz in rates:
            row = simulate_infinite_rate(
                pulse_rate_hz=pulse_rate_hz,
                n_bins=args.n_bins,
                clock_hz=args.clock_hz,
                push_hits_per_cycle=args.push_hits_per_cycle,
                drain_cells_per_cycle=args.drain_cells_per_cycle,
                warmup_pulses=args.warmup_pulses,
                sample_pulses=args.sample_pulses,
            )
            writer.writerow(
                {
                    key: f"{value:.12g}" if isinstance(value, float) else value
                    for key, value in row.items()
                }
            )
    return out_path


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--out-dir", type=Path, default=Path("histogram_statistics/model/artifacts"))
    parser.add_argument("--n-bins", type=int, default=N_BINS)
    parser.add_argument("--cb-depth", type=int, default=N_BINS)
    parser.add_argument("--clock-hz", type=float, default=CLOCK_HZ)
    parser.add_argument("--push-hits-per-cycle", type=int, default=PUSH_HITS_PER_CYCLE)
    parser.add_argument("--drain-cells-per-cycle", type=float, default=DRAIN_CELLS_PER_CYCLE)
    parser.add_argument("--min-pulse-rate-hz", type=float, default=DEFAULT_MIN_PULSE_RATE_HZ)
    parser.add_argument("--max-pulse-rate-hz", type=float, default=DEFAULT_MAX_PULSE_RATE_HZ)
    parser.add_argument("--rate-points", type=int, default=DEFAULT_RATE_POINTS)
    parser.add_argument("--warmup-pulses", type=int, default=DEFAULT_WARMUP_PULSES)
    parser.add_argument("--sample-pulses", type=int, default=DEFAULT_SAMPLE_PULSES)
    parser.add_argument("--bunched-warmup-cycles", type=int, default=DEFAULT_BUNCHED_WARMUP_CYCLES)
    parser.add_argument("--bunched-sample-cycles", type=int, default=DEFAULT_BUNCHED_SAMPLE_CYCLES)
    parser.add_argument("--iid-warmup-cycles", type=int, default=DEFAULT_IID_WARMUP_CYCLES)
    parser.add_argument("--iid-sample-cycles", type=int, default=DEFAULT_IID_SAMPLE_CYCLES)
    parser.add_argument("--iid-seed", type=int, default=DEFAULT_IID_SEED)
    parser.add_argument("--include-pulse-rate-hz", type=float, action="append", default=[])
    args = parser.parse_args()

    if args.cb_depth < 1 or args.cb_depth > args.n_bins:
        raise SystemExit("--cb-depth must be in the range 1..N_BINS for this tag-addressable model")

    out_path = write_bunched_sweep(args)
    iid_out_path = write_iid_sweep(args)
    infinite_out_path = write_infinite_sweep(args)
    print(f"Wrote {out_path}")
    print(f"Wrote {iid_out_path}")
    print(f"Wrote {infinite_out_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
