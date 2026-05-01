# Synthesis Signoff: histogram_statistics_v2

Status: `PASS`

This signoff is based on the 2026-05-01 standalone Quartus compile of
`histogram_statistics_v2` release `26.1.8.0501` in [syn/quartus](syn/quartus).
The compile was rerun after the Phase-6 deferred ping-pong readout fix.

## Build Context

- Project: `histogram_statistics_v2_signoff`
- Revision: `histogram_statistics_v2_standalone`
- Top-level entity: `histogram_statistics_v2`
- Device family: Arria V
- Device: `5AGXBA7D4F31C5`
- Quartus version: `18.1.0 Build 625 09/12/2018 SJ Standard Edition`
- Flow report timestamp: `Fri May 1 09:26:21 2026`
- Constraint file:
  [syn/quartus/histogram_statistics_v2_standalone.sdc](syn/quartus/histogram_statistics_v2_standalone.sdc)
- Clock target:
  - functional target: `125 MHz`
  - signoff target with 10% margin: `137.5 MHz`
  - constrained period: `7.273 ns`

## Command

```sh
quartus_sh --flow compile histogram_statistics_v2_signoff -c histogram_statistics_v2_standalone
```

Primary log:

```text
syn/quartus/histogram_statistics_v2_standalone_compile_26_1_8_0501.log
```

## Timing Closure

From
[syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary](syn/quartus/output_files/histogram_statistics_v2_standalone.sta.summary)
and
[syn/quartus/output_files/histogram_statistics_v2_standalone.sta.rpt](syn/quartus/output_files/histogram_statistics_v2_standalone.sta.rpt):

| Corner | Fmax | Setup Slack | Hold Slack | Min Pulse Width Slack |
|---|---:|---:|---:|---:|
| Slow 1100mV 85C | 141.58 MHz | +0.210 ns | +0.270 ns | +2.710 ns |
| Slow 1100mV 0C | 145.60 MHz | +0.405 ns | +0.251 ns | +2.694 ns |
| Fast 1100mV 85C | n/a in Fmax table | +2.816 ns | +0.160 ns | +2.821 ns |
| Fast 1100mV 0C | n/a in Fmax table | +3.300 ns | +0.149 ns | +2.833 ns |

Worst signoff corner:

- clock: `i_clk`
- worst-case setup slack: `+0.210 ns`
- worst-case hold slack: `+0.149 ns`
- worst-case minimum pulse-width slack: `+2.694 ns`
- worst-case Fmax: `141.58 MHz`

Margin assessment:

- required signoff frequency: `137.49 MHz`
- achieved worst-case Fmax: `141.58 MHz`
- frequency headroom: `4.09 MHz`
- relative headroom: `2.97%`

## Resource Summary

From
[syn/quartus/output_files/histogram_statistics_v2_standalone.fit.summary](syn/quartus/output_files/histogram_statistics_v2_standalone.fit.summary)
and
[syn/quartus/output_files/histogram_statistics_v2_standalone.fit.rpt](syn/quartus/output_files/histogram_statistics_v2_standalone.fit.rpt):

| Resource | Usage | Device Utilization |
|---|---:|---:|
| Logic utilization | 14,020 / 91,680 | 15% |
| Dedicated logic registers | 5,309 / 183,360 | 3% |
| Total LABs used | 1,693 / 9,168 | 18% |
| Total RAM blocks | 2 / 1,366 | <1% |
| Total block memory bits | 16,384 / 13,987,840 | <1% |
| Total MLAB memory bits | 69,632 | n/a |
| DSP blocks | 0 / 800 | 0% |
| Peak interconnect usage | 38.6% / 37.8% / 41.2% | total / H / V |

## Flow Runtime

From
[syn/quartus/output_files/histogram_statistics_v2_standalone.flow.rpt](syn/quartus/output_files/histogram_statistics_v2_standalone.flow.rpt):

| Stage | Elapsed | CPU Time |
|---|---:|---:|
| Analysis & Synthesis | 00:00:41 | 00:01:06 |
| Fitter | 00:04:21 | 00:10:03 |
| Assembler | 00:00:14 | 00:00:14 |
| Timing Analyzer | 00:00:24 | 00:00:53 |
| Total | 00:05:40 | 00:12:16 |

## Timing Changes

The earlier Phase-6 compile exposed two real timing risks:

- `coalescing_queue` diagnostic overflow counting placed hit-bin decode on the
  wide overflow counter enable.
- negative debug modes fed CSR-mode and debug-source selection directly into
  the ingress write-request path.

The committed implementation closes those risks with registered control and
debug-source boundaries:

- `coalescing_queue` records overflow as a one-cycle event before incrementing
  the diagnostic counter.
- `histogram_statistics_v2` decodes negative debug-mode flags only at CSR apply.
- `histogram_statistics_v2` captures selected debug-source samples one cycle
  before ingress filtering.

The debug-mode capture adds one `i_clk` cycle of debug-observation latency. Rate
and delay histogram content are unchanged; this block is an observation surface,
and the extra cycle is deterministic.

The `26.1.8.0501` refresh fixes a live readout correctness bug, not a new timing
path: deferred host reads in `pingpong_sram` now latch the frozen bank in
ping-pong mode, matching the immediate read path. This prevents a live
1-second rate-bin dump from mixing frozen last-interval bins with the active
interval while MuTRiG traffic is still updating the SRAM.

## Static And Simulation Evidence

Static screen:

```sh
python3 ~/.codex/skills/rtl-linter-and-checker/scripts/questa_static_screen.py \
  --top histogram_statistics_v2 \
  --filelist /data3/yifeng/mu3e_ip_dev/qverify/histogram_statistics_20260501/coalescing_queue/histogram_statistics_v2_static.f \
  --pre-do /data3/yifeng/mu3e_ip_dev/qverify/histogram_statistics_20260501/coalescing_queue/pre_compile_vendor.do \
  --work-dir /data3/yifeng/mu3e_ip_dev/qverify/histogram_statistics_20260501/histogram_v2_static_release_26_1_8_pingpong \
  rtl/pingpong_sram.vhd rtl/histogram_statistics_v2.vhd
```

Result: PASS, with transcript at
`/data3/yifeng/mu3e_ip_dev/qverify/histogram_statistics_20260501/histogram_v2_static_release_26_1_8_pingpong/questa_static_screen.log`.
The report shows lint error `0`, CDC violations `0`, and RDC violations `0`.

Simulation refresh:

| Flow | Command | Result |
|---|---|---|
| Standalone deterministic suite | `make -C tb run_all SEED=42` | 46 PASS, 0 FAIL |
| Pending-read frozen-bank case | `make -C tb run TEST=P05_pending_read_frozen_bank SEED=42` | PASS |
| Version metadata smoke | `make -C tb run TEST=B04_version SEED=42` | PASS, `VERSION=0x1a0181f5`, `DATE=20260501` |
| Historical UVM debug smoke | `make -C tb/uvm run TEST=hist_debug_test SEED=42` | PASS, 0 UVM errors/fatals |
| Historical UVM queue-error smoke | `make -C tb/uvm run TEST=hist_error_queue_test SEED=42` | PASS, 0 UVM errors/fatals |
| Historical UVM QST profile smoke | `make -C tb/uvm run TEST=hist_prof_qst_test SEED=42` | PASS, 0 UVM errors/fatals |

The accidental `TEST=B04_identity_header` invocation was a command error. The
standalone harness uses `B04_version`; the correct version test passed after the
metadata update.

## Signoff Conclusion

Release `26.1.8.0501` is signed off for the standalone Arria V IP target:

- timing closes at the tightened 137.5 MHz target
- slow-85 setup slack is `+0.210 ns`
- hold and minimum-pulse checks are positive at all corners
- resources remain small relative to the device envelope
- standalone DV, hard static screen, and Quartus synthesis all pass after the
  ping-pong readout fix; the focused UVM smokes remain historical support from
  the preceding same-day timing checkpoint

This signoff does not claim live FEB/SWB/DMA end-to-end closure. It only clears
the histogram IP checkpoint needed before regenerating and rebuilding the active
Phase-6 FEB datapath.
