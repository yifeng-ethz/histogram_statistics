#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
REPO_ROOT=$(cd "${ROOT_DIR}/.." && pwd)
MODEL_DIR="$ROOT_DIR/model"
ART_DIR="$MODEL_DIR/artifacts"
WORK_DIR="$ART_DIR/rtl_work"

source "${REPO_ROOT}/scripts/questa_one_env.sh"

mkdir -p "$ART_DIR" "$WORK_DIR"

python3 "$MODEL_DIR/tlm/queue_depth_model.py" \
  --out-dir "$ART_DIR" \
  --queue-depth 256

rm -rf "$WORK_DIR/work"
(
  cd "$WORK_DIR"
  cp "${QSIM_INI}" modelsim.ini
  chmod u+w modelsim.ini
  "${VLIB}" work
  "${VMAP}" -ini modelsim.ini work work >/dev/null
  "${VCOM}" -ini modelsim.ini -work work -2008 "$ROOT_DIR/rtl/histogram_statistics_v2_pkg.vhd"
  "${VCOM}" -ini modelsim.ini -work work -2008 "$ROOT_DIR/rtl/coalescing_queue.vhd"
  "${VCOM}" -ini modelsim.ini -work work -2008 "$MODEL_DIR/rtl_sim/tb_coalescing_queue_trace.vhd"
  "${VSIM}" -ini modelsim.ini -c -work work \
    -gQUEUE_DEPTH_G=256 \
    -gSTIM_FILE_G="$ART_DIR/queue_trace_stimulus.csv" \
    -gOUT_FILE_G="$ART_DIR/queue_trace_observed_rtl.csv" \
    tb_coalescing_queue_trace \
    -do "run -all; quit -f" | tee "$ART_DIR/queue_trace_rtl.log"
)

python3 "$MODEL_DIR/tlm/queue_depth_model.py" \
  --compare-rtl "$ART_DIR/queue_trace_expected_tlm.csv" "$ART_DIR/queue_trace_observed_rtl.csv" \
  --compare-out "$ART_DIR/queue_trace_compare.csv"
