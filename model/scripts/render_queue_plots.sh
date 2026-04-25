#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
REPO_ROOT=$(cd "${ROOT_DIR}/.." && pwd)
MODEL_DIR="$ROOT_DIR/model"
ART_DIR="$MODEL_DIR/artifacts"
BUILD_DIR="$ART_DIR/plot_work"
DISLIN_DIR="${DISLIN_DIR:-${REPO_ROOT}/packet_scheduler/.vendor/dislin}"

mkdir -p "$ART_DIR" "$BUILD_DIR"

python3 "$MODEL_DIR/tlm/queue_depth_model.py" --out-dir "$ART_DIR" --queue-depth 160

gcc -O2 -Wall -Wextra -std=c11 \
  -I"$DISLIN_DIR" \
  "$MODEL_DIR/scripts/queue_dislin_plots.c" \
  -L"$DISLIN_DIR" \
  -Wl,-rpath,"$DISLIN_DIR" \
  -ldislin -lm \
  -o "$BUILD_DIR/queue_dislin_plots"

: > "$ART_DIR/queue_dislin_png.log"
: > "$ART_DIR/queue_dislin_svg.log"

render_png() {
  "$BUILD_DIR/queue_dislin_plots" "$@" | tee -a "$ART_DIR/queue_dislin_png.log"
}

render_svg() {
  "$BUILD_DIR/queue_dislin_plots" "$@" | tee -a "$ART_DIR/queue_dislin_svg.log"
}

render_png loss \
  "$ART_DIR/queue_loss_vs_depth_8mutrig.csv" \
  "$ART_DIR/queue_loss_vs_depth_8mutrig.png"

render_svg loss \
  "$ART_DIR/queue_loss_vs_depth_8mutrig.csv" \
  "$ART_DIR/queue_loss_vs_depth_8mutrig.svg"

render_png quantiles \
  "$ART_DIR/queue_depth_quantiles_8mutrig.csv" \
  "$ART_DIR/queue_depth_quantiles_8mutrig.png"

render_svg quantiles \
  "$ART_DIR/queue_depth_quantiles_8mutrig.csv" \
  "$ART_DIR/queue_depth_quantiles_8mutrig.svg"

render_png trace \
  "$ART_DIR/queue_trace_expected_tlm.csv" \
  "$ART_DIR/queue_trace_observed_rtl.csv" \
  "$ART_DIR/queue_trace_tlm_rtl_match.png"

render_svg trace \
  "$ART_DIR/queue_trace_expected_tlm.csv" \
  "$ART_DIR/queue_trace_observed_rtl.csv" \
  "$ART_DIR/queue_trace_tlm_rtl_match.svg"

perl -0pi -e 's/\n+\z/\n/' "$ART_DIR/queue_dislin_png.log" "$ART_DIR/queue_dislin_svg.log"

printf 'Wrote %s\n' "$ART_DIR/queue_loss_vs_depth_8mutrig.png"
printf 'Wrote %s\n' "$ART_DIR/queue_loss_vs_depth_8mutrig.svg"
printf 'Wrote %s\n' "$ART_DIR/queue_depth_quantiles_8mutrig.png"
printf 'Wrote %s\n' "$ART_DIR/queue_depth_quantiles_8mutrig.svg"
printf 'Wrote %s\n' "$ART_DIR/queue_trace_tlm_rtl_match.png"
printf 'Wrote %s\n' "$ART_DIR/queue_trace_tlm_rtl_match.svg"
