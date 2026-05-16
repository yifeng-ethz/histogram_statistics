#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
REPO_ROOT=$(cd "${ROOT_DIR}/.." && pwd)
MODEL_DIR="$ROOT_DIR/model"
ART_DIR="$MODEL_DIR/artifacts"
BUILD_DIR="$ART_DIR/plot_work"
DISLIN_DIR="${DISLIN_DIR:-${REPO_ROOT}/packet_scheduler/.vendor/dislin}"

mkdir -p "$ART_DIR" "$BUILD_DIR"

python3 "$MODEL_DIR/tlm/bunched_coalescing_model.py" \
  --out-dir "$ART_DIR" \
  --n-bins 256 \
  --cb-depth 256 \
  --clock-hz 125000000 \
  --push-hits-per-cycle 2 \
  --drain-cells-per-cycle 0.5 \
  --min-pulse-rate-hz 10000 \
  --max-pulse-rate-hz 950000 \
  --rate-points 48 \
  --warmup-pulses 32 \
  --sample-pulses 128 \
  --bunched-warmup-cycles 5000 \
  --bunched-sample-cycles 50000 \
  --iid-warmup-cycles 5000 \
  --iid-sample-cycles 50000

gcc -O2 -Wall -Wextra -std=c11 \
  -I"$DISLIN_DIR" \
  "$MODEL_DIR/scripts/bunched_coalescing_dislin.c" \
  -L"$DISLIN_DIR" \
  -Wl,-rpath,"$DISLIN_DIR" \
  -ldislin -lm \
  -o "$BUILD_DIR/bunched_coalescing_dislin"

gcc -O2 -Wall -Wextra -std=c11 \
  -I"$DISLIN_DIR" \
  "$MODEL_DIR/scripts/bunched_infinite_queue_dislin.c" \
  -L"$DISLIN_DIR" \
  -Wl,-rpath,"$DISLIN_DIR" \
  -ldislin -lm \
  -o "$BUILD_DIR/bunched_infinite_queue_dislin"

: > "$ART_DIR/bunched_coalescing_dislin_png.log"
: > "$ART_DIR/bunched_coalescing_dislin_svg.log"
: > "$ART_DIR/bunched_infinite_queue_dislin_png.log"
: > "$ART_DIR/bunched_infinite_queue_dislin_svg.log"

"$BUILD_DIR/bunched_coalescing_dislin" \
  "$ART_DIR/bunched_coalescing_rate_sweep.csv" \
  "$ART_DIR/bunched_coalescing_rate_sweep.png" \
  occupancy \
  "Bunched" | tee -a "$ART_DIR/bunched_coalescing_dislin_png.log"

"$BUILD_DIR/bunched_coalescing_dislin" \
  "$ART_DIR/bunched_coalescing_rate_sweep.csv" \
  "$ART_DIR/bunched_coalescing_rate_sweep.svg" \
  occupancy \
  "Bunched" | tee -a "$ART_DIR/bunched_coalescing_dislin_svg.log"

"$BUILD_DIR/bunched_coalescing_dislin" \
  "$ART_DIR/bunched_coalescing_rate_sweep.csv" \
  "$ART_DIR/bunched_coalescing_kick_sweep.png" \
  kick \
  "Bunched" | tee -a "$ART_DIR/bunched_coalescing_dislin_png.log"

"$BUILD_DIR/bunched_coalescing_dislin" \
  "$ART_DIR/bunched_coalescing_rate_sweep.csv" \
  "$ART_DIR/bunched_coalescing_kick_sweep.svg" \
  kick \
  "Bunched" | tee -a "$ART_DIR/bunched_coalescing_dislin_svg.log"

"$BUILD_DIR/bunched_coalescing_dislin" \
  "$ART_DIR/iid_poisson_coalescing_rate_sweep.csv" \
  "$ART_DIR/iid_poisson_coalescing_rate_sweep.png" \
  occupancy \
  "iid-Poisson" | tee -a "$ART_DIR/bunched_coalescing_dislin_png.log"

"$BUILD_DIR/bunched_coalescing_dislin" \
  "$ART_DIR/iid_poisson_coalescing_rate_sweep.csv" \
  "$ART_DIR/iid_poisson_coalescing_rate_sweep.svg" \
  occupancy \
  "iid-Poisson" | tee -a "$ART_DIR/bunched_coalescing_dislin_svg.log"

"$BUILD_DIR/bunched_coalescing_dislin" \
  "$ART_DIR/iid_poisson_coalescing_rate_sweep.csv" \
  "$ART_DIR/iid_poisson_coalescing_kick_sweep.png" \
  kick \
  "iid-Poisson" | tee -a "$ART_DIR/bunched_coalescing_dislin_png.log"

"$BUILD_DIR/bunched_coalescing_dislin" \
  "$ART_DIR/iid_poisson_coalescing_rate_sweep.csv" \
  "$ART_DIR/iid_poisson_coalescing_kick_sweep.svg" \
  kick \
  "iid-Poisson" | tee -a "$ART_DIR/bunched_coalescing_dislin_svg.log"

"$BUILD_DIR/bunched_infinite_queue_dislin" \
  "$ART_DIR/bunched_infinite_queue_rate_sweep.csv" \
  "$ART_DIR/bunched_infinite_queue_rate_sweep.png" | tee -a "$ART_DIR/bunched_infinite_queue_dislin_png.log"

"$BUILD_DIR/bunched_infinite_queue_dislin" \
  "$ART_DIR/bunched_infinite_queue_rate_sweep.csv" \
  "$ART_DIR/bunched_infinite_queue_rate_sweep.svg" | tee -a "$ART_DIR/bunched_infinite_queue_dislin_svg.log"

perl -0pi -e 's/\n+\z/\n/' "$ART_DIR/bunched_coalescing_dislin_png.log" "$ART_DIR/bunched_coalescing_dislin_svg.log"
perl -0pi -e 's/\n+\z/\n/' "$ART_DIR/bunched_infinite_queue_dislin_png.log" "$ART_DIR/bunched_infinite_queue_dislin_svg.log"

printf 'Wrote %s\n' "$ART_DIR/bunched_coalescing_rate_sweep.csv"
printf 'Wrote %s\n' "$ART_DIR/bunched_coalescing_rate_sweep.png"
printf 'Wrote %s\n' "$ART_DIR/bunched_coalescing_rate_sweep.svg"
printf 'Wrote %s\n' "$ART_DIR/bunched_coalescing_kick_sweep.png"
printf 'Wrote %s\n' "$ART_DIR/bunched_coalescing_kick_sweep.svg"
printf 'Wrote %s\n' "$ART_DIR/iid_poisson_coalescing_rate_sweep.csv"
printf 'Wrote %s\n' "$ART_DIR/iid_poisson_coalescing_rate_sweep.png"
printf 'Wrote %s\n' "$ART_DIR/iid_poisson_coalescing_rate_sweep.svg"
printf 'Wrote %s\n' "$ART_DIR/iid_poisson_coalescing_kick_sweep.png"
printf 'Wrote %s\n' "$ART_DIR/iid_poisson_coalescing_kick_sweep.svg"
printf 'Wrote %s\n' "$ART_DIR/bunched_infinite_queue_rate_sweep.csv"
printf 'Wrote %s\n' "$ART_DIR/bunched_infinite_queue_rate_sweep.png"
printf 'Wrote %s\n' "$ART_DIR/bunched_infinite_queue_rate_sweep.svg"
