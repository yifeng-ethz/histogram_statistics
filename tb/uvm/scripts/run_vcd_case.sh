#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
UVM_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
TB_DIR="$(cd "$UVM_DIR/.." && pwd)"

usage() {
  cat <<'EOF'
Usage:
  run_vcd_case.sh --case-id HIST-SMOKE --test hist_smoke_test [--seed 111]
  run_vcd_case.sh --case-id P141-REAL-TRIAD --test hist_real_hit_rand_test --seed 141 \
    --plusargs '+HIST_REAL_PROFILE=triad +HIST_REAL_HITS=2048'
EOF
}

CASE_ID=""
TEST_NAME=""
SEED="111"
PLUSARGS=""
OUT=""
WAVE_SCOPE="/tb_top/*"

while [ $# -gt 0 ]; do
  case "$1" in
    --case-id) CASE_ID="$2"; shift 2 ;;
    --test) TEST_NAME="$2"; shift 2 ;;
    --seed) SEED="$2"; shift 2 ;;
    --plusargs) PLUSARGS="$2"; shift 2 ;;
    --out) OUT="$2"; shift 2 ;;
    --scope) WAVE_SCOPE="$2"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "unknown arg: $1" >&2; usage; exit 2 ;;
  esac
done

if [ -z "$CASE_ID" ] || [ -z "$TEST_NAME" ]; then
  usage
  exit 2
fi

mkdir -p "$TB_DIR/waves/generated"
if [ -z "$OUT" ]; then
  OUT="$TB_DIR/waves/generated/${CASE_ID}.vcd"
fi

make -C "$UVM_DIR" run_vcd TEST="$TEST_NAME" SEED="$SEED" VCD_FILE="$OUT" WAVE_SCOPE="$WAVE_SCOPE" PLUSARGS="$PLUSARGS"
echo "$OUT"
