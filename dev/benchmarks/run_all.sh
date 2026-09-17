#!/usr/bin/env bash
# Benchmarks every version sequentially. They must not overlap: two R sessions
# competing for CPU would make the timings meaningless.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
BENCH="${BENCH_DIR:?set BENCH_DIR to the directory holding libs/<version>}"
mkdir -p "$HERE/results"

for v in master premig nodplyr; do
  echo "=== $v  $(date +%H:%M:%S) ==="
  Rscript "$HERE/run_bench.R" "$v" "$BENCH/libs/$v" "$HERE/results/$v.csv" \
    > "$HERE/results/$v.log" 2>&1
  echo "  exit=$?  finished $(date +%H:%M:%S)"
  grep -E "^  [a-z]" "$HERE/results/$v.log" || true
done
echo "=== all done $(date +%H:%M:%S) ==="
