set -euo pipefail

echo "=== machine ==="
uname -a
sysctl -n machdep.cpu.brand_string
sysctl -n hw.memsize
sw_vers

echo "=== env ==="
env | grep -E '^(GC_|MADLIB_|UV_THREADPOOL_SIZE)' || true

echo "=== toolchain ==="
madlib --version

echo "=== build+run matrix ==="
for mode in O1 O3; do
  exe="/tmp/map_${mode}_exe"
  madlib compile -i fixtures/Map.mad -t llvm --$mode -o "$exe"

  for heap in default 6000000000; do
    out="/tmp/map_${mode}_${heap}.txt"
    if [ "$heap" = "default" ]; then
      /usr/bin/time -l "$exe" >"$out" 2>&1
    else
      /usr/bin/time -l env GC_INITIAL_HEAP_SIZE=$heap "$exe" >"$out" 2>&1
    fi
    echo "--- $mode / $heap ---"
    grep -nE "real|user|sys|maximum resident|page faults|involuntary context" "$out" || true
  done
done

echo "=== done ==="
ls -lh /tmp/map_O*_*.txt
