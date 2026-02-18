#!/usr/bin/env bash
export BENCHMARKS_RUNNER=TRUE

declare -A benchmarks=(
  [dune]="dune_bench:main"
  [memo]="memo_bench:memo_bench_main"
  [thread_pool]="thread_pool_bench:thread_pool_bench_main"
  [digest]="digest_bench:digest_bench_main"
  [path]="path_bench:path_bench_main"
)

if [[ -z "${benchmarks[$1]}" ]]; then
  echo "Unknown benchmark: $1" >&2
  echo "Available: ${!benchmarks[*]}" >&2
  exit 1
fi

IFS=: read -r test main <<< "${benchmarks[$1]}"
shift;
export BENCH_LIB="$test"
exec ./dune.exe exec --release -- "./bench/micro/$main.exe" -fork -run-without-cross-library-inlining "$@"
