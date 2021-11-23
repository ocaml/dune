#!/usr/bin/env sh
export BENCHMARKS_RUNNER=TRUE
case "$1" in
  "dune" ) test="dune_bench"; main="main";;
  "fiber" ) test="fiber_bench"; main="fiber_bench_main";;
esac
shift;
export BENCH_LIB="$test"
exec ./dune.exe exec --release -- "./bench/micro/$main.exe" -fork -run-without-cross-library-inlining "$@"
