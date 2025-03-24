#!/usr/bin/env sh
export BENCHMARKS_RUNNER=TRUE
case "$1" in
  "dune" ) test="dune_bench"; main="main";;
  "memo" ) test="memo_bench"; main="memo_bench_main";;
  "thread_pool" ) test="thread_pool_bench"; main="thread_pool_bench_main";;
  "digest" ) test="digest_bench"; main="digest_bench_main";;
  "path" ) test="path_bench"; main="path_bench_main";;
esac
shift;
export BENCH_LIB="$test"
exec ./dune.exe exec --release -- "./bench/micro/$main.exe" -fork -run-without-cross-library-inlining "$@"
