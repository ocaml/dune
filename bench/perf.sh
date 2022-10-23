#!/usr/bin/env bash

# Run this script simply as ./bench/perf.sh from the root directory.

set -e

TEST_REPO=https://github.com/ocaml-dune/dune-bench
TEST_COMMIT=b6bfaf2974ec8ee1eea92c4316ec37b9966322e3

# Some alternative benchmarks:

# TEST_REPO=https://github.com/ocaml/dune
# TEST_COMMIT=002edc11f4e0a57f11d5226cb2497c8b406027b5

# TEST_REPO=https://github.com/avsm/platform
# TEST_COMMIT=b254e3c6b60f3c0c09dfdcde92eb1abdc267fa1c

dune() {
  TIMEFORMAT=$'real %Rs\nuser %Us\nsys  %Ss\n'; time ../_build/default/bin/main.exe "$@" > /dev/null 2>&1
}

setup_test() {
  mkdir -p _perf

  cd _perf
  if [ ! -f README.md ]; then
    echo "Cloning $TEST_REPO..."
    wget $TEST_REPO/archive/$TEST_COMMIT.tar.gz
    tar -xzf $TEST_COMMIT.tar.gz --strip-components=1
  fi
  cd ..
}

pad () {
  while IFS='' read -r x; do printf "%-$1s\n" "$x"; done
}

run_test() {
  echo "Building Dune..."
  # [make release] is used for bootstrapping, but the real binary to benchmark is
  # then produced by a separate dune invocation.
  # This is done mainly because [make release] won't rebuild dune if it's stale.
  make release > /dev/null
  ./dune.exe build _build/default/bin/main.exe

  cd _perf
  rm -rf _build

  echo "Running full build..."
  dune build --release --cache=disabled 2>> $1

  echo "Running zero build..."
  dune build --release --cache=disabled 2>> $1

  cd ..
}

setup_test
CURRENT_BRANCH=$(git branch | sed -n -e 's/^\* \(.*\)/\1/p')

rm -f _perf/rows _perf/current _perf/main

echo "            " >> _perf/rows
echo "            " >> _perf/rows
echo "           |" >> _perf/rows
echo "Full build |" >> _perf/rows
echo "           |" >> _perf/rows
echo "            " >> _perf/rows
echo "           |" >> _perf/rows
echo "Zero build |" >> _perf/rows
echo "           |" >> _perf/rows

echo "Current branch" >> _perf/current
echo "==============" >> _perf/current
echo "Testing the current branch ($CURRENT_BRANCH)"
run_test current

echo " Main branch " >> _perf/main
echo "=============" >> _perf/main
git checkout main
echo "Testing main"
run_test main

git checkout $CURRENT_BRANCH

echo ""
echo "Summary for building $TEST_REPO:"
echo ""

paste -d ' ' <(pad 10 < _perf/rows) <(pad 14 < _perf/current) _perf/main
