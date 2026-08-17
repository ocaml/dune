#!/bin/sh
set -eu

script_dir=$(CDPATH= cd "$(dirname "$0")" && pwd)
if [ "$#" -gt 0 ]; then
  case $1 in
    /*) dune=$1 ;;
    *) dune=$PWD/$1 ;;
  esac
else
  dune=$script_dir/../../dune.exe
fi
cd "$script_dir"
version=$("$dune" --version)

run () {
  mode=$1
  rm -rf _build
  start=$(date +%s)
  DUNE_CONFIG__PRIORITY_SCHEDULING=$mode \
    "$dune" build --root . @benchmark -j10 --display=quiet --cache=disabled
  stop=$(date +%s)
  printf '%s: %ss\n' "$mode" "$((stop - start))"
}

printf '%s\n' "$version"
run disabled
run enabled
