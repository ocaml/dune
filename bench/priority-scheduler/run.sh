#!/bin/sh
set -eu

usage () {
  echo "usage: $0 [JOBS] [DUNE]" >&2
  exit 2
}

jobs=10
case ${1-} in
  '') ;;
  *[!0-9]*) ;;
  *) jobs=$1; shift ;;
esac
if [ "$jobs" -le 2 ] || [ "$#" -gt 1 ]; then
  usage
fi

script_dir=$(CDPATH= cd "$(dirname "$0")" && pwd)
if [ "$#" -eq 1 ] && [ -n "$1" ]; then
  case $1 in
    /*) dune=$1 ;;
    */*) dune=$PWD/$1 ;;
    *) dune=$1 ;;
  esac
else
  dune=$script_dir/../../dune.exe
fi

workload=$script_dir/_build/workload
rm -rf "$script_dir/_build"
mkdir -p "$workload"
printf '(lang dune 3.21)\n' > "$workload/dune-project"

rule () {
  target=$1
  duration=$2
  dependency=${3-}
  {
    printf '(rule\n (target %s)\n' "$target"
    if [ -n "$dependency" ]; then
      printf ' (deps %s)\n' "$dependency"
    fi
    printf ' (action\n  (progn\n   (run sleep %s)\n   (write-file %s done))))\n\n' \
      "$duration" "$target"
  } >> "$workload/dune"
}

# Fill every slot briefly so both schedulers select from the same ready queue.
i=0
while [ "$i" -lt "$jobs" ]; do
  rule "gate-$i" 0.1
  i=$((i + 1))
done

# Two waves of independent 2-second jobs take 4 seconds under FIFO. On JOBS-1
# slots they take three waves, so a 6-second critical path is just long enough
# to overlap all of the independent work. The ideal result is 10 seconds under
# FIFO and 6 seconds with priorities: a 40% improvement for every JOBS > 2.
independent_jobs=$((2 * jobs))
i=0
while [ "$i" -lt "$independent_jobs" ]; do
  rule "independent-$i" 2
  i=$((i + 1))
done

previous=
i=0
while [ "$i" -lt 10 ]; do
  rule "chain-$i" 0.2 "$previous"
  previous="chain-$i"
  i=$((i + 1))
done
rule chain-final 4 "$previous"

{
  printf '(alias\n (name benchmark)\n (deps\n'
  i=0
  while [ "$i" -lt "$jobs" ]; do
    printf '  gate-%s\n' "$i"
    i=$((i + 1))
  done
  i=0
  while [ "$i" -lt "$independent_jobs" ]; do
    printf '  independent-%s\n' "$i"
    i=$((i + 1))
  done
  printf '  chain-final))\n'
} >> "$workload/dune"

measure () {
  mode=$1
  trial=$2
  timing=$script_dir/_build/time-$mode-$trial
  rm -rf "$workload/_build"
  if ! (cd "$workload" && { time -p env DUNE_CONFIG__PRIORITY_SCHEDULING=$mode \
          "$dune" build --root . @benchmark -j "$jobs" --display=quiet \
            --cache=disabled; }) 2> "$timing"
  then
    cat "$timing" >&2
    return 1
  fi
  awk '$1 != "real" && $1 != "user" && $1 != "sys" { print }' "$timing" >&2
  if ! elapsed=$(awk '
      $1 == "real" && $2 ~ /^[0-9]+([.][0-9]+)?$/ { value = $2; count++ }
      END { if (count != 1) exit 1; print value }
    ' "$timing")
  then
    echo "failed to parse timing output from $timing" >&2
    cat "$timing" >&2
    return 1
  fi
  printf '%s\n' "$elapsed"
}

version=$("$dune" --version)
printf '%s (-j %s)\n' "$version" "$jobs"

# Run in both orders to balance one-time filesystem and OS cache effects.
disabled_1=$(measure disabled 1)
enabled_1=$(measure enabled 1)
enabled_2=$(measure enabled 2)
disabled_2=$(measure disabled 2)
disabled=$(awk -v a="$disabled_1" -v b="$disabled_2" 'BEGIN { printf "%.2f", (a + b) / 2 }')
enabled=$(awk -v a="$enabled_1" -v b="$enabled_2" 'BEGIN { printf "%.2f", (a + b) / 2 }')
improvement=$(awk -v d="$disabled" -v e="$enabled" \
  'BEGIN { printf "%.1f", 100 * (d - e) / d }')
printf 'disabled: %ss\n' "$disabled"
printf 'enabled: %ss\n' "$enabled"
printf 'improvement: %s%%\n' "$improvement"
