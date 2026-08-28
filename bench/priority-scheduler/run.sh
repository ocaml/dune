#!/bin/sh
set -eu

usage () {
  echo "usage: $0 [JOBS] [DUNE] [POLICY]" >&2
  exit 2
}

jobs=10
case ${1-} in
  '') ;;
  *[!0-9]*) ;;
  *) jobs=$1; shift ;;
esac
if [ "$jobs" -lt 2 ] || [ "$#" -gt 2 ]; then
  usage
fi

script_dir=$(CDPATH= cd "$(dirname "$0")" && pwd)
if [ "$#" -ge 1 ] && [ -n "$1" ]; then
  case $1 in
    /*) dune=$1 ;;
    */*) dune=$PWD/$1 ;;
    *) dune=$1 ;;
  esac
else
  dune=$script_dir/../../dune.exe
fi
if [ "$#" -gt 0 ]; then
  shift
fi
policy=${1-${DUNE_CONFIG__PRIORITY_SCHEDULING_POLICY-current}}
random_seed=${DUNE_CONFIG__PRIORITY_SCHEDULING_RANDOM_SEED-0}
case $policy in
  current | fifo | lifo | random | revealed-depth) ;;
  *) usage ;;
esac

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

# Keep representative runs near one second while retaining enough duration to
# make process and timer overhead small. Larger values still scale linearly.
unit_duration=$(awk -v j="$jobs" \
  'BEGIN { d = 1 / j; if (d < 0.1) d = 0.1; printf "%.9f", d }')

# For JOBS=m and time unit=u, m*(m-1) independent jobs take (m-1)*u under
# FIFO and m*u on the m-1 slots left by priority scheduling. A critical path
# lasting m*u overlaps all independent work in priority mode. The ideal
# reduction is (m-1)/(2*m-1), the maximum for this scheduling problem.
independent_jobs=$((jobs * (jobs - 1)))
i=0
while [ "$i" -lt "$independent_jobs" ]; do
  rule "independent-$i" "$unit_duration"
  i=$((i + 1))
done

# Use several short links so queue disciplines have repeated opportunities to
# discover and advance the serial chain.
chain_jobs=20
chain_duration=$(awk -v j="$jobs" -v n="$chain_jobs" -v u="$unit_duration" \
  'BEGIN { printf "%.9f", j * u / n }')
previous=
i=0
while [ "$i" -lt $((chain_jobs - 1)) ]; do
  rule "chain-$i" "$chain_duration" "$previous"
  previous="chain-$i"
  i=$((i + 1))
done
rule chain-final "$chain_duration" "$previous"

{
  printf '(alias\n (name benchmark)\n (deps\n'
  i=0
  while [ "$i" -lt "$independent_jobs" ]; do
    printf '  independent-%s\n' "$i"
    i=$((i + 1))
  done
  printf '  chain-final))\n'
} >> "$workload/dune"

# Keep target digest scheduling outside the model measured by this benchmark.
measure () {
  mode=$1
  trial=$2
  timing=$script_dir/_build/time-$mode-$trial
  rm -rf "$workload/_build"
  if ! (cd "$workload" && { time -p env \
          DUNE_CONFIG__BACKGROUND_DIGESTS=disabled \
          DUNE_CONFIG__PRIORITY_SCHEDULING="$mode" \
          DUNE_CONFIG__PRIORITY_SCHEDULING_POLICY="$policy" \
          DUNE_CONFIG__PRIORITY_SCHEDULING_RANDOM_SEED="$random_seed" \
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
if [ "$policy" = random ]; then
  printf '%s (-j %s, policy %s, seed %s)\n' "$version" "$jobs" "$policy" "$random_seed"
else
  printf '%s (-j %s, policy %s)\n' "$version" "$jobs" "$policy"
fi

# Run in both orders to balance one-time filesystem and OS cache effects.
disabled_1=$(measure disabled 1)
enabled_1=$(measure enabled 1)
enabled_2=$(measure enabled 2)
disabled_2=$(measure disabled 2)
disabled=$(awk -v a="$disabled_1" -v b="$disabled_2" 'BEGIN { printf "%.2f", (a + b) / 2 }')
enabled=$(awk -v a="$enabled_1" -v b="$enabled_2" 'BEGIN { printf "%.2f", (a + b) / 2 }')
improvement=$(awk -v d="$disabled" -v e="$enabled" \
  'BEGIN { printf "%.1f", 100 * (d - e) / d }')
maximum=$(awk -v j="$jobs" 'BEGIN { printf "%.1f", 100 * (j - 1) / (2 * j - 1) }')
printf 'disabled: %ss\n' "$disabled"
printf '%s: %ss\n' "$policy" "$enabled"
printf 'improvement: %s%% (maximum %s%%)\n' "$improvement" "$maximum"
