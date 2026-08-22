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
if [ "$jobs" -lt 2 ] || [ "$#" -gt 1 ]; then
  usage
fi

script_dir=$(CDPATH='' cd "$(dirname "$0")" && pwd)
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
printf '(lang dune 3.21)\n\n(using rocq 0.11)\n' > "$workload/dune-project"
printf '(rocq.theory\n (name priority_scheduler)\n (modules :standard)\n (mode vo))\n\n' \
  > "$workload/dune"

cat > "$workload/slow.v" <<'EOF'
Fixpoint slow (n : nat) :=
  match n with
  | 0 => 1
  | S n => slow n * slow n
  end.
EOF

# One evaluation takes roughly 10ms, so this matches the original benchmark's
# [max (1 / JOBS) 0.1] seconds of work per job. Distribute the remainder across
# each group of JOBS modules instead of truncating it.
eval_work_units=100
min_eval_repetitions=10
eval_repetitions=$((eval_work_units / jobs))
eval_remainder=$((eval_work_units % jobs))
if [ "$eval_repetitions" -lt "$min_eval_repetitions" ]; then
  eval_repetitions=$min_eval_repetitions
  eval_remainder=0
fi

repetitions_for_index () {
  index=$1
  repetitions=$eval_repetitions
  if [ $((index % jobs)) -lt "$eval_remainder" ]; then
    repetitions=$((repetitions + 1))
  fi
  printf '%s\n' "$repetitions"
}

rocq_module () {
  module=$1
  dependency=$2
  repetitions=$3
  file=$workload/$module.v
  printf 'From priority_scheduler Require Import slow.\n' > "$file"
  if [ -n "$dependency" ]; then
    printf 'From priority_scheduler Require %s.\n' "$dependency" >> "$file"
  fi
  printf '\n' >> "$file"
  iteration=0
  while [ "$iteration" -lt "$repetitions" ]; do
    printf 'Time Eval lazy in slow 13.\n' >> "$file"
    iteration=$((iteration + 1))
  done
}

# JOBS*(JOBS-1) independent Rocq modules take approximately JOBS-1 rounds
# under FIFO once all slots are available.
independent_jobs=$((jobs * (jobs - 1)))
i=0
while [ "$i" -lt "$independent_jobs" ]; do
  module=$(printf 'b_independent_%03d' "$i")
  repetitions=$(repetitions_for_index "$i")
  rocq_module "$module" "" "$repetitions"
  i=$((i + 1))
done

# A JOBS-module dependency chain has approximately the same total work as one
# round of independent jobs per slot. Priority scheduling can overlap this
# critical path with the independent modules instead of starting it afterward.
previous=
i=0
while [ "$i" -lt "$jobs" ]; do
  module=$(printf 'z_chain_%03d' "$i")
  repetitions=$(repetitions_for_index "$i")
  rocq_module "$module" "$previous" "$repetitions"
  previous=$module
  i=$((i + 1))
done

{
  printf '(alias\n (name benchmark)\n (deps\n'
  i=0
  while [ "$i" -lt "$independent_jobs" ]; do
    printf '  b_independent_%03d.vo\n' "$i"
    i=$((i + 1))
  done
  printf '  %s.vo))\n' "$previous"
} >> "$workload/dune"

# Keep target digest scheduling outside the model measured by this benchmark.
measure () {
  mode=$1
  trial=$2
  timing=$script_dir/_build/time-$mode-$trial
  build_log=$script_dir/_build/build-$mode-$trial.log
  rm -rf "$workload/_build"
  if ! (cd "$workload" && { time -p env \
          DUNE_CONFIG__BACKGROUND_DIGESTS=disabled \
          DUNE_CONFIG__PRIORITY_SCHEDULING="$mode" \
          "$dune" build --root . @benchmark -j "$jobs" --display=quiet \
            --cache=disabled >/dev/null; }) 2> "$build_log"
  then
    cat "$build_log" >&2
    return 1
  fi
  awk '$1 == "real" || $1 == "user" || $1 == "sys" { print }' \
    "$build_log" > "$timing"
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
disabled=$(awk -v a="$disabled_1" -v b="$disabled_2" \
  'BEGIN { printf "%.2f", (a + b) / 2 }')
enabled=$(awk -v a="$enabled_1" -v b="$enabled_2" \
  'BEGIN { printf "%.2f", (a + b) / 2 }')
improvement=$(awk -v d="$disabled" -v e="$enabled" \
  'BEGIN { printf "%.1f", 100 * (d - e) / d }')
maximum=$(awk -v j="$jobs" \
  'BEGIN { printf "%.1f", 100 * (j - 1) / (2 * j - 1) }')
printf 'disabled: %ss\n' "$disabled"
printf 'enabled: %ss\n' "$enabled"
printf 'improvement: %s%% (maximum %s%%)\n' "$improvement" "$maximum"
