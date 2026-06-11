Concurrent RPC build requests finish independently when their targets are done.

  $ make_dune_project 3.23
  $ export DUNE_TRACE=rpc,process
  $ count_trace_events () {
  >   dune trace cat | jq -r "$1" | wc -l
  > }
  $ wait_for_trace_events () {
  >   jq_program="$1"
  >   expected="$2"
  >   dune="$(command -v dune)"
  >   with_timeout bash -lc '
  >     jq_program="$1"
  >     expected="$2"
  >     dune="$3"
  >     while [ "$("$dune" trace cat | jq -r "$jq_program" | wc -l)" -lt "$expected" ]
  >     do
  >       sleep 0.01
  >     done
  >   ' bash "$jq_program" "$expected" "$dune"
  > }

  $ fast_started="$(mktemp -u)"
  $ slow_started="$(mktemp -u)"
  $ fast_release="$(mktemp -u)"
  $ slow_release="$(mktemp -u)"
  $ mkfifo "$fast_release"
  $ mkfifo "$slow_release"
  $ cat > dune <<EOF
  > (rule
  >  (target fast-target)
  >  (action
  >   (bash "touch '$fast_started'; read _ < '$fast_release'; echo fast > fast-target")))
  > (rule
  >  (target slow-target)
  >  (action
  >   (bash "touch '$slow_started'; read _ < '$slow_release'; echo slow > slow-target")))
  > EOF

  $ fast_finished_jq='select(.cat == "process" and .name == "finish" and (.args.target_files // [] | index("_build/default/fast-target"))) | .name'
  $ start_dune -j 2
  $ fast_finishes=$(count_trace_events "$fast_finished_jq")
  $ dune rpc build --wait fast-target > fast.out 2>&1 &
  $ FAST=$!
  $ dune rpc build --wait slow-target > slow.out 2>&1 &
  $ SLOW=$!
  $ with_timeout dune_cmd wait-for-file-to-appear "$fast_started"
  $ with_timeout dune_cmd wait-for-file-to-appear "$slow_started"
  $ if grep -qx Success fast.out 2>/dev/null; then
  >   echo "fast rpc request finished before release"
  > else
  >   echo "fast rpc request is waiting"
  > fi
  fast rpc request is waiting

Let the fast action finish, and wait until dune has observed its process exit.

  $ printf '.\n' > "$fast_release"
  $ wait_for_trace_events "$fast_finished_jq" $((fast_finishes + 1))
  $ with_timeout dune_cmd wait-for-file-to-appear _build/default/fast-target
  $ cat _build/default/fast-target
  fast

The fast RPC build should finish now, before the slow request is done.

  $ if wait_for_success_with_timeout fast.out 200; then
  >   echo "fast rpc request finished"
  > else
  >   echo "fast rpc request timed out"
  > fi
  fast rpc request finished
  $ file_status _build/default/slow-target
  _build/default/slow-target missing

  $ printf '.\n' > "$slow_release"
  $ wait_for_pid_to_exit_with_timeout "$FAST" 200 || (cat fast.out; false)
  $ wait "$FAST"
  $ wait_for_pid_to_exit_with_timeout "$SLOW" 200 || (cat slow.out; false)
  $ wait "$SLOW"
  $ cat fast.out slow.out | sort
  Success
  Success
  $ cat _build/default/fast-target _build/default/slow-target
  fast
  slow
  $ stop_dune_quiet

Concurrent RPC build failures also finish independently.

  $ fail_started="$(mktemp -u)"
  $ fail_release="$(mktemp -u)"
  $ slow_started="$(mktemp -u)"
  $ slow_release="$(mktemp -u)"
  $ mkfifo "$fail_release"
  $ mkfifo "$slow_release"
  $ cat > dune <<EOF
  > (rule
  >  (target fail-target)
  >  (action
  >   (bash "touch '$fail_started'; read _ < '$fail_release'; exit 1")))
  > (rule
  >  (target slow-after-fail)
  >  (action
  >   (bash "touch '$slow_started'; read _ < '$slow_release'; echo slow > slow-after-fail")))
  > EOF

  $ start_dune -j 2
  $ dune rpc build --wait fail-target > fail.out 2>&1 &
  $ FAIL=$!
  $ dune rpc build --wait slow-after-fail > slow-after-fail.out 2>&1 &
  $ SLOW=$!
  $ with_timeout dune_cmd wait-for-file-to-appear "$fail_started"
  $ with_timeout dune_cmd wait-for-file-to-appear "$slow_started"

  $ printf '.\n' > "$fail_release"
  $ if wait_for_line_with_timeout fail.out Failure 200; then
  >   echo "failing rpc request finished"
  > else
  >   echo "failing rpc request timed out"
  > fi
  failing rpc request finished
  $ wait_for_pid_to_exit_with_timeout "$FAIL" 200 || (cat fail.out; false)
  $ wait "$FAIL"
  [1]
  $ file_status _build/default/slow-after-fail
  _build/default/slow-after-fail missing

  $ printf '.\n' > "$slow_release"
  $ wait_for_pid_to_exit_with_timeout "$SLOW" 200 || (cat slow-after-fail.out; false)
  $ wait "$SLOW"
  $ cat fail.out slow-after-fail.out | sort
  Failure
  Success
  $ cat _build/default/slow-after-fail
  slow
  $ stop_dune_quiet

RPC build requests cancelled by --stop-on-first-error complete with the batch
failure.

  $ cancelled="$(mktemp -u)"
  $ fail_started="$(mktemp -u)"
  $ fail_release="$(mktemp -u)"
  $ slow_started="$(mktemp -u)"
  $ mkfifo "$fail_release"
  $ cat > dune <<EOF
  > (rule
  >  (target fail-stop)
  >  (action
  >   (bash "touch '$fail_started'; read _ < '$fail_release'; exit 1")))
  > (rule
  >  (target slow-stop)
  >  (action
  >   (bash "touch '$slow_started'; trap \"touch '$cancelled'; exit 0\" TERM; while true; do sleep 1; done")))
  > EOF

  $ start_dune -j 2 --stop-on-first-error
  $ dune rpc build --wait fail-stop > fail-stop.out 2>&1 &
  $ FAIL=$!
  $ dune rpc build --wait slow-stop > slow-stop.out 2>&1 &
  $ SLOW=$!
  $ with_timeout dune_cmd wait-for-file-to-appear "$fail_started"
  $ with_timeout dune_cmd wait-for-file-to-appear "$slow_started"

  $ printf '.\n' > "$fail_release"
  $ wait_for_pid_to_exit_with_timeout "$FAIL" 200 || (cat fail-stop.out; false)
  $ wait "$FAIL"
  [1]
  $ cat fail-stop.out
  Failure
  $ with_timeout dune_cmd wait-for-file-to-appear "$cancelled"

  $ if wait_for_pid_to_exit_with_timeout "$SLOW" 200; then
  >   echo "slow rpc request finished"
  >   wait "$SLOW" || true
  >   cat slow-stop.out
  > else
  >   echo "slow rpc request timed out"
  >   kill "$SLOW" 2>/dev/null || true
  >   wait "$SLOW" || true
  > fi
  slow rpc request finished
  Failure
  $ stop_dune_quiet

Promotions registered by an independently-finished request are available to RPC
promotion requests while the rest of the batch is still running, and are not
resurrected when that batch finishes.

  $ cat > source <<EOF
  > old
  > EOF
  $ slow_started="$(mktemp -u)"
  $ slow_release="$(mktemp -u)"
  $ cat > dune <<EOF
  > (rule
  >  (target source.corrected)
  >  (action (write-file %{target} "new\n")))
  > (rule
  >  (alias repro)
  >  (action (diff source source.corrected)))
  > (rule
  >  (target slow-with-promotion)
  >  (action
  >   (bash "\| touch '$slow_started'
  >         "\| while [ ! -e '$slow_release' ]; do
  >         "\|   sleep 0.01
  >         "\| done
  >         "\| echo slow > slow-with-promotion
  > )))
  > EOF

  $ start_dune -j 2
  $ dune rpc build --wait '(alias repro)' > promotion-build.out 2>&1 &
  $ PROMOTION_BUILD=$!
  $ dune rpc build --wait slow-with-promotion > slow-with-promotion.out 2>&1 &
  $ SLOW=$!
  $ with_timeout dune_cmd wait-for-file-to-appear "$slow_started"
  $ if wait_for_line_with_timeout promotion-build.out Failure 200; then
  >   echo "promotion rpc request finished"
  > else
  >   echo "promotion rpc request timed out"
  > fi
  promotion rpc request finished
  $ dune promote source
  Success
  $ cat source
  new

  $ touch "$slow_release"
  $ wait_for_pid_to_exit_with_timeout "$PROMOTION_BUILD" 200 || (cat promotion-build.out; false)
  $ wait "$PROMOTION_BUILD"
  [1]
  $ wait_for_pid_to_exit_with_timeout "$SLOW" 200 || (cat slow-with-promotion.out; false)
  $ wait "$SLOW"
  $ cat promotion-build.out slow-with-promotion.out | sort
  Failure
  Success
  $ dune promotion list source 2>&1
  Warning: Nothing to promote for source.
  $ stop_dune_quiet
