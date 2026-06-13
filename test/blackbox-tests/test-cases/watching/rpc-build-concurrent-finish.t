Concurrent RPC build requests currently finish together even if one target is done.

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
  $ wait_for_success_with_timeout () {
  >   output="$1"
  >   iterations="$2"
  >   while ! grep -qx Success "$output" 2>/dev/null
  >   do
  >     if [ "$iterations" = 0 ]; then return 124; fi
  >     iterations=$((iterations - 1))
  >     sleep 0.01
  >   done
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

Let the fast action finish, and wait until dune has observed its process exit.

  $ printf '.\n' > "$fast_release"
  $ wait_for_trace_events "$fast_finished_jq" $((fast_finishes + 1))
  $ with_timeout dune_cmd wait-for-file-to-appear _build/default/fast-target
  $ cat _build/default/fast-target
  fast

The fast RPC build should finish now, but it remains blocked until the slow
request is also done.

  $ if wait_for_success_with_timeout fast.out 200; then
  >   echo "fast rpc request finished"
  > else
  >   echo "fast rpc request timed out"
  > fi
  fast rpc request timed out
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
