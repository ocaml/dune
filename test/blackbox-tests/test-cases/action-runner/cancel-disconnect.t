If the worker dies while the main process is waiting for `action/cancel-build`,
the build still fails promptly instead of waiting forever for that RPC response.

  $ make_dune_project 3.23
  $ export TEST_DIR=$PWD
  $ export DUNE_TRACE=action
  $ cat > dune <<'EOF'
  > (rule
  >  (target slow)
  >  (action
  >   (bash "trap '' TERM; touch \"$TEST_DIR/slow-started\"; sleep 30; echo done > %{target}")))
  > 
  > (rule
  >  (target fail)
  >  (action
  >   (bash "while [ ! -f \"$TEST_DIR/slow-started\" ]; do sleep 0.01; done; touch \"$TEST_DIR/fail-started\"; echo failing-from-runner >&2; exit 1")))
  > EOF

The failure triggers `--stop-on-first-error`. Kill the worker after the main
process has sent `action/cancel-build` and before the worker can finish draining
that cancellation request.

  $ DUNE_JOBS=2 $timeout 10 dune build --action-runner --stop-on-first-error \
  >   slow fail > /dev/null 2>&1 &
  $ BUILD_PID=$!
  $ wait_for_runner_event_count runner-cancel-request-sent 1
  $ RUNNER_PID=$(wait_for_runner_spawn_pid)
  $ kill -9 "$RUNNER_PID"
  $ wait_for_runner_event_count runner-disconnected 1
  $ wait $BUILD_PID; [ "$?" = 1 ]

  $ file_status slow-started
  slow-started exists

  $ file_status fail-started
  fail-started exists
