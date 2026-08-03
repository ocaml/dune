If the worker dies while the main process is waiting for `action/cancel-build`,
the build still fails promptly instead of waiting forever for that RPC response.

  $ make_dune_project 3.23
  $ export TEST_DIR=$PWD
  $ export DUNE_TRACE=action
  $ cat > kill-worker-on-term.sh <<'EOF'
  > #!/bin/sh
  > trap 'touch "$TEST_DIR/worker-kill-requested"; kill -9 "$PPID" 2>/dev/null || true; exit 0' TERM
  > touch "$TEST_DIR/slow-started"
  > while :; do :; done
  > EOF
  $ chmod +x kill-worker-on-term.sh
  $ cat > dune <<'EOF'
  > (rule
  >  (target slow)
  >  (deps ./kill-worker-on-term.sh)
  >  (action
  >   (run ./kill-worker-on-term.sh)))
  > 
  > (rule
  >  (target fail)
  >  (action
  >   (bash "while [ ! -f \"$TEST_DIR/slow-started\" ]; do sleep 0.01; done; touch \"$TEST_DIR/fail-started\"; echo failing-from-runner >&2; exit 1")))
  > EOF

The failure triggers `--stop-on-first-error`. The slow action kills its parent
worker from its `TERM` trap, so the worker dies while processing
`action/cancel-build` instead of racing an external kill after a trace event.

  $ DUNE_JOBS=2 $timeout 10 dune build --action-runner --stop-on-first-error \
  >   slow fail > /dev/null 2>&1 &
  $ BUILD_PID=$!
  $ wait_for_runner_event_count runner-cancel-request-sent 1
  $ wait_for_runner_event_count runner-disconnected 1
  $ wait $BUILD_PID; [ "$?" = 1 ]

  $ file_status slow-started
  slow-started exists

  $ file_status fail-started
  fail-started exists

  $ file_status worker-kill-requested
  worker-kill-requested exists
