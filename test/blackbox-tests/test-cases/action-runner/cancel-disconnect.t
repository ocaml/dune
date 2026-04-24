Known limitation: if the worker dies while the main process is sending
`action/cancel-build`, the build waits forever for that RPC response.

  $ command -v setsid >/dev/null
  $ make_dune_project 3.23
  $ export TEST_DIR=$PWD
  $ cat > dune <<'EOF'
  > (rule
  >  (target slow)
  >  (action
  >   (bash "touch \"$TEST_DIR/slow-started\"; sleep 0.5; echo done > %{target}")))
  > 
  > (rule
  >  (target fail)
  >  (action
  >   (bash "touch \"$TEST_DIR/fail-started\"; WORKER_PID=$PPID setsid sh -c 'sleep 0.1; kill -9 \"$WORKER_PID\"' >/dev/null 2>&1 < /dev/null & echo failing-from-runner >&2; exit 1")))
  > EOF

The failure triggers `--stop-on-first-error`, and the detached helper kills the
worker just after that, while the parent is still waiting for the cancel RPC to
complete.

  $ DUNE_JOBS=2 $timeout 2 dune build --action-runner --stop-on-first-error slow fail \
  > > /dev/null 2>&1
  [124]

  $ file_status slow-started
  slow-started exists

  $ file_status fail-started
  fail-started exists

