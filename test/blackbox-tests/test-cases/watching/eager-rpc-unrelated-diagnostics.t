A failed RPC build receives the eager build's complete diagnostic set, including
an error from an unrelated sticky goal.

  $ make_dune_project 3.25

  $ server_output="$TMPDIR/eager-rpc-unrelated-diagnostics-output"
  $ client_output="$TMPDIR/eager-rpc-unrelated-diagnostics-client-output"
  $ cat > dune <<'EOF'
  > (rule
  >  (alias sticky)
  >  (action (bash "echo sticky error >&2; exit 1")))
  > (rule
  >  (alias rpc)
  >  (action (bash "echo rpc error >&2; exit 1")))
  > EOF

Start the eager watcher and wait until its sticky goal has failed before sending
an unrelated failing RPC build.

  $ ( (dune build @sticky --watch >"$server_output" 2>&1) \
  >   || (echo exit $? >>"$server_output") ) &
  $ DUNE_PID=$!
  $ wait_for_rpc_server
  $ wait_for_line_with_timeout "$server_output" "sticky error" 1000

  $ dune build @rpc >"$client_output" 2>&1
  [1]
  $ stop_dune_quiet

Both the requested goal's error and the sticky goal's error are returned to the
client.

  $ grep -E '^(sticky error|rpc error)$' "$client_output" | sort
  rpc error
  sticky error
  $ grep '^Error: Build failed' "$client_output"
  Error: Build failed with 2 errors.

Use a passive watcher for a stable snapshot of both goals. An eager watcher's
sticky rebuild can replace the RPC goal's diagnostics after it completes.
Reading the passive watcher's snapshot must not consume its errors.

Without a server, the monitor waits for a connection. Once the passive server
starts, use quit-on-disconnect for a deterministic end to the monitored build.
Match status text, not terminal escape sequences or redraw counts.

  $ monitor_output="$TMPDIR/diagnostic-monitor-output"
  $ wait_for_monitor () {
  >   for _ in $(seq 1 1000); do
  >     if grep -a -q "$1" "$monitor_output"; then return; fi
  >     sleep 0.01
  >   done
  >   tr '\r' '\n' < "$monitor_output"
  >   return 1
  > }
  $ INSIDE_EMACS=1 DUNE_CONFIG__THREADED_CONSOLE=disabled \
  >   dune monitor --display progress >"$monitor_output" 2>&1 &
  $ MONITOR_PID=$!
  $ wait_for_monitor 'Waiting for RPC server'
  $ kill "$MONITOR_PID"
  $ wait "$MONITOR_PID" 2>/dev/null || true
  $ start_dune
  $ INSIDE_EMACS=1 DUNE_CONFIG__THREADED_CONSOLE=disabled \
  >   dune monitor --display progress --quit-on-disconnect \
  >   >"$monitor_output" 2>&1 &
  $ MONITOR_PID=$!
  $ wait_for_monitor 'Initializing'
  $ dune build @sticky @rpc >both-output 2>&1
  [1]
  $ dune diagnostics >snapshot 2>&1
  $ cat snapshot
  File "$TESTCASE_ROOT/dune", lines 1-3, characters 0-71:
  1 | (rule
  2 |  (alias sticky)
  3 |  (action (bash "echo sticky error >&2; exit 1")))
  sticky error
  File "$TESTCASE_ROOT/dune", lines 4-6, characters 0-65:
  4 | (rule
  5 |  (alias rpc)
  6 |  (action (bash "echo rpc error >&2; exit 1")))
  rpc error
  $ dune diagnostics >repeated-snapshot 2>&1
  $ diff -u snapshot repeated-snapshot
  $ wait_for_monitor 'Had 2 errors'

Fixing both goals removes their diagnostics. An empty snapshot prints nothing.

  $ cat > dune <<'EOF'
  > (rule
  >  (alias sticky)
  >  (action (echo "sticky fixed")))
  > (rule
  >  (alias rpc)
  >  (action (echo "rpc fixed")))
  > EOF
  $ dune build @sticky @rpc >fixed-output 2>&1
  $ dune diagnostics
  $ wait_for_monitor 'Success, waiting for filesystem changes'
  $ stop_dune_quiet
  $ wait_for_monitor 'Lost connection to server'
  $ wait "$MONITOR_PID"
  [1]
