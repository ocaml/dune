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
