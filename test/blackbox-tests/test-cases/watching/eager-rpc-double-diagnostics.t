An eager watcher evaluates both its sticky goal and an incoming RPC goal. When
both observe the same cached failure, the forwarded build prints the diagnostic
twice.

  $ make_dune_project 3.25

  $ server_output="$TMPDIR/eager-rpc-double-diagnostics-output"
  $ client_output="$TMPDIR/eager-rpc-double-diagnostics-client-output"
  $ cat > dune <<'EOF'
  > (rule
  >  (alias fail)
  >  (action (bash "echo failed action >&2; exit 1")))
  > EOF

Start the eager watcher with the failing alias and wait for its initial
diagnostic before sending the forwarded build request.

  $ ( (dune build @fail --watch >"$server_output" 2>&1) \
  >   || (echo exit $? >>"$server_output") ) &
  $ DUNE_PID=$!
  $ wait_for_rpc_server
  $ wait_for_line_with_timeout "$server_output" "failed action" 1000

  $ dune build @fail >"$client_output" 2>&1
  [1]
  $ stop_dune_quiet

Show the diagnostics returned to the forwarded build.

  $ cat "$client_output"
  File "dune", lines 1-3, characters 0-70:
  1 | (rule
  2 |  (alias fail)
  3 |  (action (bash "echo failed action >&2; exit 1")))
  failed action
  File "dune", lines 1-3, characters 0-70:
  1 | (rule
  2 |  (alias fail)
  3 |  (action (bash "echo failed action >&2; exit 1")))
  failed action
  Error: Build failed with 2 errors.
