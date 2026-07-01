When dune is interrupted (e.g., Ctrl-C), child processes running as build
actions should receive SIGTERM before SIGKILL so they can run cleanup handlers.

See https://github.com/ocaml/dune/issues/2445

  $ make_dune_project 3.18

  $ cat > dune <<EOF
  > (rule
  >  (alias slow)
  >  (action (run dune_cmd sigterm-cleanup-sleeper)))
  > EOF

The helper blocks SIGINT (so it isn't killed by the signal meant for dune),
installs a SIGTERM handler to write a marker file, signals readiness, and
sleeps.

A wrapper script that runs dune build in the background, waits for the child to
be ready, sends SIGINT to dune, and checks if the cleanup handler ran.

  $ export TEST_DIR=$PWD

  $ dune build @slow 2>/dev/null &

  $ DUNE_PID=$!

  $ wait_for_file ready

  $ kill -INT $DUNE_PID

  $ wait_for_file cleanup_ran

  $ wait $DUNE_PID
  [130]

  $ dune trace cat | jq 'select(.name | startswith("process-")) | { name, args }'
  {
    "name": "process-cleanup-start",
    "args": {}
  }
  {
    "name": "process-cleanup-sigkill",
    "args": {}
  }
  {
    "name": "process-cleanup-finish",
    "args": {}
  }
