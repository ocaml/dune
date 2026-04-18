`dune runtest --action-runner` routes test actions through the worker too.

  $ make_dune_project 3.23
  $ cat > dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (action (bash "echo runtest-through-runner")))
  > EOF

  $ dune runtest --action-runner
  runtest-through-runner

  $ dune trace cat | jq -s 'include "dune"; runnerEventSummary'
  {
    "spawn": 1,
    "connection_start": 1,
    "connection_established": 1,
    "connected": 1,
    "request_sent": true,
    "names": [
      "action-runner"
    ],
    "spawn_pid_types": [
      "number"
    ]
  }

