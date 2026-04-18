`dune runtest --action-runner` routes test actions through the worker too.

  $ make_dune_project 3.23
  $ export DUNE_TRACE=action
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
    "exec_start": 1,
    "cancel_request_sent": 0,
    "cancel_start": 0,
    "disconnected": 1,
    "request_sent": true,
    "names": [
      "action-runner"
    ],
    "spawn_pid_types": [
      "number"
    ]
  }

Cram test commands are user-controlled and should be routed through the runner.

  $ cat > dune <<'EOF'
  > (cram)
  > EOF
  $ cat > sample.t <<'EOF'
  >   $ echo cram-through-runner
  >   cram-through-runner
  > EOF

  $ dune runtest --action-runner

  $ dune trace cat | jq -s 'include "dune"; runnerEventSummary'
  {
    "spawn": 1,
    "connection_start": 1,
    "connection_established": 1,
    "connected": 1,
    "exec_start": 1,
    "cancel_request_sent": 0,
    "cancel_start": 0,
    "disconnected": 1,
    "request_sent": true,
    "names": [
      "action-runner"
    ],
    "spawn_pid_types": [
      "number"
    ]
  }
