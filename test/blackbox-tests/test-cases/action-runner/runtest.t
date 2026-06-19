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

  $ dune trace cat | jq -s 'include "dune"; runnerEvents'
  [
    {
      "cat": "action",
      "name": "runner-spawn",
      "args": {
        "name": "action-runner",
        "pid": "number"
      }
    },
    {
      "cat": "action",
      "name": "runner-connection-start",
      "args": {
        "name": "action-runner",
        "action_runner": "action-runner"
      }
    },
    {
      "cat": "action",
      "name": "runner-connection-established",
      "args": {
        "name": "action-runner",
        "action_runner": "action-runner"
      }
    },
    {
      "cat": "action",
      "name": "runner-connected",
      "args": {
        "name": "action-runner"
      }
    },
    {
      "cat": "action",
      "name": "runner-request-sent",
      "args": {
        "name": "action-runner"
      }
    },
    {
      "cat": "action",
      "name": "runner-disconnected",
      "args": {
        "name": "action-runner"
      }
    }
  ]

Cram test commands are user-controlled and should be routed through the runner.

  $ cat > dune <<'EOF'
  > (cram)
  > EOF
  $ cat > sample.t <<'EOF'
  >   $ echo cram-through-runner
  >   cram-through-runner
  > EOF

  $ dune runtest --action-runner

  $ dune trace cat | jq -s 'include "dune"; runnerEvents'
  [
    {
      "cat": "action",
      "name": "runner-spawn",
      "args": {
        "name": "action-runner",
        "pid": "number"
      }
    },
    {
      "cat": "action",
      "name": "runner-connection-start",
      "args": {
        "name": "action-runner",
        "action_runner": "action-runner"
      }
    },
    {
      "cat": "action",
      "name": "runner-connection-established",
      "args": {
        "name": "action-runner",
        "action_runner": "action-runner"
      }
    },
    {
      "cat": "action",
      "name": "runner-connected",
      "args": {
        "name": "action-runner"
      }
    },
    {
      "cat": "action",
      "name": "runner-request-sent",
      "args": {
        "name": "action-runner"
      }
    },
    {
      "cat": "action",
      "name": "runner-disconnected",
      "args": {
        "name": "action-runner"
      }
    }
  ]
