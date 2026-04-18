`--action-runner` only dispatches actions that run processes, and trace events
emitted inside the worker are stamped with the action runner name.

  $ make_dune_project 3.23
  $ export DUNE_TRACE=action,process
  $ echo one > input
  $ cat > dune <<'EOF'
  > (rule
  >  (target pure)
  >  (deps input)
  >  (action (copy input %{target})))
  > 
  > (rule
  >  (target proc)
  >  (deps input)
  >  (action (bash "cat input > %{target}")))
  > EOF

  $ dune build pure proc
  $ dune trace cat | jq -s 'include "dune"; actionRunnerTraceSummary("action-runner")'
  {
    "runner_request_sent": 0,
    "worker_action_events": [],
    "worker_process_events": [],
    "worker_names": []
  }

  $ echo two > input
  $ dune build --action-runner pure proc
  $ dune trace cat | jq -s 'include "dune"; actionRunnerTraceSummary("action-runner")'
  {
    "runner_request_sent": 1,
    "worker_action_events": [
      "runner-connection-established",
      "runner-connection-start",
      "runner-exec-start"
    ],
    "worker_process_events": [
      "finish",
      "start"
    ],
    "worker_names": [
      "action-runner"
    ]
  }
