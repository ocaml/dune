`--action-runner` only dispatches actions that run processes. Trace events
emitted inside the worker are stamped with the action runner name, and process
events routed through the worker record the worker name and pid.

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
  $ dune trace cat | jq -s 'include "dune"; actionRunnerTraceEvents("action-runner")'
  []

  $ echo two > input
  $ dune build --action-runner pure proc
  $ dune trace cat | jq -s 'include "dune"; actionRunnerTraceEvents("action-runner")'
  [
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
      "cat": "process",
      "name": "start",
      "args": {
        "action_runner": "action-runner",
        "action_runner_pid": "number",
        "prog": "bash"
      }
    },
    {
      "cat": "process",
      "name": "finish",
      "args": {
        "action_runner": "action-runner",
        "action_runner_pid": "number",
        "prog": "bash",
        "exit": 0
      }
    }
  ]
