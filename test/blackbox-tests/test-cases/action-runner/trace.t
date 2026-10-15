`--action-runner` only dispatches actions that run processes. Trace events
emitted inside the worker are stamped with the action runner name, and process
events routed through the worker record the worker name and pid.

  $ make_dune_project 3.26
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
  >  (job_slots (echo 3))
  >  (action (bash "cat input > %{target}")))
  > EOF

  $ dune build -j 4 pure proc
  $ dune trace cat | jq -s 'include "dune"; actionRunnerTraceEvents("action-runner")'
  []

The process start event records the granted job slots with and without the
worker:

  $ job_slots () {
  >   dune trace cat | jq '
  >     select(.cat == "process" and .name == "start") | .args
  >     | select(.target_files == ["_build/default/proc"]) | .job_slots'
  > }
  $ job_slots
  3

  $ echo two > input
  $ dune build -j 4 --action-runner pure proc
  $ job_slots
  3
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
