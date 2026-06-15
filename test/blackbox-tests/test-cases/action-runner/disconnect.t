A worker disconnect in the middle of an exec request should fail the build
promptly.

  $ make_dune_project 3.23
  $ export DUNE_TRACE=action
  $ cat > dune <<'EOF'
  > (rule
  >  (target break)
  >  (action (bash "sleep 30")))
  > EOF

Start the build, wait until the worker has entered the exec handler, then kill
the worker from the test harness.

  $ DUNE_JOBS=1 $timeout 10 dune build --action-runner break > /dev/null 2>&1 &
  $ BUILD_PID=$!
  $ wait_for_runner_event_count runner-exec-start 1
  $ RUNNER_PID=$(wait_for_runner_spawn_pid)
  $ kill -9 "$RUNNER_PID"
  $ wait_for_runner_event_count runner-disconnected 1
  $ wait $BUILD_PID; [ "$?" = 1 ]

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
      "name": "runner-exec-start",
      "args": {
        "name": "action-runner",
        "action_runner": "action-runner"
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
