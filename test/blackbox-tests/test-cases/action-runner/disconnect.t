A worker disconnect in the middle of an exec request should fail the build
promptly.

  $ make_dune_project 3.23
  $ export DUNE_TRACE=action
  $ export TEST_DIR=$PWD
  $ cat > dune <<'EOF'
  > (rule
  >  (target break)
  >  (action
  >   (bash "touch \"$TEST_DIR/break-started\"; while [ ! -f \"$TEST_DIR/break-finish\" ]; do sleep 0.01; done")))
  > EOF

Start the build, wait until the worker has started the action, then kill the
worker from the test harness.

  $ DUNE_JOBS=1 $timeout 10 dune build --action-runner break > /dev/null 2>&1 &
  $ BUILD_PID=$!
  $ RUNNER_PID=$(wait_for_runner_spawn_pid)
  $ wait_for_file break-started
  $ kill -9 "$RUNNER_PID"
  $ touch break-finish
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
      "name": "runner-disconnected",
      "args": {
        "name": "action-runner"
      }
    }
  ]
