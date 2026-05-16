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

  $ dune trace cat | jq -s 'include "dune"; runnerLifecycleSummary'
  {
    "spawn": 1,
    "connection_start": 1,
    "connection_established": 1,
    "connected": 1,
    "exec_start": 1,
    "cancel_request_sent": 0,
    "cancel_start": 0,
    "disconnected": 1,
    "names": [
      "action-runner"
    ],
    "spawn_pid_types": [
      "number"
    ]
  }
