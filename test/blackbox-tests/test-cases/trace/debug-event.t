The debug event can be triggered with Sigusr1

  $ debug_events_jq='select(.name == "debug") | .name'
  $ build_request_starts_jq='select(.cat == "rpc" and .name == "request" and .args.meth == "build" and .args.stage == "start") | .name'
  $ count_trace_events () {
  >   dune trace cat | jq -r "$1" | wc -l
  > }
  $ wait_for_trace_events () {
  >   jq_program="$1"
  >   expected="$2"
  >   with_timeout bash -lc '
  >     jq_program="$1"
  >     expected="$2"
  >     while [ "$(dune trace cat | jq -r "$jq_program" | wc -l)" -lt "$expected" ]
  >     do
  >       sleep 0.1
  >     done
  >   ' bash "$jq_program" "$expected"
  > }

  $ make_dune_project 3.22

  $ ready=$(mktemp -d)/ready
  $ mkfifo $ready

  $ cat >dune << EOF
  > (rule
  >  (alias foo)
  >  (action (bash "echo done > $ready; sleep 5")))
  > EOF

  $ dune build @foo &

  $ read line < $ready

  $ pid=$(dune trace cat | jq '.args.pid' -r | head -1)
  $ debug_events=$(count_trace_events "$debug_events_jq")

  $ kill -s sigusr2 -- $pid
  $ wait_for_trace_events "$debug_events_jq" $((debug_events + 1))

  $ dune trace cat | jq -s '
  >   [ .[] | select(.name == "debug") ]
  > | last
  > | .args
  > | { scheduler: .scheduler }
  > | .scheduler.process_watcher |= keys
  > | .scheduler.events.pending_jobs |= type
  > '
  {
    "scheduler": {
      "events": {
        "pending_jobs": "number",
        "pending_worker_tasks": 0
      },
      "process_watcher": [
        "running_count",
        "table"
      ]
    }
  }

  $ kill $pid

  $ wait

The debug event also includes active RPC connections and in-flight requests.

  $ make_dune_project 3.23

  $ ready=$(mktemp -d)/ready
  $ mkfifo $ready

  $ cat >dune << EOF
  > (rule
  >  (target x)
  >  (action (bash "echo done > $ready; sleep 5")))
  > EOF

  $ export DUNE_TRACE=rpc,diagnostics,config
  $ start_dune

  $ build_request_starts=$(count_trace_events "$build_request_starts_jq")
  $ output1=$(mktemp)
  $ output2=$(mktemp)
  $ dune rpc build --wait x >"$output1" 2>&1 &
  $ build1_pid=$!
  $ dune rpc build --wait x >"$output2" 2>&1 &
  $ build2_pid=$!

Wait for both RPC requests to be accepted before dumping state.

  $ wait_for_trace_events "$build_request_starts_jq" $((build_request_starts + 2))

  $ read line < $ready

  $ pid=$(dune trace cat | jq '.args.pid' -r | head -1)
  $ debug_events=$(count_trace_events "$debug_events_jq")

  $ kill -s sigusr2 -- $pid
  $ wait_for_trace_events "$debug_events_jq" $((debug_events + 1))

  $ dune trace cat | jq -s '
  >   [ .[] | select(.name == "debug") ]
  > | last
  > | .args.rpc
  > | { connections:
  >       ([.connections[] | .requests_in_flight | map(.call.method_)] | sort)
  >   }
  > '
  {
    "connections": [
      [
        "build"
      ],
      [
        "build"
      ]
    ]
  }

  $ stop_dune_quiet
  $ wait "$build1_pid" || true
  $ wait "$build2_pid" || true
