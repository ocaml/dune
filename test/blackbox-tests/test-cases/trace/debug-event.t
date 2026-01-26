The debug event can be triggered with Sigusr1

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > EOF

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

  $ kill -s sigusr2 -- $pid

  $ dune trace cat | jq 'select(.name == "debug") | .args | .scheduler.process_watcher |= keys'
  {
    "scheduler": {
      "events": {
        "pending_jobs": 1,
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
