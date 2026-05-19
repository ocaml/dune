Batch and watch-mode builds emit run ids and dune's own rusage snapshots on
build trace events.

  $ make_dune_project 3.22

  $ cat >x <<EOF
  > original
  > EOF

  $ cat >z <<EOF
  > first
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (target y)
  >  (deps x z)
  >  (action (system "cat x z > y")))
  > EOF

  $ dune build y

  $ dune trace cat | jq -s 'include "dune"; .[] | buildEvents'
  {
    "args": {
      "run_id": 0,
      "restart": false,
      "rusage": [
        "inblock",
        "majflt",
        "maxrss",
        "minflt",
        "nivcsw",
        "nvcsw",
        "oublock",
        "system_cpu_time",
        "user_cpu_time"
      ]
    },
    "name": "build-start"
  }
  {
    "args": {
      "run_id": 0,
      "outcome": "success",
      "rusage": [
        "inblock",
        "majflt",
        "maxrss",
        "minflt",
        "nivcsw",
        "nvcsw",
        "oublock",
        "system_cpu_time",
        "user_cpu_time"
      ]
    },
    "name": "build-finish"
  }

  $ start_dune

  $ build y
  Success

  $ echo updated > x
  $ echo second > z

  $ build y
  Success

  $ stop_dune > /dev/null

File watcher backends may batch file changes differently. Normalize contiguous
restart events so the test checks the run id and reasons, not the batching.

  $ dune trace cat | jq -s '
  > include "dune";
  > [ .[] | buildEvents ] | normalizeBuildRestartEvents'
  {
    "args": {
      "run_id": 1,
      "restart": false,
      "rusage": [
        "inblock",
        "majflt",
        "maxrss",
        "minflt",
        "nivcsw",
        "nvcsw",
        "oublock",
        "system_cpu_time",
        "user_cpu_time"
      ]
    },
    "name": "build-start"
  }
  {
    "args": {
      "run_id": 1,
      "outcome": "success",
      "rusage": [
        "inblock",
        "majflt",
        "maxrss",
        "minflt",
        "nivcsw",
        "nvcsw",
        "oublock",
        "system_cpu_time",
        "user_cpu_time"
      ]
    },
    "name": "build-finish"
  }
  {
    "args": {
      "run_id": 2,
      "reasons": [
        "x changed",
        "z changed"
      ]
    },
    "name": "build-restart"
  }
  {
    "args": {
      "run_id": 2,
      "restart": true,
      "files": [
        "x",
        "z"
      ],
      "rusage": [
        "inblock",
        "majflt",
        "maxrss",
        "minflt",
        "nivcsw",
        "nvcsw",
        "oublock",
        "system_cpu_time",
        "user_cpu_time"
      ]
    },
    "name": "build-start"
  }
  {
    "args": {
      "run_id": 2,
      "outcome": "success",
      "restart_duration": "number",
      "rusage": [
        "inblock",
        "majflt",
        "maxrss",
        "minflt",
        "nivcsw",
        "nvcsw",
        "oublock",
        "system_cpu_time",
        "user_cpu_time"
      ]
    },
    "name": "build-finish"
  }

  $ dune trace cat | jq -s '
  > last
  > | select(.name == "exit")
  > | { name, cat, rusage: (.args.rusage | keys) }'
  {
    "name": "exit",
    "cat": "config",
    "rusage": [
      "inblock",
      "majflt",
      "maxrss",
      "minflt",
      "nivcsw",
      "nvcsw",
      "oublock",
      "system_cpu_time",
      "user_cpu_time"
    ]
  }
