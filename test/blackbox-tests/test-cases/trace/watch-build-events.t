Batch and watch-mode builds emit run ids on build trace events.

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

  $ dune trace cat | jq -s '
  > [ .[]
  >   | select(
  >       .cat == "build"
  >       and (.name == "build-start" or .name == "build-restart" or .name == "build-finish")
  >     )
  >   | { args, name }
  >   | if .args.restart_duration? != null
  >     then .args.restart_duration |= type
  >     else .
  >     end
  > ] | .[]'
  {
    "args": {
      "run_id": 0,
      "restart": false
    },
    "name": "build-start"
  }
  {
    "args": {
      "run_id": 0,
      "outcome": "success"
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

  $ dune trace cat | jq -s '
  > [ .[]
  >   | select(
  >       .cat == "build"
  >       and (.name == "build-start" or .name == "build-restart" or .name == "build-finish")
  >     )
  >   | { args, name }
  >   | if .args.restart_duration? != null
  >     then .args.restart_duration |= type
  >     else .
  >     end
  > ] | .[]'
  {
    "args": {
      "run_id": 1,
      "restart": false
    },
    "name": "build-start"
  }
  {
    "args": {
      "run_id": 1,
      "outcome": "success"
    },
    "name": "build-finish"
  }
  {
    "args": {
      "run_id": 2,
      "reasons": [
        "x changed"
      ]
    },
    "name": "build-restart"
  }
  {
    "args": {
      "run_id": 2,
      "reasons": [
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
      ]
    },
    "name": "build-start"
  }
  {
    "args": {
      "run_id": 2,
      "outcome": "success",
      "restart_duration": "number"
    },
    "name": "build-finish"
  }
