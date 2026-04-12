Watch-mode builds emit start, restart, and finish trace events.

  $ make_dune_project 3.22

  $ cat >x <<EOF
  > original
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (target y)
  >  (deps x)
  >  (action (system "cat x > y")))
  > EOF

  $ start_dune

  $ build y
  Success

  $ echo updated > x

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
      "restart": false
    },
    "name": "build-start"
  }
  {
    "args": {
      "outcome": "success"
    },
    "name": "build-finish"
  }
  {
    "args": {
      "reasons": [
        "x changed"
      ]
    },
    "name": "build-restart"
  }
  {
    "args": {
      "restart": true
    },
    "name": "build-start"
  }
  {
    "args": {
      "outcome": "success",
      "restart_duration": "number"
    },
    "name": "build-finish"
  }
