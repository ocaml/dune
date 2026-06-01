Interrupted watch builds emit a restart event before the interrupted iteration's
build-finish event.

  $ make_dune_project 3.22

  $ cat > x <<EOF
  > original
  > EOF

  $ cat > build.sh <<'EOF'
  > if [ "$(cat "$1")" = unstable ]; then
  >   touch "$3"
  >   sleep 1000
  >   exit 1
  > else
  >   cat "$1" > "$2"
  > fi
  > EOF

Keep the synchronization marker outside the workspace so it does not enqueue its
own source invalidation.

  $ marker="$TMPDIR/go-ahead"
  $ cat > dune <<EOF
  > (rule
  >  (target y)
  >  (deps x build.sh)
  >  (action (run bash %{dep:build.sh} %{dep:x} y "$marker")))
  > EOF

  $ rm -f "$marker"
  $ start_dune @idle

  $ build y
  Success
  $ cat _build/default/y
  original

  $ echo unstable > x
  $ (dune_cmd wait-for-file-to-appear "$marker"; echo updated > x) & build y
  Success
  $ cat _build/default/y
  updated

  $ stop_dune > /dev/null

  $ dune trace cat | jq_dune -s '
  > [ .[] | buildEvents | del(.args.rusage) ] | .[-4:]'
  [
    {
      "args": {
        "run_id": 2,
        "restart": true
      },
      "name": "build-start"
    },
    {
      "args": {
        "run_id": 3,
        "reasons": [
          "x changed"
        ]
      },
      "name": "build-restart"
    },
    {
      "args": {
        "run_id": 3,
        "restart": true
      },
      "name": "build-start"
    },
    {
      "args": {
        "run_id": 3,
        "outcome": "success",
        "restart_duration": "number"
      },
      "name": "build-finish"
    }
  ]
