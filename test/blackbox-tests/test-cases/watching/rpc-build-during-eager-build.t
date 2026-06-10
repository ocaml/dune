RPC build requests sent while the eager loop is building cancel the current
iteration and restart it with the RPC request incorporated.

  $ make_dune_project 3.18

  $ marker_dir="$(mktemp -d)"
  $ marker="$marker_dir/eager-build-started"
  $ cat > dune <<EOF
  > (rule
  >  (alias default)
  >  (target eager.txt)
  >  (action (bash "touch '$marker'; sleep 1; echo eager > eager.txt")))
  > (rule
  >  (target rpc.txt)
  >  (action (with-stdout-to rpc.txt (echo rpc))))
  > EOF

  $ dune build @default --watch > .#dune-output 2>&1 &
  $ DUNE_PID=$!
  $ wait_for_rpc_server
  $ wait_for_file "$marker"
  $ with_timeout dune rpc build --wait rpc.txt
  Success

The eager loop still reacts to filesystem changes after the RPC build has run.

  $ echo '(rule (alias default) (action (echo changed)))' > dune
  $ with_timeout dune rpc flush-file-watcher --wait
  $ stop_dune > /dev/null

  $ dune trace cat | jq_dune -s '
  > [ [ .[] | buildEvents ] | normalizeBuildRestartEvents ]
  > | map(select(
  >     (.name == "build-start" and .args.restart == true)
  >     or (.name == "build-finish" and .args.outcome == "success")))
  > | .[-2:]
  > | map({ name, restart: .args.restart, outcome: .args.outcome }
  >       | del(.. | nulls))'
  [
    {
      "name": "build-start",
      "restart": true
    },
    {
      "name": "build-finish",
      "outcome": "success"
    }
  ]

The sticky goal is not rebuilt when nothing changes (idle optimization).

  $ rm -rf _build
  $ make_dune_project 3.18
  $ marker="$(mktemp -u)"
  $ cat > dune <<EOF
  > (rule
  >  (alias default)
  >  (action (bash "touch '$marker'; echo idle > idle.txt")))
  > EOF

  $ dune build @default --watch > .#dune-output 2>&1 &
  $ DUNE_PID=$!
  $ wait_for_rpc_server
  $ wait_for_file "$marker"
  $ with_timeout dune rpc flush-file-watcher --wait
  $ stop_dune > /dev/null

  $ dune trace cat | jq_dune -s '
  > [ .[] | buildEvents | select(.name == "build-start") ] | length'
  1
