RPC build requests sent while the eager loop is building are serialized with
that eager iteration instead of entering the build loop concurrently.

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > EOF

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

  $ dune trace cat | jq -s '
  > include "dune";
  > [ [ .[] | buildEvents ] | normalizeBuildRestartEvents ]
  > | map(select(
  >     (.name == "build-restart" and (.args.reasons | index("dune changed")))
  >     or (.name == "build-start" and .args.restart == true)
  >     or (.name == "build-finish" and .args.outcome == "success")))
  > | .[-3:]
  > | map({ name, restart: .args.restart, outcome: .args.outcome, reasons: .args.reasons }
  >       | del(.. | nulls))'
  [
    {
      "name": "build-restart",
      "reasons": [
        "dune changed"
      ]
    },
    {
      "name": "build-start",
      "restart": true
    },
    {
      "name": "build-finish",
      "outcome": "success"
    }
  ]
