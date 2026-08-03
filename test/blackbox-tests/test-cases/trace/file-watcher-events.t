File watcher events are traced in watch mode.

  $ export DUNE_TRACE=file_watcher
  $ setup_xdg_runtime_dir
  $ make_dune_project 3.23

  $ cat >x <<EOF
  > original
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (target y)
  >  (deps x)
  >  (action (copy x y)))
  > EOF

  $ start_dune @idle
  $ build y
  Success

  $ echo updated > x
  $ with_timeout dune rpc flush-file-watcher --wait
  $ stop_dune_quiet

Only keep reproducible fields from the traced events.

  $ dune trace cat | jq -s '
  > [ .[]
  > | select(.cat == "file_watcher")
  > | if .name == "changed" and .args.path? == "x"
  >   then { name, path: .args.path }
  >   elif .name == "sync"
  >   then { name }
  >   else empty
  >   end
  > ]
  > | unique
  > | sort_by(.name, .path // "")
  > | .[]'
  {
    "name": "changed",
    "path": "x"
  }
  {
    "name": "sync"
  }
