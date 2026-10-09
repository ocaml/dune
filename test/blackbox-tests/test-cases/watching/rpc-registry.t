Batch builds do not write to the RPC registry.

  $ setup_xdg_runtime_dir
  $ export DUNE_TRACE=rpc
  $ cp "$(command -v action_plugin_helper)" ./action_plugin_helper.exe

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (target x)
  >  (action (write-file %{target} ok)))
  > 
  > (rule
  >  (target dynamic-target)
  >  (deps input)
  >  (action
  >   (progn
  >    (dynamic-run ./action_plugin_helper.exe noop)
  >    (write-file %{target} ok))))
  > EOF

  $ dune build x

  $ dune trace cat | jq -r 'select(.cat == "rpc" and .name == "registry-write") | .name'

Batch builds that start the RPC server for a dynamic action still do not write
to the RPC registry.

  $ echo batch > input
  $ DUNE_RPC=invalid-inherited-address dune build dynamic-target

  $ dune trace cat | jq -r 'select(.cat == "rpc" and .name == "registry-write") | .name'

Watch mode writes a registry entry when the RPC server starts.

  $ echo watch > input
  $ start_dune

  $ build_quiet dynamic-target
  $ cat _build/default/dynamic-target
  ok

  $ stop_dune_quiet

  $ dune trace cat | jq -r 'select(.cat == "rpc" and .name == "registry-write") | .name'
  registry-write
