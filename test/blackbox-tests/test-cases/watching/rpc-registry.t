Batch builds do not write to the RPC registry.

  $ setup_xdg_runtime_dir
  $ export DUNE_TRACE=rpc

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > (using action-plugin 0.1)
  > EOF

  $ cat > plugin.ml <<EOF
  > open Dune_action_plugin.V1
  > 
  > let () = run (return ())
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name plugin)
  >  (libraries dune-action-plugin))
  > 
  > (rule
  >  (target x)
  >  (action (write-file %{target} ok)))
  > 
  > (rule
  >  (target dynamic-target)
  >  (action
  >   (progn
  >    (dynamic-run ./plugin.exe)
  >    (write-file %{target} ok))))
  > EOF

  $ dune build x

  $ dune trace cat | jq -r 'select(.cat == "rpc" and .name == "registry-write") | .name'

Batch builds that start the RPC server for a dynamic action still do not write
to the RPC registry.

  $ dune build dynamic-target

  $ dune trace cat | jq -r 'select(.cat == "rpc" and .name == "registry-write") | .name'

Watch mode writes a registry entry when the RPC server starts.

  $ start_dune

  $ stop_dune_quiet

  $ dune trace cat | jq -r 'select(.cat == "rpc" and .name == "registry-write") | .name'
  registry-write
