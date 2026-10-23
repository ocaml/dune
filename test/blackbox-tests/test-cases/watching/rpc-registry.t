Batch builds do not write to the RPC registry. Use an isolated runtime directory
so registry queries do not see watchers started by other tests.

  $ export TMPDIR="$(mktemp -d)"
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

An empty registry has no statuses to print.

  $ dune rpc status --all 2>&1 | dune_cmd subst '^$' '<empty>'
  <empty>
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

Status reports the connected client's negotiated menu. The registry view also
reports this server, rather than silently dropping it.

  $ dune rpc status 2>&1 \
  > | grep -E '^(Server|Connected|Client|- (diagnostics|status):)' \
  > | dune_cmd subst '(unix|tcp):.*' '$ENDPOINT'
  Server is listening on $ENDPOINT
  Connected clients (including this one):
  Client [status] with the following RPC versions:
  - diagnostics: 2
  - status: 1
  $ dune rpc status --all 2>&1 \
  > | dune_cmd subst 'pid: [0-9]+' 'pid: $PID'
  root:
  .
  - pid: $PID
  - clients: 1
  $ cp "$XDG_RUNTIME_DIR"/dune/rpc/*.csexp saved-registry

  $ build_quiet dynamic-target
  $ cat _build/default/dynamic-target
  ok

  $ stop_dune_quiet

  $ dune trace cat | jq -r 'select(.cat == "rpc" and .name == "registry-write") | .name'
  registry-write

Shutdown removes the live entry. Restoring it simulates a stale registry entry:
status must report the connection failure instead of failing the whole query.

  $ dune rpc status --all 2>&1 | dune_cmd subst '^$' '<empty>'
  <empty>
  $ cp saved-registry "$XDG_RUNTIME_DIR/dune/rpc/stale.csexp"
  $ dune rpc status --all >stale-status 2>&1
  $ grep -E '^- (pid|clients):' stale-status \
  > | dune_cmd subst 'pid: [0-9]+' 'pid: $PID'
  - pid: $PID
  - clients: failed to connect to RPC
