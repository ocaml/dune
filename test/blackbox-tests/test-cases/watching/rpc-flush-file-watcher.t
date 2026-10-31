The flush-file-watcher RPC waits for pending file watcher notifications to be
handled by the watch server.

  $ export DUNE_TRACE=cache,rpc
  $ # Needed when upgrading this test to Dune language 3.25:
  $ # export DUNE_CONFIG__LANDLOCK=disabled
  $ setup_xdg_runtime_dir

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > (using action-plugin 0.1)
  > EOF

  $ cat > x <<EOF
  > original
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (target y)
  >  (deps x)
  >  (action (copy x y)))
  > EOF

  $ start_dune

  $ build y
  Success

  $ echo updated > x

This is the synchronization point: the RPC returns only after file watcher
events preceding the request have reached the scheduler.

  $ with_timeout dune rpc flush-file-watcher --wait

  $ stop_dune_quiet

The source event should have been processed before the flush RPC returned, not
later during shutdown.

  $ dune trace cat | jq -s '
  > ([ .[] | select(.name == "fs_update" and .args.path == "x") | .ts ] | max) as $last_fs_update
  > | ([ .[]
  >      | select(.cat == "rpc"
  >               and .name == "request"
  >               and .args.meth == "flush-file-watcher"
  >               and .args.stage == "stop")
  >      | .ts
  >    ] | min) as $flush_done
  > | ($last_fs_update != null and $flush_done != null and $last_fs_update <= $flush_done)'
  true

The RPC reports [`Not_in_watch_mode] when the server is only running for a
batch build. The action uses [dynamic-run] because Dune starts the batch RPC
server before running a dynamic action. Once the helper has touched [$STARTED],
the test can query the server without racing its startup.

  $ STARTED="$PWD/started"
  $ RELEASE="$PWD/release"
  $ rm -f "$STARTED" "$RELEASE"

  $ cat > dune <<EOF
  > (rule
  >  (target hold-target)
  >  (action
  >   (progn
  >    (dynamic-run action_plugin_helper hold "$STARTED" "$RELEASE")
  >    (write-file %{target} ok))))
  > EOF

  $ dune build hold-target > batch.out 2>&1 &
  $ BATCH_PID=$!
  $ with_timeout dune_cmd wait-for-file-to-appear "$STARTED"

  $ with_timeout dune rpc flush-file-watcher --wait
  Error: flush-file-watcher is only available in watch mode
  [1]

  $ touch "$RELEASE"
  $ if wait_for_pid_to_exit_with_timeout "$BATCH_PID" 200; then
  >   wait "$BATCH_PID"
  > else
  >   cat batch.out
  >   kill "$BATCH_PID"
  >   wait "$BATCH_PID"
  > fi
  $ cat batch.out
