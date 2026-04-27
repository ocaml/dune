The flush-file-watcher RPC waits for pending file watcher notifications to be
handled by the watch server.

  $ export DUNE_TRACE=cache
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

  $ dune trace cat | jq -s 'include "dune";
  > [ .[] | fsUpdateWithPath("x")
  >        | select(.cache_type == "file_digest" and .result == "changed")
  >        | {cache_type, path, result}
  > ] | unique | .[]'
  {
    "cache_type": "file_digest",
    "path": "x",
    "result": "changed"
  }

The RPC reports [`Not_in_watch_mode] when the server is only running for a
batch build. The action uses [dynamic-run] because Dune starts the batch RPC
server before running a dynamic action. Once the helper has touched [$STARTED],
the test can query the server without racing its startup.

  $ STARTED="$PWD/started"
  $ RELEASE="$PWD/release"
  $ rm -f "$STARTED" "$RELEASE"

  $ cat > hold.ml <<EOF
  > open Dune_action_plugin.V1
  > 
  > let touch path =
  >   let oc = open_out path in
  >   close_out oc
  > ;;
  > 
  > let rec wait_for_file path =
  >   if not (Sys.file_exists path)
  >   then (
  >     ignore (Unix.select [] [] [] 0.01);
  >     wait_for_file path)
  > ;;
  > 
  > let () =
  >   let started = Sys.argv.(1) in
  >   let release = Sys.argv.(2) in
  >   touch started;
  >   wait_for_file release;
  >   run (return ())
  > ;;
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name hold)
  >  (libraries dune-action-plugin unix))
  > 
  > (rule
  >  (target hold-target)
  >  (action
  >   (progn
  >    (dynamic-run ./hold.exe "$STARTED" "$RELEASE")
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
