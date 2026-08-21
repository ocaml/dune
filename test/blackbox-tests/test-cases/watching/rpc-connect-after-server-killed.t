A client connecting to a watch server that was killed without cleaning up its
RPC socket reports the connection failure without a backtrace.

  $ make_simple_rpc_watch_project

  $ start_dune 2>/dev/null

  $ SERVER_PID=$(cat _build/.lock)
  $ kill -9 "$SERVER_PID"
  $ wait_for_pid_to_exit_with_timeout "$SERVER_PID" 200
  $ wait_for_dune_exit_with_timeout

  $ dune rpc ping
  Error: failed to connect to RPC server unix:path=_build/.rpc/dune
  Reason: connect(): Connection refused
  [1]
