A client connecting to a watch server that was killed without cleaning up its
RPC socket reports the connection failure with a backtrace.

  $ make_simple_rpc_watch_project

  $ start_dune 2>/dev/null

  $ SERVER_PID=$(cat _build/.lock)
  $ kill -9 "$SERVER_PID"
  $ wait_for_pid_to_exit_with_timeout "$SERVER_PID" 200
  $ wait_for_dune_exit_with_timeout

  $ with_timeout dune rpc ping > client.output 2>&1
  [1]
  $ head -n 3 client.output
  Error: failed to connect to RPC server unix:path=_build/.rpc/dune
  Unix.Unix_error(Unix.ECONNREFUSED, "connect", "")
  backtrace:
  $ grep -m1 '^Raised by primitive operation' client.output \
  > | sed 's/ in file.*//'
  Raised by primitive operation at Rpc__Csexp_rpc.Client.connect
