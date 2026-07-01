Minimal RPC watch shutdown, separating the shutdown command from server exit.

  $ export DUNE_TRACE=rpc

  $ make_simple_rpc_watch_project

  $ start_dune

  $ shutdown_dune_quiet

  $ ! with_timeout_quiet dune rpc ping >/dev/null 2>&1

  $ wait_for_dune_exit_with_timeout

  $ summarize_rpc_trace
  shutdown start
  shutdown stop
  accept stop close
