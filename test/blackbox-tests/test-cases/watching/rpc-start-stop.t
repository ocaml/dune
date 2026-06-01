Minimal RPC watch startup and shutdown without an RPC build.

  $ export DUNE_TRACE=rpc

  $ make_simple_rpc_watch_project

  $ start_dune

  $ stop_dune_quiet

  $ summarize_rpc_trace
  shutdown start
  shutdown stop
  accept stop close
