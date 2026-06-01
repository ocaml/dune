Minimal RPC watch shutdown after an RPC build.

  $ export DUNE_TRACE=rpc

  $ make_simple_rpc_watch_project

  $ start_dune
  $ build_quiet x

  $ stop_dune_quiet

  $ summarize_rpc_trace
  build start
  build stop
  shutdown start
  shutdown stop
  accept stop close
