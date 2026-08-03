Minimal RPC watch shutdown after an RPC build, separating the shutdown command
from server exit.

  $ make_simple_rpc_watch_project

  $ export DUNE_TRACE=rpc

  $ start_dune

  $ build_quiet x

  $ shutdown_dune_quiet

  $ wait_for_dune_exit_with_timeout

  $ summarize_rpc_trace
  build start
  build stop
  shutdown start
  shutdown stop
  accept stop close
