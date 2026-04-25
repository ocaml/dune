Minimal RPC watch shutdown after an RPC build, separating the shutdown command
from server exit.

  $ echo "(lang dune 3.23)" > dune-project

  $ cat > dune <<EOF
  > (rule
  >  (target x)
  >  (action (write-file %{target} ok)))
  > EOF

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
