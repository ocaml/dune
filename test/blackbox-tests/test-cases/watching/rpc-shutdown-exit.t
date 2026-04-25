Minimal RPC watch shutdown, separating the shutdown command from server exit.

  $ export DUNE_TRACE=rpc

  $ echo "(lang dune 3.23)" > dune-project

  $ cat > dune <<EOF
  > (rule
  >  (target x)
  >  (action (write-file %{target} ok)))
  > EOF

  $ start_dune

  $ shutdown_dune_quiet

  $ ! with_timeout_quiet dune rpc ping >/dev/null 2>&1

  $ wait_for_dune_exit_with_timeout

  $ summarize_rpc_trace
  shutdown start
  shutdown stop
  accept stop close
