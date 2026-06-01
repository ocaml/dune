Minimal RPC watch startup and shutdown without an RPC build.

  $ export DUNE_TRACE=rpc

  $ make_dune_project 3.23

  $ cat > dune <<EOF
  > (rule
  >  (target x)
  >  (action (write-file %{target} ok)))
  > EOF

  $ start_dune

  $ stop_dune_quiet

  $ summarize_rpc_trace
  shutdown start
  shutdown stop
  accept stop close
