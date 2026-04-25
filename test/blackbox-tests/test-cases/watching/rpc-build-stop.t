Minimal RPC watch shutdown after an RPC build.

  $ export DUNE_TRACE=rpc

  $ echo "(lang dune 3.23)" > dune-project

  $ cat > dune <<EOF
  > (rule
  >  (target x)
  >  (action (write-file %{target} ok)))
  > EOF

  $ start_dune
  $ build_quiet x

  $ stop_dune_quiet

  $ summarize_rpc_trace
  build start
  build stop
  shutdown start
  shutdown stop
  accept stop close
