Known limitation: `dune shutdown` does not yet stop the action-runner loop in
watch mode.

  $ export DUNE_TRACE=rpc
  $ echo "(lang dune 3.23)" > dune-project
  $ cat > dune <<EOF
  > (rule
  >  (target x)
  >  (action (bash "echo ok > %{target}")))
  > EOF
  $ start_dune --action-runner
  $ build_quiet x
  $ shutdown_dune_quiet >/dev/null 2>&1
  $ wait_for_dune_exit_with_timeout >/dev/null 2>&1
  $ [ "$?" = 124 ]
  $ kill $DUNE_PID 2>/dev/null || true
  $ wait_for_dune_exit

