`dune shutdown` stops the action-runner loop in watch mode.

  $ setup_xdg_runtime_dir
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
  $ [ "$?" = 0 ]
