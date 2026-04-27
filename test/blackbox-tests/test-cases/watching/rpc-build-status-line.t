Forwarded builds display a rich status line once connected over RPC.

  $ setup_xdg_runtime_dir

  $ echo "(lang dune 3.23)" > dune-project

  $ cat > dune <<EOF
  > (rule
  >  (target x)
  >  (action (write-file %{target} ok)))
  > EOF

  $ start_dune
  $ INSIDE_EMACS=1 DUNE_CONFIG__THREADED_CONSOLE=disabled \
  >   with_timeout dune build --display progress x > output 2>&1
  $ tr '\r' '\n' < output | grep -m 1 "Connected to RPC server"
  Connected to RPC server

  $ stop_dune_quiet
