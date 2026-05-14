Forwarded builds display a rich status line once connected over RPC.

  $ setup_xdg_runtime_dir

  $ echo "(lang dune 3.23)" > dune-project

  $ cat > dune <<EOF
  > (rule
  >  (target x)
  >  (action (write-file %{target} ok)))
  > EOF

  $ export INSIDE_EMACS=1
  $ export DUNE_CONFIG__THREADED_CONSOLE=disabled

  $ start_dune --display progress
  $ : > .#dune-output
  $ with_timeout_quiet dune rpc ping
  $ tr '\r' '\n' < .#dune-output | grep -a -m 1 "\[rpc 0\]"
  [1]

  $ : > .#dune-output
  $ INSIDE_EMACS=1 DUNE_CONFIG__THREADED_CONSOLE=disabled \
  >   with_timeout dune build --display progress x > output 2>&1
  $ tr '\r' '\n' < output | grep -m 1 "Connected to RPC server"
  Connected to RPC server
  $ tr '\r' '\n' < .#dune-output | grep -a -m 1 "\[rpc 1\]"
  [rpc 1]

  $ stop_dune_quiet
