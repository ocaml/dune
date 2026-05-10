Forwarded builds display a rich status line once connected over RPC.

  $ export XDG_RUNTIME_DIR="$PWD/.xdg-runtime"
  $ mkdir -p "$XDG_RUNTIME_DIR"
  $ chmod 700 "$XDG_RUNTIME_DIR"

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
