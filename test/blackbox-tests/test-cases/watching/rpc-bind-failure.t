Startup RPC bind failures should be reported immediately and terminate dune.

  $ export DUNE_TRACE=rpc

  $ echo "(lang dune 3.23)" > dune-project

  $ cat > dune <<EOF
  > (rule
  >  (target x)
  >  (action (write-file %{target} ok)))
  > EOF

Poison the parent of the Unix-domain RPC socket so that binding
_build/.rpc/dune fails during startup.

  $ mkdir -p _build
  $ : > _build/.rpc

  $ OUTPUT=$(mktemp)
  $ ((dune build --passive-watch-mode >"$OUTPUT" 2>&1) || (echo exit $? >>"$OUTPUT")) &
  $ DUNE_PID=$!

  $ wait_for_dune_exit_with_timeout

  $ grep "Uncaught RPC Error" "$OUTPUT"
  Uncaught RPC Error

  $ grep "bind" "$OUTPUT"
  Unix.Unix_error(Unix.ENOTDIR, "bind", "")
  Raised by primitive operation at Rpc__Csexp_rpc.Socket.U.bind in file
  Called from Rpc__Csexp_rpc.Socket.bind in file "src/rpc/csexp_rpc.ml"
  Error: bind(): Not a directory

  $ grep '^exit 1$' "$OUTPUT"
  exit 1

  $ dune trace cat | jq -c '
  >    select(.cat == "rpc" and .name == "startup-failure")
  > | { name, error: .args.error.exn }
  > '
  {"name":"startup-failure","error":"Unix.Unix_error(Unix.ENOTDIR, \"bind\", \"\")"}

  $ ! with_timeout_quiet dune rpc ping >/dev/null 2>&1
