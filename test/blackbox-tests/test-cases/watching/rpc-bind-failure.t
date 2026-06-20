Startup RPC bind failures should be reported immediately and terminate dune.

  $ export DUNE_TRACE=rpc

  $ make_simple_rpc_watch_project

Poison the parent of the Unix-domain RPC socket so that binding
_build/.rpc/dune fails during startup.

  $ mkdir -p _build
  $ : > _build/.rpc

  $ OUTPUT=$(mktemp)
  $ ((dune build --passive-watch-mode >"$OUTPUT" 2>&1) || (echo exit $? >>"$OUTPUT")) &
  $ DUNE_PID=$!

  $ wait_for_dune_exit_with_timeout

The important behavior is that the process exits promptly and reports the bind
failure.

  $ grep '^Error: bind(): Not a directory$' "$OUTPUT"
  Error: bind(): Not a directory

  $ grep '^exit 1$' "$OUTPUT"
  exit 1

  $ ! with_timeout_quiet dune rpc ping >/dev/null 2>&1
