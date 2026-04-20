Test that the default trace file is backed up as trace.csexp.old before each
build.

  $ make_dune_project 3.21

First build creates the trace file but no backup yet:

  $ dune build
  $ test -f _build/trace.csexp
  $ test -f _build/trace.csexp.old
  [1]

Second build should back up the previous trace:

  $ dune build
  $ test -f _build/trace.csexp
  $ test -f _build/trace.csexp.old

The backup should be a valid trace with an exit event from the previous build:

  $ dune trace cat --trace-file _build/trace.csexp.old | jq 'select(.name == "exit") | {name}'
  {
    "name": "exit"
  }

When --trace-file is used, no backup should be created. The existing custom
trace file is simply overwritten:

  $ rm -f _build/trace.csexp.old
  $ dune build --trace-file custom-trace.csexp
  $ test -f custom-trace.csexp
  $ dune trace cat --trace-file custom-trace.csexp | jq 'select(.name == "exit") | {name}'
  {
    "name": "exit"
  }

Write a marker into the custom trace to verify it gets overwritten:

  $ echo "stale" > custom-trace.csexp
  $ dune build --trace-file custom-trace.csexp
  $ dune trace cat --trace-file custom-trace.csexp | jq 'select(.name == "exit") | {name}'
  {
    "name": "exit"
  }

No .old backup was created for the custom trace:

  $ test -f custom-trace.csexp.old
  [1]

The default trace backup should not have been recreated either:

  $ test -f _build/trace.csexp.old
  [1]
