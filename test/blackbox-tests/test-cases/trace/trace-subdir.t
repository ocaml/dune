Test that trace files are written to the correct location when running dune from
a subdirectory.

  $ make_dune_project 3.21
  $ mkdir -p src

Run dune build from the subdirectory:

  $ (cd src && dune build --root ..)
  Entering directory '..'
  Leaving directory '..'

Check that the trace file is not written in the incorrect location:

  $ test -f src/_build/trace.csexp
  [1]

The trace file should be written in the correct location:

  $ test -f _build/trace.csexp

dune trace cat should work from both the root and subdirectory:

  $ dune trace cat | jq 'select(.name == "exit") | {name}'
  {
    "name": "exit"
  }

  $ (cd src && dune trace cat | jq 'select(.name == "exit") | {name}')
  {
    "name": "exit"
  }
