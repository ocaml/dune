  $ dune build a/test.exe --debug-dependency-path
  Warning: one state has shift/reduce conflicts.
  Warning: one shift/reduce conflict was arbitrarily resolved.
  $ ls _build/default/a/parser.conflicts
  _build/default/a/parser.conflicts
  $ dune build b/test.exe --debug-dependency-path
  Warning: one state has shift/reduce conflicts.
  Warning: one shift/reduce conflict was arbitrarily resolved.
  $ ls _build/default/b/parser.conflicts
  _build/default/b/parser.conflicts
  $ dune build a/parser.conflicts
