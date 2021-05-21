  $ dune build a
  Error: Dependency cycle between:
     _build/default/a
  -> _build/default/b
  -> _build/default/a
  [1]

This second example is slightly more complicated as we request result1
but the cycle doesn't involve result1. We must make sure the output
does show a cycle.

  $ dune build result1
  Error: Dependency cycle between:
     _build/default/input
  -> _build/default/result2
  -> _build/default/input
  -> required by _build/default/result1
  [1]

  $ dune build result1 --debug-dependency-path
  Error: Dependency cycle between:
     _build/default/input
  -> _build/default/result2
  -> _build/default/input
  -> required by _build/default/result1
  [1]
