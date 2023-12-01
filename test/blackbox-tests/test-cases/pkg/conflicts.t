The solver should repsect the (conflicts) field of the (package) stanza.

  $ . ./helpers.sh
  $ mkrepo
  $ mkpkg foo
  $ mkpkg bar << EOF
  > depends: [ foo ]
  > EOF

The solver should say no solution rather than just ignoring the conflict.

  $ solve_project << EOF
  > (lang dune 3.11)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (conflicts foo)
  >  (depends bar))
  > EOF
  Solution for dune.lock:
  - bar.0.0.1

