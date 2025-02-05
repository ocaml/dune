The solver should repsect the (conflicts) field of the (package) stanza.

  $ . ./helpers.sh
  $ mkrepo
  $ mkpkg foo
  $ mkpkg bar << EOF
  > depends: [ "foo" ]
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
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory dune.lock:
  Couldn't solve the package dependency formula.
  Selected candidates: bar.0.0.1 x.dev
  - foo -> (problem)
      No usable implementations:
        foo.0.0.1: Package does not satisfy constraints of local package x
  [1]

