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
  Can't find all required versions.
  Selected: bar.0.0.1 x.dev
  - foo -> (problem)
      x dev requires conflict with all versions
      Rejected candidates:
        foo.0.0.1: Incompatible with restriction: conflict with all versions
  [1]

