Package conflicts are ignored when dune-projects contains multiple conflicts

  $ . ./helpers.sh
  $ mkrepo
  $ mkpkg bar

A package which depends on a single package and also conflicts with the same package:
  $ solve_project << EOF
  > (lang dune 3.11)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends bar)
  >  (conflicts bar))
  > EOF
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory dune.lock:
  Couldn't solve the package dependency formula.
  Selected candidates: foo.dev
  - bar -> (problem)
      foo dev requires conflict with all versions
      Rejected candidates:
        bar.0.0.1: Incompatible with restriction: conflict with all versions
  [1]

Now add an additional conflict on a non-existant package "baz". Dune will choose the package "bar" despite it being a conflict:
  $ solve_project << EOF
  > (lang dune 3.11)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends bar)
  >  (conflicts bar baz))
  > EOF
  Solution for dune.lock:
  - bar.0.0.1

