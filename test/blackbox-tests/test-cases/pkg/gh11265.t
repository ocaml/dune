Package conflicts are ignored when dune-projects contains multiple conflicts

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
  Error:
  Unable to solve dependencies while generating lock directory: dune.lock
  
  Couldn't solve the package dependency formula.
  Selected candidates: foo.dev
  - bar -> (problem)
      No usable implementations:
        bar.0.0.1: Package does not satisfy constraints of local package foo
  [1]

Now add an additional conflict on a non-existant package "baz". Dune should continue to fail to find a solution due to the conflict with "bar".
  $ solve_project << EOF
  > (lang dune 3.11)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends bar)
  >  (conflicts bar baz))
  > EOF
  Error:
  Unable to solve dependencies while generating lock directory: dune.lock
  
  Couldn't solve the package dependency formula.
  Selected candidates: foo.dev
  - bar -> (problem)
      No usable implementations:
        bar.0.0.1: Package does not satisfy constraints of local package foo
  [1]

