This tests that depopts are considered optional by the solver.

  $ . ../helpers.sh
  $ mkrepo
  $ mkpkg foo
  $ mkpkg bar

We create a package that depends on foo and has an optional dependency on bar
but also a conflict with bar. This should ensure that bar is never selected by
the solver.

  $ solve_project << EOF
  > (lang dune 3.11)
  > (package
  >  (name x)
  >  (depends foo)
  >  (depopts bar)
  >  (conflicts bar))
  > EOF
  Solution for dune.lock:
  - foo.0.0.1
