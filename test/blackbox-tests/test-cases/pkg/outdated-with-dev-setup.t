Reproduce internal error with dune pkg outdated in #11188.

  $ . ./helpers.sh
  $ mkrepo
  $ mkpkg a
  $ mkpkg b

  $ solve_project<<EOF
  > (lang dune 3.20)
  > (package
  >  (name foo)
  >  (depends a (b :with-dev-setup)))
  > EOF
  Solution for .dune-solution-cache:
  - a.0.0.1

dune pkg outdated is able to handle :with-dev-setup correctly.
  $ dune pkg outdated
  .dune-solution-cache is up to date.

