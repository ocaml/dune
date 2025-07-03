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
  Solution for dune.lock:
  - a.0.0.1

  $ dune pkg outdated 2>&1 | grep 'Error: exception Invalid_argument("filter_deps")'
  Error: exception Invalid_argument("filter_deps")

