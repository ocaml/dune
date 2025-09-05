Dune is defined in the workspace where we're solving

  $ . ./helpers.sh

  $ mkrepo

  $ mkpkg bar <<EOF
  > depends: [ "dune" ]
  > EOF

  $ mkpkg test-dep <<EOF
  > depends: [ "dune" ]
  > EOF

  $ solve_project <<EOF
  > (lang dune 3.19)
  > (package
  >  (name dune)
  >  (depends (test-dep :with-test)))
  > (package
  >  (name foo)
  >  (depends bar))
  > EOF
  Solution for dune.lock:
  - bar.0.0.1
  - test-dep.0.0.1
