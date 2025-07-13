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
  Error: Dune does not support packages outside the workspace depending on
  packages in the workspace. The package "bar" is not in the workspace but it
  depends on the package "dune" which is in the workspace.
  [1]
