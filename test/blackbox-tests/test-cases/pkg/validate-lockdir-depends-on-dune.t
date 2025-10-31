Reproduce internal error with dune pkg validate-lockdir in #11188.

  $ . ./helpers.sh
  $ mkrepo
  $ mkpkg a <<EOF
  > depends: [ "dune" ]
  > EOF
  $ mkpkg dune

  $ solve_project<<EOF
  > (lang dune 3.20)
  > (package
  >  (name vscode)
  >  (depends
  >   a))
  > EOF
  Solution for .dune-solution-cache:
  - a.0.0.1

Dune is able to verify this lock directory correctly:
  $ dune pkg validate-lockdir
