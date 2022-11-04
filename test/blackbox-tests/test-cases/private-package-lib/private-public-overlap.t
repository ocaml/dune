We should forbid private libraries that belong to a package from depending on a
private library

  $ cat >dune-project <<EOF
  > (lang dune 3.5)
  > (package (name pkg))
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (libraries bar)
  >  (package pkg)
  >  (modules))
  > (library
  >  (name bar)
  >  (modules))
  > EOF

  $ dune build foo.cma
