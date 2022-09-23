external library dependencies of a simple project

  $ echo "(lang dune 2.3)" > dune-project
  $ touch dummypkg.opam
  $ cat >dune <<EOF
  > (library
  >  (public_name dummypkg)
  >  (libraries base doesnotexist.foo))
  > EOF
  $ dune describe external-lib-deps
  (default ((. ((base required) (doesnotexist.foo required)))))
