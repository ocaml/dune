Reproduce #3143

  $ echo "(lang dune 2.3)" > dune-project
  $ touch dummypkg.opam
  $ cat >dune <<EOF
  > (library
  >  (public_name dummypkg)
  >  (libraries base doesnotexist.foo))
  > EOF
  $ dune external-lib-deps @install
  These are the external library dependencies in the default context:
  - base
  - doesnotexist.foo
