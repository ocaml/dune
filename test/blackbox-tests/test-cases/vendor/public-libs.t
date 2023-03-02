A public library shouldn't be allowed to depend on a vendored library.

A public library that depends on a vendored library is impossible to install,
since we cannot install the vendored artifacts.

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > (package (name foo))
  > EOF

  $ mkdir -p vendor/mypkg
  $ cat >vendor/mypkg/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name mypkg))
  > EOF

  $ cat >vendor/mypkg/dune <<EOF
  > (library
  >  (name invendor)
  >  (public_name mypkg.invendor))
  > EOF

  $ cat >dune <<EOF
  > (vendored_dirs vendor)
  > (library
  >  (libraries mypkg.invendor)
  >  (public_name foo))
  > EOF

  $ dune build foo.cma
