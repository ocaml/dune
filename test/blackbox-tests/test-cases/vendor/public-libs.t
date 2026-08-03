A public library shouldn't be allowed to depend on a vendored library.

A public library that depends on a vendored library is impossible to install,
since we cannot install the vendored artifacts.

  $ make_dune_project_with_package 3.7 foo

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
