Headers of libraries are accidentally visible

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package (name mypkg))
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (public_name mypkg))
  > EOF

  $ mkdir sub
  $ touch sub/foo.h

foo.h should only be visible when we use mypkg.sub

  $ cat >sub/dune <<EOF
  > (library
  >  (name mypkg_sub)
  >  (public_name mypkg.sub)
  >  (install_c_headers foo))
  > EOF

  $ dune build mypkg.install

  $ dune install --prefix _install mypkg
  $ export OCAMLPATH=$PWD/_install/lib:$OCAMLPATH

  $ mkdir subdir
  $ cd subdir

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > EOF

We depend on mypkg, but we can see the header of mypkg.sub

  $ cat >dune <<EOF
  > (executable
  >  (name bar)
  >  (foreign_stubs
  >   (language c)
  >   (include_dirs (lib mypkg))
  >   (names foo)))
  > EOF
  $ touch bar.ml
  $ cat >foo.c <<EOF
  > // This header shouldn't be visible
  > #include <sub/foo.h>
  > EOF
  $ dune build bar.exe
