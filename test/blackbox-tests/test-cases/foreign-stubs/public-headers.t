Headers with the same filename cannot be installed together:

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package (name mypkg))
  > EOF

  $ mkdir inc

  $ cat >dune <<EOF
  > (library
  >  (public_name mypkg)
  >  (public_headers foo.h inc/foo.h))
  > EOF

  $ touch foo.h inc/foo.h

  $ dune build mypkg.install && cat _build/default/mypkg.install | grep ".h"
    "_build/install/default/lib/mypkg/foo.h"
    "_build/install/default/lib/mypkg/inc/foo.h" {"inc/foo.h"}

Now we try to use the installed headers:

  $ dune install --prefix _install mypkg
  $ export OCAMLPATH=$PWD/_install/lib:$OCAMLPATH

  $ mkdir subdir
  $ cd subdir

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > EOF

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
  > #include <foo.h>
  > #include <inc/foo.h>
  > EOF
  $ dune build bar.exe
