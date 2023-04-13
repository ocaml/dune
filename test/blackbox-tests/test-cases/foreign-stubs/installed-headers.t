Headers with the same filename cannot be installed together:

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package (name mypkg))
  > EOF

  $ mkdir inc

  $ cat >dune <<EOF
  > (library
  >  (public_name mypkg)
  >  (install_c_headers foo inc/foo))
  > EOF

  $ touch foo.h inc/foo.h

  $ dune build mypkg.install && cat _build/default/mypkg.install | grep ".h"
  Error: Multiple rules generated for _build/install/default/lib/mypkg/foo.h:
  - dune:3
  - dune:3
  -> required by _build/default/mypkg.install
  [1]

Now we demonstrate that header paths get squashed when installed

  $ mv inc/foo.h inc/bar.h
  $ cat >dune <<EOF
  > (library
  >  (public_name mypkg)
  >  (install_c_headers foo inc/bar))
  > EOF

  $ dune build mypkg.install && cat _build/default/mypkg.install | grep ".h"
    "_build/install/default/lib/mypkg/bar.h"
    "_build/install/default/lib/mypkg/foo.h"

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
  > #include <inc/bar.h>
  > EOF
  $ dune build bar.exe 2>/dev/null
  [1]
