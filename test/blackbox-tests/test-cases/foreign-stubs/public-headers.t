Headers with the same filename cannot be installed together:

  $ make_dune_project_with_package 3.8 mypkg

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

  $ make_foreign_header_consumer <<EOF
  > #include <foo.h>
  > #include <inc/foo.h>
  > EOF
  $ dune build bar.exe
