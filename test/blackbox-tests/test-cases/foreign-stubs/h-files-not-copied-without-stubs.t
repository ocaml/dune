Header files (.h) should only be copied to _build when C/C++ compilation
is configured for the directory. Without foreign_stubs, .h files in the
source tree should not appear in _build.

See https://github.com/ocaml/dune/issues/2370

  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > EOF

A library with no C stubs — .h files should not be copied:

  $ mkdir no-stubs
  $ cat > no-stubs/dune << EOF
  > (library (name mylib))
  > EOF
  $ cat > no-stubs/mylib.ml << EOF
  > let x = 1
  > EOF
  $ cat > no-stubs/vendored.h << EOF
  > /* should not appear in _build */
  > EOF

  $ dune build
  $ test -f _build/default/no-stubs/vendored.h && echo "copied" || echo "not copied"
  not copied

A library with foreign_stubs — .h files should be copied:

  $ mkdir with-stubs
  $ cat > with-stubs/dune << EOF
  > (library
  >  (name stublib)
  >  (foreign_stubs (language c) (names stub)))
  > EOF
  $ cat > with-stubs/stublib.ml << EOF
  > external foo : unit -> unit = "foo"
  > EOF
  $ cat > with-stubs/stub.c << EOF
  > #include <caml/mlvalues.h>
  > #include "needed.h"
  > CAMLprim value foo(value unit) { return Val_unit; }
  > EOF
  $ cat > with-stubs/needed.h << EOF
  > /* needed by stub.c */
  > EOF

  $ dune build
  $ test -f _build/default/with-stubs/needed.h && echo "copied" || echo "not copied"
  copied
