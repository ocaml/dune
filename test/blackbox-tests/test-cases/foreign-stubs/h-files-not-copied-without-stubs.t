Header files (.h) should only be dependencies when C/C++ compilation
is configured for the directory. Without foreign_stubs, .h files in the
source tree should not appear as dependencies of any build rule.

See https://github.com/ocaml/dune/issues/2370

  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > EOF

A library with no C stubs — .h files should not be a dependency of any rule:

  $ mkdir no-stubs
  $ cat > no-stubs/dune << EOF
  > (library (name mylib))
  > EOF
  $ cat > no-stubs/mylib.ml << EOF
  > let x = 1
  > EOF
  $ cat > no-stubs/vendored.h << EOF
  > /* should not appear as a dependency */
  > EOF

  $ dune build no-stubs/.mylib.objs/byte/mylib.cmo
  $ dune rules --root . --format=json --deps no-stubs/ |
  > jq -e 'include "dune"; [ .[] | depsFilePaths | select(endswith("vendored.h")) ] | length == 0'
  true

A library with foreign_stubs — .h files should be a dependency.
Build the stub object with sandboxing to ensure dependencies are accurate:

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

  $ dune build --sandbox copy with-stubs/stub.o
  $ dune rules --root . --format=json --deps with-stubs/stub.o |
  > jq -r 'include "dune"; .[] | depsFilePaths | select(endswith("needed.h"))'
  _build/default/with-stubs/needed.h
