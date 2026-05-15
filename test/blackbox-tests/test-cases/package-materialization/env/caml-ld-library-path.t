Test that (deps (package ...)) adds lib/stublibs/ to CAML_LD_LIBRARY_PATH.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name mypkg))
  > EOF
  $ mkdir src
  $ cat >src/dune <<EOF
  > (library
  >  (public_name mypkg)
  >  (foreign_stubs (language c) (names stub)))
  > EOF
  $ cat >src/mypkg.ml <<'EOF'
  > let x = 1
  > EOF
  $ cat >src/stub.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value mypkg_stub(value unit) { return Val_unit; }
  > EOF
  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package mypkg))
  >  (action (with-stdout-to out (bash "echo $CAML_LD_LIBRARY_PATH"))))
  > EOF

  $ baseline=$CAML_LD_LIBRARY_PATH
  $ dune build out
  $ env_added "$(cat _build/default/out)" "$baseline" | censor
  $PWD/_build/install/default/.packages/$DIGEST/lib/stublibs
