Test that compile_commands.json is not generated on dune lang < 3.23.

  $ make_dune_project 3.22

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (foreign_stubs (language c) (names stub)))
  > EOF

  $ cat >stub.c <<EOF
  > #include <caml/mlvalues.h>
  > value foo_stub(value unit) { return Val_int(42); }
  > EOF

  $ cat >foo.ml <<EOF
  > external stub : unit -> int = "foo_stub"
  > EOF

  $ dune build @check
  $ test -f _build/default/compile_commands.json
  [1]
  $ test -f compile_commands.json
  [1]
