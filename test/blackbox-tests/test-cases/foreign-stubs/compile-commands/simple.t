Test that compile_commands.json is generated for C stubs.

  $ make_dune_project 3.23

Create a simple library with a C stub:

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

Build compile_commands.json directly:

  $ dune build compile_commands.json

The file is promoted to the workspace root:

  $ test -f compile_commands.json

Check that the file contains our stub:

  $ jq '.[0].file' compile_commands.json
  "stub.c"

Building @check also produces the file:

  $ dune clean
  $ dune build @check
  $ test -f compile_commands.json

Clean removes the promoted file:

  $ dune clean
  $ test -f compile_commands.json
  [1]
