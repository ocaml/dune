Test that compile_commands.json includes entries for executable stubs.

  $ make_dune_project 3.23

Create an executable with a C stub:

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (foreign_stubs (language c) (names stub)))
  > EOF

  $ cat >stub.c <<EOF
  > #include <caml/mlvalues.h>
  > value my_stub(value unit) { return Val_int(42); }
  > EOF

  $ cat >main.ml <<EOF
  > external my_stub : unit -> int = "my_stub"
  > let () = Printf.printf "%d\n" (my_stub ())
  > EOF

Build compile_commands.json:

  $ dune build compile_commands.json

Check that the executable's stub appears:

  $ jq '.[0].file' compile_commands.json
  "stub.c"
