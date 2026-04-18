Test that compile_commands.json is generated for C++ stubs.

  $ make_dune_project 3.23

Create a library with both C and C++ stubs:

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (foreign_stubs (language c) (names c_stub))
  >  (foreign_stubs (language cxx) (names cxx_stub)))
  > EOF

  $ cat >c_stub.c <<EOF
  > #include <caml/mlvalues.h>
  > value c_stub(value unit) { return Val_int(1); }
  > EOF

  $ cat >cxx_stub.cpp <<EOF
  > #include <caml/mlvalues.h>
  > extern "C" value cxx_stub(value unit) { return Val_int(2); }
  > EOF

  $ cat >foo.ml <<EOF
  > external c_fn : unit -> int = "c_stub"
  > external cxx_fn : unit -> int = "cxx_stub"
  > EOF

Build compile_commands.json:

  $ dune build compile_commands.json

Check that both C and C++ files are present in the promoted file:

  $ jq '[.[].file] | sort' compile_commands.json
  [
    "c_stub.c",
    "cxx_stub.cpp"
  ]
