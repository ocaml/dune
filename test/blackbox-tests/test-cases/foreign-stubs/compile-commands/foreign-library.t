Test compile_commands.json with foreign_library stanzas and include directories.

  $ make_dune_project 3.23

First, create a library that exposes headers:

  $ mkdir -p headerslib
  $ cat >headerslib/dune <<EOF
  > (library
  >  (name headerslib)
  >  (install_c_headers myapi))
  > EOF
  $ cat >headerslib/myapi.h <<EOF
  > #define API_VERSION 1
  > EOF
  $ cat >headerslib/headerslib.ml <<EOF
  > let version = 1
  > EOF

Create an include directory with a header:

  $ mkdir include
  $ cat >include/myheader.h <<EOF
  > #define MY_VALUE 42
  > EOF

Create multiple foreign_library stanzas with include_dirs:

  $ cat >dune <<EOF
  > (foreign_library
  >  (archive_name add)
  >  (language c)
  >  (include_dirs include (lib headerslib))
  >  (names add))
  > 
  > (foreign_library
  >  (archive_name mul)
  >  (language c)
  >  (include_dirs include)
  >  (names mul))
  > 
  > (library
  >  (name calc)
  >  (libraries headerslib)
  >  (foreign_archives add mul))
  > EOF

  $ cat >add.c <<EOF
  > #include <caml/mlvalues.h>
  > #include "myheader.h"
  > value add_stub(value x, value y) { return Val_int(Int_val(x) + Int_val(y) + MY_VALUE); }
  > EOF

  $ cat >mul.c <<EOF
  > #include <caml/mlvalues.h>
  > #include "myheader.h"
  > value mul_stub(value x, value y) { return Val_int(Int_val(x) * Int_val(y) * MY_VALUE); }
  > EOF

  $ cat >calc.ml <<EOF
  > external add : int -> int -> int = "add_stub"
  > external mul : int -> int -> int = "mul_stub"
  > EOF

Build compile_commands.json:

  $ dune build compile_commands.json

Check that we have entries for both C files:

  $ jq '[.[].file] | sort' compile_commands.json
  [
    "add.c",
    "mul.c"
  ]

Check that include directories appear in the arguments for each file.
mul.c should have just "include", add.c should have both "include" and "headerslib":

  $ jq '.[] | {file: .file, includes: [.arguments | indices("-I") as $i | .[$i[]+1]] | map(select(contains("ocaml") | not))}' compile_commands.json
  {
    "file": "mul.c",
    "includes": [
      "include"
    ]
  }
  {
    "file": "add.c",
    "includes": [
      "include",
      "headerslib"
    ]
  }
