Test that compile_commands.json is regenerated when flags change.

  $ make_dune_project 3.23

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (foreign_stubs (language c) (names stub) (flags :standard -DFOO=1)))
  > EOF

  $ cat >stub.c <<EOF
  > #include <caml/mlvalues.h>
  > value foo_stub(value unit) { return Val_int(42); }
  > EOF

  $ cat >foo.ml <<EOF
  > external stub : unit -> int = "foo_stub"
  > EOF

  $ dune build compile_commands.json

The initial flags should contain -DFOO=1:

  $ jq '.[0].arguments | map(select(startswith("-DFOO")))' compile_commands.json
  [
    "-DFOO=1"
  ]

Now change the flags:

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (foreign_stubs (language c) (names stub) (flags :standard -DFOO=2)))
  > EOF

  $ dune build compile_commands.json

The flags should now contain -DFOO=2:

  $ jq '.[0].arguments | map(select(startswith("-DFOO")))' compile_commands.json
  [
    "-DFOO=2"
  ]

A stanza without foreign stubs leaves the compilation database unchanged:

  $ cp compile_commands.json original.json
  $ cat >>dune <<EOF
  > (alias (name unrelated))
  > (library
  >  (name bar)
  >  (modules))
  > EOF
  $ dune build compile_commands.json
  $ cmp original.json compile_commands.json
  $ cp dune dune-without-bar-stubs

Adding foreign stubs to that stanza includes its entries:

  $ cat >bar_stub.c <<EOF
  > #include <caml/mlvalues.h>
  > value bar_stub(value unit) { return Val_int(7); }
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (foreign_stubs (language c) (names stub) (flags :standard -DFOO=2)))
  > (alias (name unrelated))
  > (library
  >  (name bar)
  >  (modules)
  >  (foreign_stubs (language c) (names bar_stub)))
  > EOF
  $ dune build compile_commands.json
  $ jq '[.[].file]' compile_commands.json
  [
    "bar_stub.c",
    "stub.c"
  ]

Removing those stubs restores the original compilation database:

  $ cp dune-without-bar-stubs dune
  $ dune build compile_commands.json
  $ cmp original.json compile_commands.json
