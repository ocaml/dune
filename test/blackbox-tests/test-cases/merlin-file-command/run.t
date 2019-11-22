Test the dune merlin-file command. This command is used to generate the .merlin
config for a particular source file.

  $ echo "(lang dune 2.0)" > dune-project
  $ mkdir foo bar
  $ cat >bar/dune <<EOF
  > (library (name bar))
  > EOF
  $ cat >foo/dune <<EOF
  > (library (name foo) (libraries bar))
  > EOF
  $ dune merlin-file ./foo/test.ml
  EXCLUDE_QUERY_DIR
  B ../_build/default/bar/.bar.objs/byte
  B ../_build/default/foo/.foo.objs/byte
  S ../bar
  S .
  FLG -open Foo -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs
