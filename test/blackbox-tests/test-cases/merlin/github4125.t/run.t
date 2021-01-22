  $ dune build

  $ ls -a _build/cross/.merlin-conf
  .
  ..
  lib-foo

  $ dune ocaml-merlin --dump-config="$(pwd)"
  Foo
  ((EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/cross/.foo.objs/byte)
   (S
    $TESTCASE_ROOT)
   (FLG
    (-w
     @1..3@5..28@30..39@43@46..47@49..57@61..62-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs)))
