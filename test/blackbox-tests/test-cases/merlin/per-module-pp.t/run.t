  $ dune build @check
  File "dune", line 8, characters 15-18:
  8 |  (libraries pp ppx))
                     ^^^
  Error: Library "ppx" not found.
  Hint: try:
    dune external-lib-deps --missing @check
  [1]

We dump the config for Foo and Bar modules but the pp.exe preprocessor
should appear only once since only Foo is using it.

  $ dune ocaml-merlin --dump-config=$(pwd)
  Foo
  ((EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (S
    $TESTCASE_ROOT)
   (FLG
    (-pp
     $TESTCASE_ROOT/_build/default/pp/pp.exe))
   (FLG
    (-w
     @1..3@5..28@30..39@43@46..47@49..57@61..62-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs)))
  Bar
  ((EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (S
    $TESTCASE_ROOT)
   (FLG
    (-w
     @1..3@5..28@30..39@43@46..47@49..57@61..62-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs)))
