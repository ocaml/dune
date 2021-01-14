  $ dune exec ./foo.exe
  42

  $ dune ocaml-merlin --dump-config=$(pwd)
  Foo
  ((EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/.foo.eobjs/byte)
   (B
    $TESTCASE_ROOT/_build/default/foo/.foo.objs/byte)
   (S
    $TESTCASE_ROOT)
   (S
    $TESTCASE_ROOT/foo)
   (FLG
    (-w
     @1..3@5..28@30..39@43@46..47@49..57@61..62-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs)))

  $ dune ocaml-merlin --dump-config=$(pwd)/foo
  Bar
  ((EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/foo/.foo.objs/byte)
   (S
    $TESTCASE_ROOT/foo)
   (FLG
    (-open
     Foo
     -w
     @1..3@5..28@30..39@43@46..47@49..57@61..62-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs)))
  Foo
  ((EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/foo/.foo.objs/byte)
   (S
    $TESTCASE_ROOT/foo)
   (FLG
    (-open
     Foo
     -w
     @1..3@5..28@30..39@43@46..47@49..57@61..62-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs)))

FIXME : module Foo is not unbound
  $ ocamlmerlin single errors -filename foo.ml < foo.ml | jq ".value"
  [
    {
      "start": {
        "line": 1,
        "col": 10
      },
      "end": {
        "line": 1,
        "col": 25
      },
      "type": "typer",
      "sub": [],
      "valid": true,
      "message": "Unbound module Foo"
    }
  ]
