We build the project
  $ dune exec ./test.exe
  bar

Verify that merlin configuration was generated...
  $ dune ocaml-merlin --dump-config=$(pwd) |
  > sed 's#'$(opam config var prefix)'#OPAM_PREFIX#'
  Test
  ((STDLIB OPAM_PREFIX/lib/ocaml)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (B
    $TESTCASE_ROOT/_build/default/.test.eobjs/byte)
   (S
    $TESTCASE_ROOT)
   (S
    $TESTCASE_ROOT/411)
   (FLG
    (-w
     @1..3@5..28@30..39@43@46..47@49..57@61..62-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs)))
  Foo
  ((STDLIB OPAM_PREFIX/lib/ocaml)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (S
    $TESTCASE_ROOT)
   (S
    $TESTCASE_ROOT/411)
   (FLG
    (-w
     @1..3@5..28@30..39@43@46..47@49..57@61..62-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs)))

...but not in the sub-folder whose content was copied
  $ dune ocaml-merlin --dump-config=$(pwd)/411

Now we check that both querying from the root and the subfolder works
  $ FILE=$(pwd)/foo.ml
  $ FILE411=$(pwd)/411/test.ml

  $ dune ocaml-merlin  <<EOF | sed -E "s/[[:digit:]]+:/\?:/g" | sed 's#'$(opam config var prefix)'#OPAM_PREFIX#'
  > (4:File${#FILE}:$FILE)
  > EOF
  ((?:STDLIB?:OPAM_PREFIX/lib/ocaml)(?:EXCLUDE_QUERY_DIR)(?:B?:$TESTCASE_ROOT/_build/default/.foo.objs/byte)(?:S?:$TESTCASE_ROOT)(?:S?:$TESTCASE_ROOT/411)(?:FLG(?:-w?:@1..3@5..28@30..39@43@46..47@49..57@61..62-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs)))

  $ dune ocaml-merlin  <<EOF | sed -E "s/[[:digit:]]+:/\?:/g" | sed 's#'$(opam config var prefix)'#OPAM_PREFIX#'
  > (4:File${#FILE411}:$FILE411)
  > EOF
  ((?:STDLIB?:OPAM_PREFIX/lib/ocaml)(?:EXCLUDE_QUERY_DIR)(?:B?:$TESTCASE_ROOT/_build/default/.foo.objs/byte)(?:B?:$TESTCASE_ROOT/_build/default/.test.eobjs/byte)(?:S?:$TESTCASE_ROOT)(?:S?:$TESTCASE_ROOT/411)(?:FLG(?:-w?:@1..3@5..28@30..39@43@46..47@49..57@61..62-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs)))
