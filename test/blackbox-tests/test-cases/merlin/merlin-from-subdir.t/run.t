  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"

We build the project
  $ dune exec ./test.exe
  bar

Verify that merlin configuration was generated...
  $ dune ocaml-merlin --dump-config=$(pwd)
  Test
  ((STDLIB /OCAMLC_WHERE)
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
  ((STDLIB /OCAMLC_WHERE)
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

  $ dune ocaml-merlin  <<EOF | sed -E "s/[[:digit:]]+:/\?:/g"
  > (4:File${#FILE}:$FILE)
  > EOF
  Bad input: invalid character '\n', expected '(', ')' or '0'..'9'
  ((?:STDLIB?:/OCAMLC_WHERE)(?:EXCLUDE_QUERY_DIR)(?:B?:$TESTCASE_ROOT/_build/default/.foo.objs/byte)(?:S?:$TESTCASE_ROOT)(?:S?:$TESTCASE_ROOT/411)(?:FLG(?:-w?:@1..3@5..28@30..39@43@46..47@49..57@61..62-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs)))

  $ dune ocaml-merlin  <<EOF | sed -E "s/[[:digit:]]+:/\?:/g"
  > (4:File${#FILE411}:$FILE411)
  > EOF
  Bad input: invalid character '\n', expected '(', ')' or '0'..'9'
  ((?:STDLIB?:/OCAMLC_WHERE)(?:EXCLUDE_QUERY_DIR)(?:B?:$TESTCASE_ROOT/_build/default/.foo.objs/byte)(?:B?:$TESTCASE_ROOT/_build/default/.test.eobjs/byte)(?:S?:$TESTCASE_ROOT)(?:S?:$TESTCASE_ROOT/411)(?:FLG(?:-w?:@1..3@5..28@30..39@43@46..47@49..57@61..62-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs)))
