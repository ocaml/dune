  $ ocamlc_where="$(ocamlc -where)"
  $ ENCODED_OCAMLC_WHERE=$(dune_cmd encode-prefix "$ocamlc_where")
  $ export BUILD_PATH_PREFIX_MAP=\
  > "/OCAMLC_WHERE=$ENCODED_OCAMLC_WHERE:$BUILD_PATH_PREFIX_MAP"

We build the project
  $ dune exec ./test.exe
  bar

Verify that merlin configuration was generated...
  $ dune ocaml merlin dump-config $PWD
  Test
  ((STDLIB /workspace_root/lib/ocaml)
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
     -keep-locs
     -g)))
  Foo
  ((STDLIB /workspace_root/lib/ocaml)
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
     -keep-locs
     -g)))

...but not in the sub-folder whose content was copied
  $ dune ocaml merlin dump-config $PWD/411

Now we check that both querying from the root and the subfolder works
  $ FILE=$PWD/foo.ml
  $ FILE411=$PWD/411/test.ml

  $ printf "(4:File%d:%s)" ${#FILE} $FILE | dune ocaml-merlin | sed -E "s/[[:digit:]]+:/\?:/g"
  ((?:STDLIB?:/workspace_root/lib/ocaml)(?:EXCLUDE_QUERY_DIR)(?:B?:$TESTCASE_ROOT/_build/default/.foo.objs/byte)(?:S?:$TESTCASE_ROOT)(?:S?:$TESTCASE_ROOT/411)(?:FLG(?:-w?:@1..3@5..28@30..39@43@46..47@49..57@61..62-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs?:-g)))

  $ printf "(4:File%d:%s)" ${#FILE411} $FILE411 | dune ocaml-merlin | sed -E "s/[[:digit:]]+:/\?:/g"
  ((?:STDLIB?:/workspace_root/lib/ocaml)(?:EXCLUDE_QUERY_DIR)(?:B?:$TESTCASE_ROOT/_build/default/.foo.objs/byte)(?:B?:$TESTCASE_ROOT/_build/default/.test.eobjs/byte)(?:S?:$TESTCASE_ROOT)(?:S?:$TESTCASE_ROOT/411)(?:FLG(?:-w?:@1..3@5..28@30..39@43@46..47@49..57@61..62-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs?:-g)))
