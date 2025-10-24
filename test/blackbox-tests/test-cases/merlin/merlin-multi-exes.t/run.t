  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"
 
  $ FILE=$PWD/multi-exes/bin/b.ml
  $ printf "(4:File%d:%s)" ${#FILE} $FILE | dune ocaml-merlin
  ((5:ERROR71:No config found for file multi-exes/bin/b.ml. Try calling 'dune build'.))

  $ dune build @check

  $ printf "(4:File%d:%s)" ${#FILE} $FILE | dune ocaml-merlin | sed -E "s/[[:digit:]]+:/\?:/g"
  ((?:INDEX?:$TESTCASE_ROOT/_build/default/multi-exes/bin/.c.eobjs/cctx.ocaml-index)(?:INDEX?:$TESTCASE_ROOT/_build/default/multi-exes/bin/.b.eobjs/cctx.ocaml-index)(?:INDEX?:$TESTCASE_ROOT/_build/default/multi-exes/lib/.lib.objs/cctx.ocaml-index)(?:STDLIB?:/OCAMLC_WHERE)(?:SOURCE_ROOT?:$TESTCASE_ROOT)(?:EXCLUDE_QUERY_DIR)(?:B?:$TESTCASE_ROOT/_build/default/multi-exes/bin/.b.eobjs/byte)(?:S?:$TESTCASE_ROOT/multi-exes/bin)(?:FLG(?:-w?:@1..3@5..28@30..39@43@46..47@49..57@61..62-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs?:-g))(?:UNIT_NAME?:b))

  $ FILE=$PWD/multi-exes/bin/c.ml
  $ printf "(4:File%d:%s)" ${#FILE} $FILE | dune ocaml-merlin | sed -E "s/[[:digit:]]+:/\?:/g"
  ((?:INDEX?:$TESTCASE_ROOT/_build/default/multi-exes/bin/.c.eobjs/cctx.ocaml-index)(?:INDEX?:$TESTCASE_ROOT/_build/default/multi-exes/bin/.b.eobjs/cctx.ocaml-index)(?:INDEX?:$TESTCASE_ROOT/_build/default/multi-exes/lib/.lib.objs/cctx.ocaml-index)(?:STDLIB?:/OCAMLC_WHERE)(?:SOURCE_ROOT?:$TESTCASE_ROOT)(?:EXCLUDE_QUERY_DIR)(?:B?:$TESTCASE_ROOT/_build/default/multi-exes/bin/.c.eobjs/byte)(?:B?:$TESTCASE_ROOT/_build/default/multi-exes/lib/.lib.objs/byte)(?:S?:$TESTCASE_ROOT/multi-exes/bin)(?:S?:$TESTCASE_ROOT/multi-exes/lib)(?:FLG(?:-w?:@1..3@5..28@30..39@43@46..47@49..57@61..62-?:-strict-sequence?:-strict-formats?:-short-paths?:-keep-locs?:-g))(?:UNIT_NAME?:c))
