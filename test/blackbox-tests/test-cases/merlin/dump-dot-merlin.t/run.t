  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"

  $ dune build
  $ dune ocaml dump-dot-merlin src\ with\ spaces
  EXCLUDE_QUERY_DIR
  STDLIB /OCAMLC_WHERE
  B $TESTCASE_ROOT/_build/default/src with spaces/.foo.eobjs/byte
  S $TESTCASE_ROOT/src with spaces
  # FLG -pp ''\''$TESTCASE_ROOT/_build/default/p p/pp.exe'\'''
  # FLG -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs
  
  $ dune ocaml dump-dot-merlin "p p"
  EXCLUDE_QUERY_DIR
  STDLIB /OCAMLC_WHERE
  B $TESTCASE_ROOT/_build/default/p p/.pp.eobjs/byte
  S $TESTCASE_ROOT/p p
  # FLG -w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs
  
