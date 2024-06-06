  $ dune build @check

  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"

We dump the config for Foo and Bar modules but the pp.exe preprocessor
should appear only once since only Foo is using it.

  $ dune ocaml merlin dump-config $PWD
  Bar: _build/default/bar
  ((INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/pp/.pp.eobjs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (S $TESTCASE_ROOT)
   (FLG (-w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -g))
   (UNIT_NAME bar))
  Bar: _build/default/bar.ml
  ((INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/pp/.pp.eobjs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (S $TESTCASE_ROOT)
   (FLG (-w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -g))
   (UNIT_NAME bar))
  Foo: _build/default/foo
  ((INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/pp/.pp.eobjs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (S $TESTCASE_ROOT)
   (FLG (-pp $TESTCASE_ROOT/_build/default/pp/pp.exe))
   (FLG (-w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -g))
   (UNIT_NAME foo))
  Foo: _build/default/foo.ml
  ((INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/pp/.pp.eobjs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (S $TESTCASE_ROOT)
   (FLG (-pp $TESTCASE_ROOT/_build/default/pp/pp.exe))
   (FLG (-w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -g))
   (UNIT_NAME foo))
