  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"

  $ dune build foo.cma --profile release
  $ dune ocaml merlin dump-config $PWD
  Foo: _build/default/foo
  ((INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index) (STDLIB /OCAMLC_WHERE) (SOURCE_ROOT $TESTCASE_ROOT) (EXCLUDE_QUERY_DIR) (B $TESTCASE_ROOT/_build/default/.foo.objs/byte) (S $TESTCASE_ROOT) (FLG (-w -40 -g)) (UNIT_NAME foo))
  Foo: _build/default/foo.ml-gen
  ((INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index) (STDLIB /OCAMLC_WHERE) (SOURCE_ROOT $TESTCASE_ROOT) (EXCLUDE_QUERY_DIR) (B $TESTCASE_ROOT/_build/default/.foo.objs/byte) (S $TESTCASE_ROOT) (FLG (-w -40 -g)) (UNIT_NAME foo))

  $ rm -f .merlin
  $ dune build foo.cma --profile release
  $ dune ocaml merlin dump-config $PWD
  Foo: _build/default/foo
  ((INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index) (STDLIB /OCAMLC_WHERE) (SOURCE_ROOT $TESTCASE_ROOT) (EXCLUDE_QUERY_DIR) (B $TESTCASE_ROOT/_build/default/.foo.objs/byte) (S $TESTCASE_ROOT) (FLG (-w -40 -g)) (UNIT_NAME foo))
  Foo: _build/default/foo.ml-gen
  ((INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index) (STDLIB /OCAMLC_WHERE) (SOURCE_ROOT $TESTCASE_ROOT) (EXCLUDE_QUERY_DIR) (B $TESTCASE_ROOT/_build/default/.foo.objs/byte) (S $TESTCASE_ROOT) (FLG (-w -40 -g)) (UNIT_NAME foo))

  $ echo toto > .merlin
  $ dune build foo.cma --profile release
  $ dune ocaml merlin dump-config $PWD
  Foo: _build/default/foo
  ((INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index) (STDLIB /OCAMLC_WHERE) (SOURCE_ROOT $TESTCASE_ROOT) (EXCLUDE_QUERY_DIR) (B $TESTCASE_ROOT/_build/default/.foo.objs/byte) (S $TESTCASE_ROOT) (FLG (-w -40 -g)) (UNIT_NAME foo))
  Foo: _build/default/foo.ml-gen
  ((INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index) (STDLIB /OCAMLC_WHERE) (SOURCE_ROOT $TESTCASE_ROOT) (EXCLUDE_QUERY_DIR) (B $TESTCASE_ROOT/_build/default/.foo.objs/byte) (S $TESTCASE_ROOT) (FLG (-w -40 -g)) (UNIT_NAME foo))
