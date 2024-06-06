  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"

  $ dune exec ./foo.exe
  42

  $ dune ocaml merlin dump-config $PWD
  Foo: _build/default/foo
  ((INDEX $TESTCASE_ROOT/_build/default/.foo.eobjs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/foo/.foo.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/.foo.eobjs/byte)
   (B $TESTCASE_ROOT/_build/default/foo/.foo.objs/byte)
   (S $TESTCASE_ROOT)
   (S $TESTCASE_ROOT/foo)
   (FLG (-w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -g))
   (UNIT_NAME dune__exe__Foo))
  Foo: _build/default/foo.ml
  ((INDEX $TESTCASE_ROOT/_build/default/.foo.eobjs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/foo/.foo.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/.foo.eobjs/byte)
   (B $TESTCASE_ROOT/_build/default/foo/.foo.objs/byte)
   (S $TESTCASE_ROOT)
   (S $TESTCASE_ROOT/foo)
   (FLG (-w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -g))
   (UNIT_NAME dune__exe__Foo))

  $ dune ocaml merlin dump-config $PWD/foo
  Bar: _build/default/foo/bar
  ((INDEX $TESTCASE_ROOT/_build/default/.foo.eobjs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/foo/.foo.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/foo/.foo.objs/byte)
   (S $TESTCASE_ROOT/foo)
   (FLG (-open Foo))
   (FLG (-w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -g))
   (UNIT_NAME foo__Bar))
  Bar: _build/default/foo/bar.ml
  ((INDEX $TESTCASE_ROOT/_build/default/.foo.eobjs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/foo/.foo.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/foo/.foo.objs/byte)
   (S $TESTCASE_ROOT/foo)
   (FLG (-open Foo))
   (FLG (-w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -g))
   (UNIT_NAME foo__Bar))
  Foo: _build/default/foo/foo
  ((INDEX $TESTCASE_ROOT/_build/default/.foo.eobjs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/foo/.foo.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/foo/.foo.objs/byte)
   (S $TESTCASE_ROOT/foo)
   (FLG (-w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -g))
   (UNIT_NAME foo))
  Foo: _build/default/foo/foo.ml-gen
  ((INDEX $TESTCASE_ROOT/_build/default/.foo.eobjs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/foo/.foo.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/foo/.foo.objs/byte)
   (S $TESTCASE_ROOT/foo)
   (FLG (-w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -g))
   (UNIT_NAME foo))

FIXME : module Foo is not unbound
This test is disabled because it depends on root detection and is not reproducible.
$ ocamlmerlin single errors -filename foo.ml < foo.ml | jq ".value.message"
"Unbound module Foo"
