  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"

  $ FILE=$PWD/main.ml
  $ query_ocaml_merlin_pp "$FILE"
  ((ERROR "No config found for file main.ml. Try calling 'dune build'."))

  $ dune build @check

  $ query_ocaml_merlin_pp "$FILE"
  ((INDEX $TESTCASE_ROOT/_build/default/.not-a-module-name.eobjs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.main.eobjs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.mylib3.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.mylib.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/.main.eobjs/byte)
   (B $TESTCASE_ROOT/_build/default/.mylib.objs/byte)
   (B $TESTCASE_ROOT/_build/default/.mylib3.objs/byte)
   (S $TESTCASE_ROOT)
   (FLG
    (-w
     @1..3@5..28@30..39@43@46..47@49..57@61..62-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs
     -g))
   (FLG
    (-open Dune__exe))
   (UNIT_NAME dune__exe__Main))

  $ FILE=$PWD/lib3.ml
  $ query_ocaml_merlin_pp "$FILE"
  ((INDEX $TESTCASE_ROOT/_build/default/.not-a-module-name.eobjs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.main.eobjs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.mylib3.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.mylib.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/.mylib.objs/byte)
   (B $TESTCASE_ROOT/_build/default/.mylib3.objs/byte)
   (S $TESTCASE_ROOT)
   (FLG
    (-w
     @1..3@5..28@30..39@43@46..47@49..57@61..62-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs
     -g))
   (FLG
    (-open Mylib3))
   (UNIT_NAME mylib3__Lib3))

If a file has a name of the kind `module_name.xx.xxx.ml/i`
we consider it as ``module_name.ml/i`
This can be useful when some build scripts perform custom
preprocessing and copy files around.
  $ FILE=lib3.foobar.ml
  $ query_ocaml_merlin_pp "$FILE"
  ((INDEX $TESTCASE_ROOT/_build/default/.not-a-module-name.eobjs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.main.eobjs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.mylib3.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.mylib.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/.mylib.objs/byte)
   (B $TESTCASE_ROOT/_build/default/.mylib3.objs/byte)
   (S $TESTCASE_ROOT)
   (FLG
    (-w
     @1..3@5..28@30..39@43@46..47@49..57@61..62-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs
     -g))
   (FLG
    (-open Mylib3))
   (UNIT_NAME mylib3__Lib3))

If a directory has no configuration the configuration of its parent is used
This can be useful when some build scripts copy files from subdirectories.
  $ FILE=foobar/lib3.foobar.ml
  $ query_ocaml_merlin_pp "$FILE"
  ((INDEX $TESTCASE_ROOT/_build/default/.not-a-module-name.eobjs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.main.eobjs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.mylib3.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.mylib.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/.mylib.objs/byte)
   (B $TESTCASE_ROOT/_build/default/.mylib3.objs/byte)
   (S $TESTCASE_ROOT)
   (FLG
    (-w
     @1..3@5..28@30..39@43@46..47@49..57@61..62-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs
     -g))
   (FLG
    (-open Mylib3))
   (UNIT_NAME mylib3__Lib3))

Test of an valid invalid module name
  $ FILE=not-a-module-name.ml
  $ query_ocaml_merlin_pp "$FILE"
  ((INDEX $TESTCASE_ROOT/_build/default/.not-a-module-name.eobjs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.main.eobjs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.mylib3.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.mylib.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/.not-a-module-name.eobjs/byte)
   (S $TESTCASE_ROOT)
   (FLG
    (-w
     @1..3@5..28@30..39@43@46..47@49..57@61..62-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs
     -w
     -24
     -g))
   (UNIT_NAME dune__exe__Not-a-module-name))

Dune should also provide configuration when the file is in the build folder
  $ FILE=$PWD/_build/default/lib3.ml
  $ query_ocaml_merlin_pp "$FILE"
  ((INDEX $TESTCASE_ROOT/_build/default/.not-a-module-name.eobjs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.main.eobjs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.mylib3.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.mylib.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/.mylib.objs/byte)
   (B $TESTCASE_ROOT/_build/default/.mylib3.objs/byte)
   (S $TESTCASE_ROOT)
   (FLG
    (-w
     @1..3@5..28@30..39@43@46..47@49..57@61..62-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs
     -g))
   (FLG
    (-open Mylib3))
   (UNIT_NAME mylib3__Lib3))

  $ FILE=_build/default/lib3.ml
  $ query_ocaml_merlin_pp "$FILE"
  ((INDEX $TESTCASE_ROOT/_build/default/.not-a-module-name.eobjs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.main.eobjs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.mylib3.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.mylib.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B $TESTCASE_ROOT/_build/default/.mylib.objs/byte)
   (B $TESTCASE_ROOT/_build/default/.mylib3.objs/byte)
   (S $TESTCASE_ROOT)
   (FLG
    (-w
     @1..3@5..28@30..39@43@46..47@49..57@61..62-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs
     -g))
   (FLG
    (-open Mylib3))
   (UNIT_NAME mylib3__Lib3))
