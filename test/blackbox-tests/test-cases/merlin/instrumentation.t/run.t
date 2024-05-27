  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"
  $ ocamlfind_libs="$(ocamlfind printconf path | while read line; do printf lib=${line}:; done)"
  $ export BUILD_PATH_PREFIX_MAP="$ocamlfind_libs:$BUILD_PATH_PREFIX_MAP"

Here we test that instrumentation processing is not passed to merlin by setting
up a project with instrumentation and testing checking the merlin config.

  $ dune build --instrument-with hello ./lib/.merlin-conf/lib-foo ./lib/.merlin-conf/lib-bar --profile release
  $ dune ocaml merlin dump-config $PWD/lib
  Bar: _build/default/lib/bar
  ((INDEX
    $TESTCASE_ROOT/_build/default/exe/.x.eobjs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/lib/.bar.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/lib/.foo.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/ppx/.hello.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/ppx/.hello_ppx.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT
    $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/lib/.bar.objs/byte)
   (B
    $TESTCASE_ROOT/_build/default/ppx/.hello.objs/byte)
   (S
    $TESTCASE_ROOT/lib)
   (S
    $TESTCASE_ROOT/lib/subdir)
   (S
    $TESTCASE_ROOT/ppx)
   (FLG (-w -40 -g))
   (UNIT_NAME bar))
  Bar: _build/default/lib/bar.ml-gen
  ((INDEX
    $TESTCASE_ROOT/_build/default/exe/.x.eobjs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/lib/.bar.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/lib/.foo.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/ppx/.hello.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/ppx/.hello_ppx.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT
    $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/lib/.bar.objs/byte)
   (B
    $TESTCASE_ROOT/_build/default/ppx/.hello.objs/byte)
   (S
    $TESTCASE_ROOT/lib)
   (S
    $TESTCASE_ROOT/lib/subdir)
   (S
    $TESTCASE_ROOT/ppx)
   (FLG (-w -40 -g))
   (UNIT_NAME bar))
  File: _build/default/lib/subdir/file
  ((INDEX
    $TESTCASE_ROOT/_build/default/exe/.x.eobjs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/lib/.bar.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/lib/.foo.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/ppx/.hello.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/ppx/.hello_ppx.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT
    $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/lib/.bar.objs/byte)
   (B
    $TESTCASE_ROOT/_build/default/ppx/.hello.objs/byte)
   (S
    $TESTCASE_ROOT/lib)
   (S
    $TESTCASE_ROOT/lib/subdir)
   (S
    $TESTCASE_ROOT/ppx)
   (FLG (-open Bar))
   (FLG (-w -40 -g))
   (UNIT_NAME bar__File))
  File: _build/default/lib/subdir/file.ml
  ((INDEX
    $TESTCASE_ROOT/_build/default/exe/.x.eobjs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/lib/.bar.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/lib/.foo.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/ppx/.hello.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/ppx/.hello_ppx.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT
    $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/lib/.bar.objs/byte)
   (B
    $TESTCASE_ROOT/_build/default/ppx/.hello.objs/byte)
   (S
    $TESTCASE_ROOT/lib)
   (S
    $TESTCASE_ROOT/lib/subdir)
   (S
    $TESTCASE_ROOT/ppx)
   (FLG (-open Bar))
   (FLG (-w -40 -g))
   (UNIT_NAME bar__File))
  Foo: _build/default/lib/foo
  ((INDEX
    $TESTCASE_ROOT/_build/default/exe/.x.eobjs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/lib/.bar.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/lib/.foo.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/ppx/.hello.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/ppx/.hello_ppx.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT
    $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/lib/.foo.objs/byte)
   (B
    $TESTCASE_ROOT/_build/default/ppx/.hello.objs/byte)
   (S
    $TESTCASE_ROOT/lib)
   (S
    $TESTCASE_ROOT/lib/subdir)
   (S
    $TESTCASE_ROOT/ppx)
   (FLG (-w -40 -g))
   (UNIT_NAME foo))
  Foo: _build/default/lib/foo.ml-gen
  ((INDEX
    $TESTCASE_ROOT/_build/default/exe/.x.eobjs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/lib/.bar.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/lib/.foo.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/ppx/.hello.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/ppx/.hello_ppx.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT
    $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/lib/.foo.objs/byte)
   (B
    $TESTCASE_ROOT/_build/default/ppx/.hello.objs/byte)
   (S
    $TESTCASE_ROOT/lib)
   (S
    $TESTCASE_ROOT/lib/subdir)
   (S
    $TESTCASE_ROOT/ppx)
   (FLG (-w -40 -g))
   (UNIT_NAME foo))
  Privmod: _build/default/lib/privmod
  ((INDEX
    $TESTCASE_ROOT/_build/default/exe/.x.eobjs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/lib/.bar.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/lib/.foo.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/ppx/.hello.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/ppx/.hello_ppx.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT
    $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/lib/.foo.objs/byte)
   (B
    $TESTCASE_ROOT/_build/default/ppx/.hello.objs/byte)
   (S
    $TESTCASE_ROOT/lib)
   (S
    $TESTCASE_ROOT/lib/subdir)
   (S
    $TESTCASE_ROOT/ppx)
   (FLG (-open Foo))
   (FLG (-w -40 -g))
   (UNIT_NAME foo__Privmod))
  Privmod: _build/default/lib/privmod.ml
  ((INDEX
    $TESTCASE_ROOT/_build/default/exe/.x.eobjs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/lib/.bar.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/lib/.foo.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/ppx/.hello.objs/cctx.ocaml-index)
   (INDEX
    $TESTCASE_ROOT/_build/default/ppx/.hello_ppx.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT
    $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/lib/.foo.objs/byte)
   (B
    $TESTCASE_ROOT/_build/default/ppx/.hello.objs/byte)
   (S
    $TESTCASE_ROOT/lib)
   (S
    $TESTCASE_ROOT/lib/subdir)
   (S
    $TESTCASE_ROOT/ppx)
   (FLG (-open Foo))
   (FLG (-w -40 -g))
   (UNIT_NAME foo__Privmod))
