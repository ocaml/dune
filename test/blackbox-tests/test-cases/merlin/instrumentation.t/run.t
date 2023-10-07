  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"
  $ ocamlfind_libs="$(ocamlfind printconf path | while read line; do printf lib=${line}:; done)"
  $ export BUILD_PATH_PREFIX_MAP="$ocamlfind_libs:$BUILD_PATH_PREFIX_MAP"

Here we test that instrumentation processing is not passed to merlin by setting
up a project with instrumentation and testing checking the merlin config.

  $ dune build --instrument-with hello ./lib/.merlin-conf/lib-foo ./lib/.merlin-conf/lib-bar --profile release
  $ dune ocaml merlin dump-config $PWD/lib
  Bar
  ((STDLIB /OCAMLC_WHERE)
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
   (FLG (-w -40 -g)))
  File
  ((STDLIB /OCAMLC_WHERE)
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
   (FLG (-w -40 -g)))
  Foo
  ((STDLIB /OCAMLC_WHERE)
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
   (FLG (-w -40 -g)))
  Privmod
  ((STDLIB /OCAMLC_WHERE)
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
   (FLG (-w -40 -g)))
