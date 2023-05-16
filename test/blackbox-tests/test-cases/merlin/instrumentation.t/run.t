  $ ocamlc_where="$(ocamlc -where)"
  $ ENCODED_OCAMLC_WHERE=$(dune_cmd encode-prefix "$ocamlc_where")
  $ export BUILD_PATH_PREFIX_MAP=\
  > "/OCAMLC_WHERE=$ENCODED_OCAMLC_WHERE:$BUILD_PATH_PREFIX_MAP"
  $ for path in $(ocamlfind printconf path)
  > do
  > EPATH=$(dune_cmd encode-prefix "$path")
  > export BUILD_PATH_PREFIX_MAP="/LIB=$EPATH:$BUILD_PATH_PREFIX_MAP"
  > done

Here we test that instrumentation processing is not passed to merlin by setting
up a project with instrumentation and testing checking the merlin config.

  $ dune build --instrument-with hello ./lib/.merlin-conf/lib-foo ./lib/.merlin-conf/lib-bar --profile release
  $ dune ocaml merlin dump-config $PWD/lib
  Bar
  ((STDLIB /workspace_root/lib/ocaml)
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
  ((STDLIB /workspace_root/lib/ocaml)
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
  ((STDLIB /workspace_root/lib/ocaml)
   (EXCLUDE_QUERY_DIR)
   (B /workspace_root/lib/findlib)
   (B /workspace_root/lib/ocaml)
   (B
    $TESTCASE_ROOT/_build/default/lib/.foo.objs/byte)
   (B
    $TESTCASE_ROOT/_build/default/ppx/.hello.objs/byte)
   (S /workspace_root/lib/findlib)
   (S /workspace_root/lib/ocaml)
   (S
    $TESTCASE_ROOT/lib)
   (S
    $TESTCASE_ROOT/lib/subdir)
   (S
    $TESTCASE_ROOT/ppx)
   (FLG (-w -40 -g)))
  Privmod
  ((STDLIB /workspace_root/lib/ocaml)
   (EXCLUDE_QUERY_DIR)
   (B /workspace_root/lib/findlib)
   (B /workspace_root/lib/ocaml)
   (B
    $TESTCASE_ROOT/_build/default/lib/.foo.objs/byte)
   (B
    $TESTCASE_ROOT/_build/default/ppx/.hello.objs/byte)
   (S /workspace_root/lib/findlib)
   (S /workspace_root/lib/ocaml)
   (S
    $TESTCASE_ROOT/lib)
   (S
    $TESTCASE_ROOT/lib/subdir)
   (S
    $TESTCASE_ROOT/ppx)
   (FLG (-open Foo))
   (FLG (-w -40 -g)))
