  $ ocamlc_where="$(ocamlc -where)"
  $ ENCODED_OCAMLC_WHERE=$(dune_cmd encode-prefix "$ocamlc_where")
  $ export BUILD_PATH_PREFIX_MAP=\
  > "/OCAMLC_WHERE=$ENCODED_OCAMLC_WHERE:$BUILD_PATH_PREFIX_MAP"

  $ dune build foo.cma --profile release
  $ dune ocaml merlin dump-config $PWD
  Foo
  ((STDLIB /workspace_root/lib/ocaml)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (S
    $TESTCASE_ROOT)
   (FLG (-w -40 -g)))

  $ rm -f .merlin
  $ dune build foo.cma --profile release
  $ dune ocaml merlin dump-config $PWD
  Foo
  ((STDLIB /workspace_root/lib/ocaml)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (S
    $TESTCASE_ROOT)
   (FLG (-w -40 -g)))

  $ echo toto > .merlin
  $ dune build foo.cma --profile release
  $ dune ocaml merlin dump-config $PWD
  Foo
  ((STDLIB /workspace_root/lib/ocaml)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (S
    $TESTCASE_ROOT)
   (FLG (-w -40 -g)))
