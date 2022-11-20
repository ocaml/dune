  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"

We call `$(opam switch show)` so that this test always uses an existing switch

  $ cat >dune-workspace <<EOF
  > (lang dune 2.0)
  > (context (default))
  > (context
  > (opam
  >  (name cross)
  >  (switch $(opam switch show))
  >  (merlin)))
  > EOF

  $ dune build

  $ ls -a _build/cross/.merlin-conf
  .
  ..
  lib-foo

  $ dune ocaml-merlin --dump-config="$PWD"
  Foo
  ((STDLIB /OCAMLC_WHERE)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/cross/.foo.objs/byte)
   (S
    $TESTCASE_ROOT)
   (FLG
    (-w
     @1..3@5..28@30..39@43@46..47@49..57@61..62-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs)))
