  $ export BUILD_PATH_PREFIX_MAP=\
  > "OPAM_PREFIX=$(ocamlc -where):$BUILD_PATH_PREFIX_MAP"

If Merlin field is absent, default context is chosen

  $ cat >dune-workspace <<EOF
  > (lang dune 2.9)
  > (context (default))
  > (context
  >  (default
  >   (name cross)))
  > EOF

  $ dune build

The rules are generated for all contexts, regardless which one is chosen

  $ ls -a _build/cross/.merlin-conf
  .
  ..
  lib-foo

  $ ls -a _build/default/.merlin-conf
  .
  ..
  lib-foo

But the default context is used by default

  $ dune ocaml merlin dump-config "$PWD"
  Foo: _build/default/foo
  ((STDLIB OPAM_PREFIX)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (S
    $TESTCASE_ROOT)
   (FLG
    (-w
     @1..3@5..28@30..39@43@46..47@49..57@61..62-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs
     -g)))

If Merlin field is present, this context is chosen

  $ cat >dune-workspace <<EOF
  > (lang dune 2.8)
  > (context (default))
  > (context
  >  (default
  >   (name cross)
  >   (merlin)))
  > EOF

  $ dune build

  $ ls -a _build/cross/.merlin-conf
  .
  ..
  lib-foo

  $ ls -a _build/default/.merlin-conf
  .
  ..
  lib-foo

  $ dune ocaml merlin dump-config "$PWD"
  Foo: _build/cross/foo
  ((STDLIB OPAM_PREFIX)
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
     -keep-locs
     -g)))
