Because INSIDE_DUNE is set for sandboxing Dune does not perform root discovery
in tests. That is why for each call to Dune we specify manually the `root`
directory in these tests.

  $ ln -s realroot linkroot

  $ cd linkroot

  $ dune build

Absolute path with symlinks won't match with Dune's root path in which symlinks
are resolved:
  $ dune ocaml merlin dump-config "$PWD/realsrc" --root="."
  Path $TESTCASE_ROOT/linkroot/realsrc is not in dune workspace ($TESTCASE_ROOT/realroot).

Absolute path with resolved symlinks will match with Dune's root path:
  $ dune ocaml merlin \
  > dump-config "$(pwd | sed 's/linkroot/realroot/')/realsrc" \
  > --root="." | head -n 1
  Foo: _build/default/realsrc/foo


Dune ocaml-merlin also accepts paths relative to the current directory
  $ dune ocaml merlin dump-config "realsrc" --root="." | head -n 1
  Foo: _build/default/realsrc/foo

  $ cd realsrc

  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"

  $ dune ocaml merlin dump-config "." --root=".." | head -n 2
  Foo: _build/default/realsrc/foo
  ((INDEX $TESTCASE_ROOT/realroot/_build/default/realsrc/.foo.eobjs/cctx.ocaml-index) (STDLIB /OCAMLC_WHERE) (SOURCE_ROOT $TESTCASE_ROOT/realroot) (EXCLUDE_QUERY_DIR) (B $TESTCASE_ROOT/realroot/_build/default/realsrc/.foo.eobjs/byte) (S $TESTCASE_ROOT/realroot/realsrc) (FLG (-w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -g)) (UNIT_NAME dune__exe__Foo))
