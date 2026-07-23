When emitting JS for an installed Melange library, Dune must conservatively
stage the installed Melange object dirs rather than only the entry module's
`.cmj`.

  $ mkdir -p lib app prefix

  $ cat > lib/dune-project <<'EOF'
  > (lang dune 3.20)
  > (using melange 0.1)
  > (package (name repro))
  > EOF

  $ write_melange_private_map_library

  $ dune build --root lib @install

  $ dune install --root lib --prefix $PWD/prefix --display short
  Installing $TESTCASE_ROOT/prefix/lib/repro/META
  Installing $TESTCASE_ROOT/prefix/lib/repro/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo__.cmi
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo__.cmj
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo__.cmt
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo__.ml
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo__Foo_internalAVLtree.cmi
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo__Foo_internalAVLtree.cmj
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo__Foo_internalAVLtree.cmt
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo__Foo_internalMapInt.cmi
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo__Foo_internalMapInt.cmj
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo__Foo_internalMapInt.cmt
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo__Foo_internalMapInt.cmti
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo__Foo_map.cmi
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo__Foo_map.cmj
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo__Foo_map.cmt
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo__Foo_map.cmti
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo__Foo_mapInt.cmi
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo__Foo_mapInt.cmj
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo__Foo_mapInt.cmt
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo__Foo_mapInt.cmti
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo_internalAVLtree.ml
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo_internalMapInt.ml
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo_internalMapInt.mli
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo_map.ml
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo_map.mli
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo_mapInt.ml
  Installing $TESTCASE_ROOT/prefix/lib/repro/foo/melange/foo_mapInt.mli

  $ cat > app/dune-project <<'EOF'
  > (lang dune 3.20)
  > (using melange 0.1)
  > (package (name app))
  > EOF

  $ write_melange_repro_foo_consumer

  $ OCAMLPATH=$PWD/prefix/lib:$OCAMLPATH dune rules --format=json --deps --root app --display=quiet dist/node_modules/repro.foo/foo_map.js > deps.json
  $ jq_dune -r '.[] | depsGlobEntriesWithPredicate("*.cmj") | select(.dir | endswith("/repro/foo")) | "\(.dir_kind) \(.dir)"' deps.json
  External $TESTCASE_ROOT/prefix/lib/repro/foo
  $ jq_dune -r '.[] | depsGlobEntriesWithPredicate("*.cmi") | select(.dir | endswith("/repro/foo")) | "\(.dir_kind) \(.dir)"' deps.json
  External $TESTCASE_ROOT/prefix/lib/repro/foo
