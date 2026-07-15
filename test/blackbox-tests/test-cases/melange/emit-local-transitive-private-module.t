When emitting JS for a same-workspace Melange library, Dune must stage the
transitive implementation closure, even across `.mli` boundaries.

  $ mkdir -p lib app

  $ cat > dune-project <<'EOF'
  > (lang dune 3.20)
  > (using melange 0.1)
  > (package (name repro))
  > EOF

  $ write_melange_private_map_library

  $ write_melange_repro_foo_consumer

  $ dune rules --root . --format=json --deps app/dist/node_modules/repro.foo/foo_map.js |
  > jq_dune -r '.[] | depsFilePaths | select(test("lib/\\.foo\\.objs/melange/.*\\.cmj$")) | sub("^_build/default/"; "")'
  lib/.foo.objs/melange/foo__Foo_internalAVLtree.cmj
  lib/.foo.objs/melange/foo__Foo_internalMapInt.cmj
  lib/.foo.objs/melange/foo__Foo_map.cmj
  lib/.foo.objs/melange/foo__Foo_mapInt.cmj
