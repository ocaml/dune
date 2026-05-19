When emitting JS for an installed Melange library, Dune must conservatively
stage the installed Melange object dirs rather than only the entry module's
`.cmj`.

  $ mkdir -p lib app prefix

  $ cat > lib/dune-project <<'EOF'
  > (lang dune 3.20)
  > (using melange 0.1)
  > (package (name repro))
  > EOF

  $ cat > lib/dune <<'EOF'
  > (library
  >  (name foo)
  >  (public_name repro.foo)
  >  (wrapped true)
  >  (libraries melange)
  >  (modes melange))
  > EOF

  $ cat > lib/foo.ml <<'EOF'
  > module Foo_map = Foo_map
  > EOF

  $ cat > lib/foo_map.mli <<'EOF'
  > module Int = Foo_mapInt
  > val size : Int.t -> int
  > EOF

  $ cat > lib/foo_map.ml <<'EOF'
  > module Int = Foo_mapInt
  > let size x = Int.size x
  > EOF

  $ cat > lib/foo_mapInt.mli <<'EOF'
  > type t
  > val size : t -> int
  > EOF

  $ cat > lib/foo_mapInt.ml <<'EOF'
  > type t = Foo_internalMapInt.t
  > let size x = Foo_internalMapInt.size x
  > EOF

  $ cat > lib/foo_internalMapInt.mli <<'EOF'
  > type t
  > val size : t -> int
  > EOF

  $ cat > lib/foo_internalMapInt.ml <<'EOF'
  > type t = int array
  > let size x = Foo_internalAVLtree.size x
  > EOF

  $ cat > lib/foo_internalAVLtree.ml <<'EOF'
  > let size x = Array.length x
  > EOF

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

  $ cat > app/dune <<'EOF'
  > (melange.emit
  >  (target dist)
  >  (alias mel)
  >  (emit_stdlib false)
  >  (libraries repro.foo))
  > EOF

  $ cat > app/main.ml <<'EOF'
  > let () = ignore (Foo.Foo_map.Int.size (Obj.magic 0))
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib:$OCAMLPATH dune rules --format=json --deps --root app --display=quiet dist/node_modules/repro.foo/foo_map.js > deps.json
  $ jq -r 'include "dune"; .[] | depsGlobEntriesWithPredicate("*.cmj") | select(.dir | endswith("/repro/foo")) | "\(.dir_kind) \(.dir)"' deps.json
  External $TESTCASE_ROOT/prefix/lib/repro/foo
  $ jq -r 'include "dune"; .[] | depsGlobEntriesWithPredicate("*.cmi") | select(.dir | endswith("/repro/foo")) | "\(.dir_kind) \(.dir)"' deps.json
  External $TESTCASE_ROOT/prefix/lib/repro/foo
