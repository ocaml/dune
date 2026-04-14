When emitting JS for a same-workspace Melange library, Dune must stage the
transitive implementation closure, even across `.mli` boundaries.

  $ mkdir -p lib app

  $ cat > dune-project <<'EOF'
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

  $ dune rules --deps app/dist/node_modules/repro.foo/foo_map.js \
  >   | sed -n 's#.*\(lib/.foo\.objs/melange/[^)]*\).*#\1#p'
  lib/.foo.objs/melange/foo__Foo_internalAVLtree.cmj
  lib/.foo.objs/melange/foo__Foo_internalMapInt.cmj
  lib/.foo.objs/melange/foo__Foo_map.cmj
  lib/.foo.objs/melange/foo__Foo_mapInt.cmj
