A multi-module consumer depends on a `(wrapped false)` singleton
library `bridge` that itself has `(libraries leaf)`. The singleton's
module exposes a value typed `Mod_leaf.t`; the consumer reads it
through `Mod_bridge.v` without naming `Mod_leaf` in source. Pins
that this transitive shape both builds and surfaces `leaf` to the
consumer's compile rule.

Sister to `no-ocamldep-leaf-lib.t`, which covers the
singleton-with-no-requires case.

  $ make_dune_project 3.24

`leaf`: transitive dep reached only through `bridge`. The consumer
never names `Mod_leaf` directly.

  $ mkdir leaf
  $ cat > leaf/dune <<EOF
  > (library (name leaf) (wrapped false))
  > EOF
  $ cat > leaf/mod_leaf.ml <<EOF
  > type t = int
  > let v : t = 1
  > EOF
  $ cat > leaf/mod_leaf.mli <<EOF
  > type t = int
  > val v : t
  > EOF

`bridge`: `(wrapped false)` singleton with `(libraries leaf)`. Its
interface exposes `Mod_leaf.t`, so a consumer reading
`Mod_bridge.cmi` also needs `Mod_leaf.cmi` on the include path.

  $ mkdir bridge
  $ cat > bridge/dune <<EOF
  > (library (name bridge) (wrapped false) (libraries leaf))
  > EOF
  $ cat > bridge/mod_bridge.ml <<EOF
  > let v : Mod_leaf.t = Mod_leaf.v
  > EOF
  $ cat > bridge/mod_bridge.mli <<EOF
  > val v : Mod_leaf.t
  > EOF

`consumer`: multi-module. Only `uses_bridge` names `Mod_bridge`;
`leaf` reaches the consumer's compile scope only via `bridge`.

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (library (name consumer) (wrapped false) (libraries bridge))
  > EOF
  $ cat > consumer/uses_bridge.ml <<EOF
  > let _ = Mod_bridge.v
  > EOF
  $ cat > consumer/sibling.ml <<EOF
  > let _ = ()
  > EOF

  $ dune build @check

Case 1: `uses_bridge` tracks both `bridge` and `leaf`. The compile
rule must surface `Mod_leaf.cmi` — reached transitively through
`Mod_bridge.cmi`'s interface, never named in consumer source.

  $ dune rules --root . --format=json --deps '%{cmo:consumer/uses_bridge}' > deps_uses.json
  $ jq -r 'include "dune"; .[] | depsGlobs
  >   | select(.dir | endswith("bridge/.bridge.objs/byte"))
  >   | .dir + " " + .predicate' < deps_uses.json
  _build/default/bridge/.bridge.objs/byte *.cmi
  $ jq -r 'include "dune"; .[] | depsGlobs
  >   | select(.dir | endswith("leaf/.leaf.objs/byte"))
  >   | .dir + " " + .predicate' < deps_uses.json
  _build/default/leaf/.leaf.objs/byte *.cmi
  $ jq -r 'include "dune"; .[] | depsFilePaths
  >   | select(endswith("bridge/.bridge.objs/byte/mod_bridge.cmi"))' < deps_uses.json
  $ jq -r 'include "dune"; .[] | depsFilePaths
  >   | select(endswith("leaf/.leaf.objs/byte/mod_leaf.cmi"))' < deps_uses.json

Case 2: `sibling` references neither lib in source.

  $ dune rules --root . --format=json --deps '%{cmo:consumer/sibling}' > deps_sib.json
  $ jq -r 'include "dune"; .[] | depsGlobs
  >   | select(.dir | endswith("bridge/.bridge.objs/byte"))
  >   | .dir + " " + .predicate' < deps_sib.json
  _build/default/bridge/.bridge.objs/byte *.cmi
  $ jq -r 'include "dune"; .[] | depsGlobs
  >   | select(.dir | endswith("leaf/.leaf.objs/byte"))
  >   | .dir + " " + .predicate' < deps_sib.json
  _build/default/leaf/.leaf.objs/byte *.cmi
