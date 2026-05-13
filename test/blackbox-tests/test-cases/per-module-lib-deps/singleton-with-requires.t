A multi-module consumer of a [(wrapped false)] singleton dep that
itself has [(libraries leaf)] builds. The cross-library walker
propagates through the singleton via ocamldep on its module so
[leaf]'s cmi stays reachable from the consumer's compile.

Sister to [no-ocamldep-leaf-lib.t]: that test covers the
singleton-with-no-requires (walker-terminal) case. This one covers
the singleton-with-requires case, where the walker must run
ocamldep on the singleton's module to discover its transitive refs.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

[leaf]: transitive dep reached only through [bridge]'s ocamldep
output. The consumer never names [Mod_leaf] directly.

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

[bridge]: [(wrapped false)] singleton with [(libraries leaf)]. Its
interface exposes [Mod_leaf.t], so a consumer reading
[Mod_bridge.cmi] also needs [Mod_leaf.cmi] on the include path.

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

[consumer]: multi-module ([can_filter = true]). Only [uses_bridge]
names [Mod_bridge]; [leaf] reaches the consumer's compile scope
only via the BFS walker stepping through [bridge].

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
