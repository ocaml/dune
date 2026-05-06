A library declaring [(root_module ...)] alongside [(libraries
...)] must build correctly when its modules reference the dep
library through the Root alias.

Root modules carry no ocamldep dep-rule: the kind matches
[Root | Alias _] in [Dep_rules.read_transitive_deps_of_module]
and [read_immediate_deps_of], both of which short-circuit to
empty deps (avoiding the cycle that would otherwise arise from a
Root that references itself transitively). Any future change to
inter-library dep handling that misses this short-circuit would
regress this scenario.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

[dep_lib]: an unwrapped dep library.

  $ mkdir dep_lib
  $ cat > dep_lib/dune <<EOF
  > (library (name dep_lib) (wrapped false))
  > EOF
  $ cat > dep_lib/dep_lib.ml <<EOF
  > let v = 1
  > EOF
  $ cat > dep_lib/dep_lib.mli <<EOF
  > val v : int
  > EOF

[consumer_lib]: declares [(root_module root) (libraries dep_lib)]
and references [Dep_lib] through [Root].

  $ mkdir consumer_lib
  $ cat > consumer_lib/dune <<EOF
  > (library
  >  (name consumer_lib)
  >  (libraries dep_lib)
  >  (root_module root))
  > EOF
  $ cat > consumer_lib/m.ml <<EOF
  > let _ = Root.Dep_lib.v
  > EOF

  $ dune build @check
