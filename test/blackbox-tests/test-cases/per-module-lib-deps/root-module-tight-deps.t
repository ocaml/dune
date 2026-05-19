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

The root module's cmi is the specific artefact whose dep
behaviour is under test. Building just that artefact narrows
the success criterion to the root.cmi rule itself; a build of
[@check] could succeed for other reasons.

  $ dune build '%{cmi:consumer_lib/root}'

The root.cmi rule's deps include a glob over [dep_lib]'s objdir
for cmis. A future regression that omits the dep entirely
(returning empty deps for root from the per-module filter)
would remove this glob and fail the assertion; a future
refinement narrower than the glob would flip the snapshot and
require deliberate promotion.

  $ dune rules --root . --format=json --deps '%{cmi:consumer_lib/root}' > deps.json

  $ jq -r 'include "dune"; .[] | depsFilePaths | select(endswith("root.ml-gen"))' < deps.json
  _build/default/consumer_lib/root.ml-gen

  $ jq -r 'include "dune"; .[] | depsGlobs
  >   | select(.dir | endswith("dep_lib/.dep_lib.objs/byte"))
  >   | .dir + " " + .predicate' < deps.json
  _build/default/dep_lib/.dep_lib.objs/byte *.cmi
