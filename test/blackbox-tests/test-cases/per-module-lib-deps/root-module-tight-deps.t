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

  $ ROOT_CMI=_build/default/consumer_lib/.consumer_lib.objs/byte/consumer_lib__Root.cmi
  $ dune build $ROOT_CMI

The root.cmi rule's deps include a glob over [dep_lib]'s objdir
for cmis. A future regression that omits the dep entirely
(returning empty deps for root from the per-module filter)
would remove this glob and fail the assertion; a future
refinement narrower than the glob would flip the snapshot and
require deliberate promotion.

The compiler-binary dep (a [(File (External ...))] entry whose
sexp shape varies with path length across opam/Nix builds) is
filtered out; we keep the [root.ml-gen] file dep and the glob.

  $ dune rules --deps $ROOT_CMI 2>&1 \
  >   | dune_cmd print-from 'In_build_dir _build/default/consumer_lib/root\.ml-gen' \
  >   | dune_cmd print-until '\)\)\)\)$'
   (File (In_build_dir _build/default/consumer_lib/root.ml-gen))
   (glob
    ((dir (In_build_dir _build/default/dep_lib/.dep_lib.objs/byte))
     (predicate *.cmi)
     (only_generated_files false))))
