Baseline: multi-module consumer library with a sibling module that
references no module of its declared dependency library.

This is an observational test. It records the number of rebuild
targets for the consumer's [spurious_rebuild] module when a module
of the dependency library [dep_lib] has its interface edited.

[consumer_lib] is an unwrapped library with two modules.
[consumes_dep] references [Referenced_dep]; [spurious_rebuild]
references nothing from [dep_lib]. On current main, editing
[referenced_dep]'s interface rebuilds [spurious_rebuild] even
though it references nothing from [dep_lib]: the consumer depends
on a glob over [dep_lib]'s object directory, which is invalidated
by the cmi change.

The zero-reference-sibling-in-a-library corner is distinct from
scenarios covered by existing tests: [lib-to-lib-unwrapped.t] probes
siblings that reference a different (non-edited) module of the dep;
[transitive.t] and [unwrapped.t] probe zero-reference modules but
within executable stanzas, not library stanzas. A future fix
implementing library-level dep filtering at consumer-module
granularity would drop [dep_lib] entirely from [spurious_rebuild]'s
deps, tightening this corner to zero rebuilds.

See: https://github.com/ocaml/dune/issues/4572
See: https://github.com/ocaml/dune/pull/14116#issuecomment-4301275263

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name dep_lib)
  >  (wrapped false)
  >  (modules unused_module referenced_dep))
  > (library
  >  (name consumer_lib)
  >  (wrapped false)
  >  (modules consumes_dep spurious_rebuild)
  >  (libraries dep_lib))
  > EOF

  $ cat > unused_module.ml <<EOF
  > let x = 42
  > EOF
  $ cat > referenced_dep.ml <<EOF
  > let x = 43
  > EOF
  $ cat > referenced_dep.mli <<EOF
  > val x : int
  > EOF
  $ cat > consumes_dep.ml <<EOF
  > let x = Referenced_dep.x
  > EOF
  $ cat > spurious_rebuild.ml <<EOF
  > let x = 12
  > EOF

  $ dune build @check

Edit [referenced_dep]'s interface (add a binding to both [.mli]
and [.ml] so the [.cmi] content actually changes). [spurious_rebuild]
references nothing from [dep_lib], so its compile rule should not
fire — no rule with a [spurious_rebuild]-named target re-runs:

  $ cat > referenced_dep.mli <<EOF
  > val x : int
  > val y : string
  > EOF
  $ cat > referenced_dep.ml <<EOF
  > let x = 43
  > let y = "hello"
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("spurious_rebuild"))]'
  []
