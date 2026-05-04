Multi-module consumer library with a sibling module that references
no module of its declared dependency library.

[consumer_lib] is an unwrapped library with two modules.
[consumes_dep] references [Referenced_dep]; [spurious_rebuild]
references nothing from [dep_lib]. On trunk, editing
[referenced_dep]'s interface rebuilds [spurious_rebuild] too — the
consumer's compile rule depends on a glob over [dep_lib]'s objdir,
invalidated on any [.cmi] change. The per-module ocamldep filter
introduced by this commit observes that [spurious_rebuild]
references no [dep_lib] module and drops [dep_lib] entirely from
its compile-rule deps; no [spurious_rebuild]-named target re-runs.

The zero-reference-sibling-in-a-library corner is distinct from
scenarios covered by existing tests: [lib-to-lib-unwrapped.t] probes
siblings that reference a different (non-edited) module of the dep;
[transitive.t] and [unwrapped.t] probe zero-reference modules but
within executable stanzas, not library stanzas.

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

Edit [referenced_dep]'s interface (add a binding to both [.mli] and
[.ml] so the [.cmi] content actually changes). [spurious_rebuild]
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
