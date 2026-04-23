Baseline: single-module consumer library whose only module references
no module of its declared dependency library.

This is an observational test. It records the number of rebuild
targets for the consumer's [spurious_rebuild] module when the
dependency library [dep_lib]'s only module has its interface
edited.

[consumer_lib] declares [(libraries dep_lib)] but
[spurious_rebuild.ml] does not reference any module of [dep_lib].
On current main this scenario still rebuilds [spurious_rebuild]:
[consumer_lib] is a single-module stanza, so dune skips ocamldep
as an optimisation and cannot discover that [spurious_rebuild]
references no module of [dep_lib]; the consumer falls back to a
glob over [dep_lib]'s object directory, which is invalidated by
the cmi change.

The zero-reference case is a distinct corner from the single-module
consumer that references some (but not all) modules of its dep,
which [single-module-lib.t] already documents. A future fix that
detects "ocamldep yields no references to dep_lib" could tighten
this corner to zero rebuilds without needing to solve the broader
single-module-consumer skip-ocamldep limitation.

See: https://github.com/ocaml/dune/issues/4572
See: https://github.com/ocaml/dune/pull/14116#issuecomment-4286949811

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ cat > dune <<EOF
  > (library (name dep_lib) (wrapped false) (modules dep_module))
  > (library
  >  (name consumer_lib)
  >  (wrapped false)
  >  (modules spurious_rebuild)
  >  (libraries dep_lib))
  > EOF

  $ cat > dep_module.ml <<EOF
  > let x = 42
  > EOF
  $ cat > dep_module.mli <<EOF
  > val x : int
  > EOF
  $ cat > spurious_rebuild.ml <<EOF
  > let x = 12
  > EOF

  $ dune build @check

Edit [dep_module]'s interface. [spurious_rebuild] does not
reference [dep_module]. Record the number of [spurious_rebuild]
rebuild targets observed in the trace:

  $ cat > dep_module.mli <<EOF
  > val x : int
  > val y : string
  > EOF
  $ cat > dep_module.ml <<EOF
  > let x = 42
  > let y = "hello"
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("spurious_rebuild"))] | length'
  1
