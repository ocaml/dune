Baseline: single-module consumer library whose only module references
no module of its declared dependency library.

This is an observational test. It records the number of rebuild
targets for the consumer's [spurious_rebuild] module when the
dependency library [dep_lib]'s only module has its interface
edited.

[consumer_lib] declares [(libraries dep_lib)] but
[spurious_rebuild.ml] does not reference any module of [dep_lib].
The per-module filter drops [dep_lib] from [spurious_rebuild]'s
compile-rule deps for this single-module consumer, so editing
[dep_module]'s interface fires no [spurious_rebuild]-named rule.

The zero-reference case is a distinct corner from the single-module
consumer that references some (but not all) modules of its dep,
which [single-module-lib.t] already documents. Pre-#14116 this
overrebuilt because dune skipped ocamldep for single-module
stanzas as an optimisation and so fell back to a glob over
[dep_lib]'s objdir, which the cmi change invalidated.

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
  0
