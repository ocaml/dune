Baseline: single-module consumer library whose only module references
no module of its declared dependency library.

This is an observational test. It records the number of rebuild
targets for the consumer's single module [modB] when a module of the
dependency library [libA] has its interface edited.

[libB] declares [(libraries libA)] but [modB.ml] does not reference
any module of [libA]. On current main this scenario still rebuilds
[modB]: libB is a single-module stanza, so dune skips ocamldep as an
optimisation and cannot discover that [modB] references no module of
[libA]; the consumer falls back to a glob over [libA]'s object
directory, which is invalidated by the cmi change.

The zero-reference case is a distinct corner from the single-module
consumer that references some (but not all) modules of its dep,
which [single-module-lib.t] already documents. A future fix that
detects "ocamldep yields no references to libA" could tighten this
corner to zero rebuilds without needing to solve the broader
single-module-consumer skip-ocamldep limitation.

See: https://github.com/ocaml/dune/issues/4572
See: https://github.com/ocaml/dune/pull/14116#issuecomment-4286949811

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ cat > dune <<EOF
  > (library (name libA) (wrapped false) (modules modA))
  > (library (name libB) (wrapped false) (modules modB) (libraries libA))
  > EOF

  $ cat > modA.ml <<EOF
  > let x = 42
  > EOF
  $ cat > modA.mli <<EOF
  > val x : int
  > EOF
  $ cat > modB.ml <<EOF
  > let x = 12
  > EOF

  $ dune build @check

Edit modA's interface. modB does not reference modA. Record the
number of modB rebuild targets observed in the trace:

  $ cat > modA.mli <<EOF
  > val x : int
  > val y : string
  > EOF
  $ cat > modA.ml <<EOF
  > let x = 42
  > let y = "hello"
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("modB"))] | length'
  1
