Single-module consumer library whose only module references no module
of its declared dependency library. Under per-module library dep
filtering, the consumer is not rebuilt when the dependency's
interface changes.

[libB] declares [(libraries libA)] but [modB.ml] does not reference
any module of [libA]. Ocamldep runs on [modB] (single-module stanzas
with library deps are no longer short-circuited), reports no
references to [libA], and the filter drops [libA] from [modB]'s deps
entirely. Editing [modA]'s interface no longer invalidates anything
in [modB]'s dep set, so [modB] stays built.

See: https://github.com/ocaml/dune/issues/4572
See: https://github.com/ocaml/dune/pull/14116#issuecomment-4286949811

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
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
  0
