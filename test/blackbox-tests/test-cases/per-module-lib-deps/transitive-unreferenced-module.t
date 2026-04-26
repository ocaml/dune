Per-module deps propagate across library boundaries: a module in
a transitively-reached library is a dep only if some referenced
entry module's ocamldep output names it. Editing an unreferenced
module in such a library does not invalidate consumers.

Scenario: executable `main` references only `ModB` (from `libB`).
`libB` references only `ModA2` (from `libA`). `libA` is in `main`'s
compilation context only via transitive closure through `libB`.
Starting from `main`'s own ocamldep reads (`{ModB}`), the cross-lib
walk reads `libB/modB`'s ocamldep (names `ModA2`) and then
`libA/modA2`'s ocamldep (no new names). The tight set is
`{ModB, ModA2}`; `modA1` never enters it, so `main` depends only on
`libB/modB.cmi` and `libA/modA2.cmi`, not a glob over `libA`'s
objdir.

Editing `modA1.mli` (which neither `main` nor `modB` references)
leaves `main`'s deps unchanged, so `main` is not rebuilt.

See: https://github.com/ocaml/dune/pull/14116#issuecomment-4310263512

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ cat > dune <<EOF
  > (library (name libA) (wrapped false) (modules modA1 modA2))
  > (library (name libB) (wrapped false) (modules modB) (libraries libA))
  > (executable (name main) (modules main) (libraries libB))
  > EOF

  $ cat > modA1.ml <<EOF
  > let x = 42
  > EOF
  $ cat > modA1.mli <<EOF
  > val x : int
  > EOF
  $ cat > modA2.ml <<EOF
  > let x = 43
  > EOF
  $ cat > modB.ml <<EOF
  > let x = ModA2.x
  > EOF
  $ cat > main.ml <<EOF
  > let _ = ModB.x
  > EOF

  $ dune build @check

Edit modA1's interface. Neither `main` nor `modB` references `modA1`,
so `main` should not rebuild:

  $ cat > modA1.mli <<EOF
  > val x : int
  > val y : string
  > EOF
  $ cat > modA1.ml <<EOF
  > let x = 42
  > let y = "hi"
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("dune__exe__Main"))] | length'
  0
