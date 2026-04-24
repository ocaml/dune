Known limitation: libraries reached only through `Lib.closure` fall
back to a conservative glob over their object directory. Editing an
unreferenced module in a transitively-reached library therefore still
rebuilds consumers even when they do not use the edited module.

Scenario: executable `main` references only `ModB` (from `libB`).
`libB` references only `ModA2` (from `libA`). `libA` is in `main`'s
compilation context only via transitive closure through `libB`. The
per-module filter applied at `main`'s rule computes
`direct_libs = [libB]` and `all_libs = Lib.closure [libB] = [libB; libA]`;
`libA` is not in `main`'s direct set, so it falls into the glob path
and `main` depends on a glob over `libA`'s objdir.

Editing `modA1.mli` (a module `main` does not reference and which
`modB` does not reference) invalidates the glob, and `main` rebuilds.
This test records the current rebuild count. Recovering the case
requires propagating each module's tight-dep subset through the
library dep graph instead of falling back to glob on transitively
reached libraries.

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

Edit modA1's interface. Neither `main` nor `modB` references `modA1`.
Record `main`'s rebuild-target count:

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
  2
