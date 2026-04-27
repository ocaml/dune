Multi-module consumer library with a sibling module that references
no module of its declared dependency library. Under per-module
library dep filtering, the unreferenced sibling is not rebuilt when
the dependency's interface changes.

[libB] is an unwrapped library with two modules. [modB2] references
[ModA2]; [modB1] references nothing from [libA]. Ocamldep's output
for [modB1] is empty of references to [libA], so the filter drops
[libA] entirely from [modB1]'s deps. Editing [modA2]'s interface
does not invalidate anything in [modB1]'s dep set, so [modB1] stays
built.

See: https://github.com/ocaml/dune/issues/4572
See: https://github.com/ocaml/dune/pull/14116#issuecomment-4301275263

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ cat > dune <<EOF
  > (library (name libA) (wrapped false) (modules modA1 modA2))
  > (library (name libB) (wrapped false) (modules modB1 modB2) (libraries libA))
  > EOF

  $ cat > modA1.ml <<EOF
  > let x = 42
  > EOF
  $ cat > modA2.ml <<EOF
  > let x = 43
  > EOF
  $ cat > modA2.mli <<EOF
  > val x : int
  > EOF
  $ cat > modB1.ml <<EOF
  > let x = 12
  > EOF
  $ cat > modB2.ml <<EOF
  > let x = ModA2.x
  > EOF

  $ dune build @check

Edit modA2's interface. modB1 references nothing in libA. Record the
number of modB1 rebuild targets observed in the trace:

  $ cat > modA2.mli <<EOF
  > val x : int
  > val y : string
  > EOF
  $ cat > modA2.ml <<EOF
  > let x = 43
  > let y = "hello"
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("modB1"))] | length'
  0
