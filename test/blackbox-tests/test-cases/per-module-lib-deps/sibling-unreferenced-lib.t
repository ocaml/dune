Per-module library filtering within a single consumer library.

When a consumer library has multiple modules, only some of which reference a
given dependency library, changes to that dependency library must not trigger
recompilation of consumer modules that do not reference it — even when their
siblings do.

This distinguishes the filter's granularity from the two-library case in
multiple-libraries.t: the filter must operate at the consumer-module level,
not merely at the consumer-library level.

See: https://github.com/ocaml/dune/issues/4572
See: https://github.com/ocaml/dune/pull/14116#issuecomment-4301275263

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
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
  $ cat > modB1.ml <<EOF
  > let x = 12
  > EOF
  $ cat > modB2.ml <<EOF
  > let x = ModA2.x
  > EOF

  $ dune build @check

modB1 references nothing in libA. Even when libA's ModA2 (used by sibling
modB2) changes, modB1 must not be recompiled:

  $ echo "let x = 44" > modA2.ml
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("modB1"))] | length'
  0
