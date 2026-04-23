Per-module library filtering for a single-module consumer that does not
reference its dependency library.

When a consumer library has one module and it does not reference any
module from a dependency library, changing modules in that dependency
must not trigger recompilation of the consumer.

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
  $ cat > modB.ml <<EOF
  > let x = 12
  > EOF

  $ dune build @check

Modify modA only; modB does not reference modA, so modB must not be
recompiled:

  $ echo "let x = 43" > modA.ml
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("modB"))] | length'
  0
