Baseline: multi-module consumer library with a sibling module that
references no module of its declared dependency library.

This is an observational test. It records the number of rebuild
targets for consumer module [modB1] when a module of the dependency
library [libA] has its interface edited.

[libB] is an unwrapped library with two modules. [modB2] references
[ModA2]; [modB1] references nothing from [libA]. On current main,
editing [modA2]'s interface rebuilds [modB1] even though [modB1]
references nothing from [libA]: the consumer depends on a glob over
[libA]'s object directory, which is invalidated by the cmi change.

The zero-reference-sibling-in-a-library corner is distinct from
scenarios covered by existing tests: [lib-to-lib-unwrapped.t] probes
siblings that reference a different (non-edited) module of the dep;
[transitive.t] and [unwrapped.t] probe zero-reference modules but
within executable stanzas, not library stanzas. A future fix
implementing library-level dep filtering at consumer-module
granularity would drop [libA] entirely from [modB1]'s deps, tightening
this corner to zero rebuilds.

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
  1
