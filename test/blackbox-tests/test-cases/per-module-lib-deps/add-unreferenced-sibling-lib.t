Observational baseline: adding a library to a stanza's
[(libraries ...)] list currently rebuilds every module in the
stanza, including modules that reference nothing from the new
library. The [-I]/[-H] flags on each module's compiler invocation
are derived from the cctx-wide library list rather than from the
module's own ocamldep reference set, so adding an unreferenced
library still changes every consumer's compile-action hash and
invalidates the rule cache.

[libB] starts with [(libraries libA)] and two modules: [modB1]
references [modA]; [modB2] references nothing from [libA] or any
other library. After the initial build, [libC] is added to the
stanza's [(libraries ...)] list. Neither [modB1] nor [modB2] uses
anything from [libC], yet both rebuild. Records the rebuild counts
on each so a future change can flip them to 0.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ cat > dune <<EOF
  > (library (name libA) (wrapped false) (modules modA))
  > (library (name libC) (wrapped false) (modules modC))
  > (library (name libB) (wrapped false) (modules modB1 modB2) (libraries libA))
  > EOF

  $ cat > modA.ml <<EOF
  > let x = 42
  > EOF
  $ cat > modA.mli <<EOF
  > val x : int
  > EOF
  $ cat > modC.ml <<EOF
  > let y = "C"
  > EOF
  $ cat > modC.mli <<EOF
  > val y : string
  > EOF
  $ cat > modB1.ml <<EOF
  > let x = ModA.x
  > EOF
  $ cat > modB2.ml <<EOF
  > let x = 12
  > EOF

  $ dune build @check

Add [libC] to [libB]'s [(libraries ...)]. Neither [modB1] nor [modB2]
references [libC]:

  $ cat > dune <<EOF
  > (library (name libA) (wrapped false) (modules modA))
  > (library (name libC) (wrapped false) (modules modC))
  > (library (name libB) (wrapped false) (modules modB1 modB2) (libraries libA libC))
  > EOF

  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("modB1"))] | length'
  1
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("modB2"))] | length'
  1
