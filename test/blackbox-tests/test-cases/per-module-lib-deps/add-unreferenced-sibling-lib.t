Adding a library to a stanza's [(libraries ...)] list does not
rebuild modules that reference nothing from the new library. Each
module's [-I]/[-H] flags are derived from its own ocamldep
reference set rather than from the cctx-wide library list, so an
unreferenced sibling addition leaves consumer compile actions
byte-identical and the rule cache holds.

[libB] starts with [(libraries libA)] and two modules: [modB1]
references [modA]; [modB2] references nothing from [libA] or any
other library. After the initial build, [libC] is added to the
stanza's [(libraries ...)] list. Neither [modB1] nor [modB2] uses
anything from [libC]; their per-module include flags stay narrow,
so neither's compile action hash changes and neither rebuilds.

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
  0
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("modB2"))] | length'
  0
